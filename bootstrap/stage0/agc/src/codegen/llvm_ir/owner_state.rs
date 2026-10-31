use std::sync::Arc;

use inkwell::AddressSpace;
use inkwell::IntPredicate;
use inkwell::types::StructType;
use inkwell::values::{BasicValueEnum, PointerValue};

use super::{DeferAction, DeferredEntry, LlvmIrGenerator};
use crate::codegen::{CodegenError, CodegenResult};
use crate::parser::ast;
use crate::semantic::init_plan::{LayoutProjection, OwnerLocalId, OwnerLocalPlan};

const UNINIT: u64 = 0;
const PARTIAL: u64 = 1;
const INIT: u64 = 2;

#[derive(Clone)]
pub(crate) struct RuntimeOwner<'ctx> {
    plan: Arc<OwnerLocalPlan>,
    nodes: Vec<PointerValue<'ctx>>,
    data: Vec<PointerValue<'ctx>>,
}

impl<'ctx> LlvmIrGenerator<'ctx> {
    pub(crate) fn owner_node_for_storage(&self, data: PointerValue<'ctx>) -> Option<PointerValue<'ctx>> {
        self.owner_states.get(&data).map(|owner| owner.nodes[0])
            .or_else(|| self.borrowed_binding_states.get(&data).copied())
    }

    pub(crate) fn owner_node_for_expression(
        &self,
        expression: &ast::Expression,
    ) -> CodegenResult<Option<PointerValue<'ctx>>> {
        Ok(self.owner_place(expression)?.map(|(owner, node)| owner.nodes[node]))
    }

    pub(crate) fn owner_node_type(&self) -> StructType<'ctx> {
        let pointer = self.context.ptr_type(AddressSpace::default());
        self.context.struct_type(
            &[
                pointer.into(),
                pointer.into(),
                self.context.i32_type().into(),
                self.context.i8_type().into(),
            ],
            false,
        )
    }

    fn owner_state_ptr(
        &self,
        owner: &RuntimeOwner<'ctx>,
        node: usize,
    ) -> CodegenResult<PointerValue<'ctx>> {
        self.builder
            .build_struct_gep(
                self.owner_node_type(),
                owner.nodes[node],
                3,
                "owner.state.ptr",
            )
            .map_err(|e| CodegenError::new(format!("owner state projection: {e}")))
    }

    pub(crate) fn register_owner_state(
        &mut self,
        identifier: &ast::Identifier,
        ty: &ast::Type,
        data: PointerValue<'ctx>,
        initialized: bool,
    ) -> CodegenResult<bool> {
        if !self.type_needs_drop_glue(ty, &mut Vec::new())? {
            return Ok(false);
        }
        let function_id = self
            .owner_function
            .as_ref()
            .ok_or_else(|| CodegenError::new("owner declaration outside a function"))?;
        let id = OwnerLocalId {
            declaration: identifier.span.into(),
            name: identifier.name.clone(),
        };
        let plan = self
            .ownership
            .owner_layout(function_id, id, ty, initialized)
            .map_err(|e| {
                CodegenError::with_span(
                    crate::diagnostics::messages::owner_init_plan_error(&format!("{e:?}")),
                    identifier.span,
                )
            })?;
        plan.validate().map_err(|e| {
            CodegenError::new(crate::diagnostics::messages::owner_init_plan_error(
                &format!("{e:?}"),
            ))
        })?;
        let node_type = self.owner_node_type();
        let storage_type = node_type.array_type(plan.nodes.len() as u32);
        let function = self
            .current_fn
            .ok_or_else(|| CodegenError::new("owner storage outside a function"))?;
        let storage = self.create_entry_alloca(
            function,
            &format!("{}.owner", identifier.name),
            storage_type.into(),
        )?;
        let mut nodes = Vec::with_capacity(plan.nodes.len());
        let zero = self.context.i32_type().const_zero();
        for i in 0..plan.nodes.len() {
            let pointer = unsafe {
                self.builder.build_in_bounds_gep(
                    storage_type,
                    storage,
                    &[zero, self.context.i32_type().const_int(i as u64, false)],
                    "owner.node",
                )
            }
            .map_err(|e| CodegenError::new(format!("owner node address: {e}")))?;
            nodes.push(pointer);
        }
        let values = self.owner_value_addresses(&plan, data)?;
        // These stores execute at the declaration, including every loop entry.
        for (i, node) in plan.nodes.iter().enumerate() {
            let null = self.context.ptr_type(AddressSpace::default()).const_null();
            let parent = node.parent.map(|p| nodes[p]).unwrap_or(null);
            let first = if node.child_count == 0 {
                null
            } else {
                nodes[node.first_child]
            };
            let initial = match node.initial_state {
                crate::semantic::move_path::InitState::Initialized => INIT,
                crate::semantic::move_path::InitState::Uninitialized => UNINIT,
                crate::semantic::move_path::InitState::PartiallyInitialized => PARTIAL,
            };
            let fields: [BasicValueEnum<'ctx>; 4] = [
                parent.into(),
                first.into(),
                self.context.i32_type().const_int(node.child_count as u64, false).into(),
                self.context.i8_type().const_int(initial, false).into(),
            ];
            for (field, value) in fields.into_iter().enumerate() {
                let pointer = self.builder.build_struct_gep(node_type, nodes[i], field as u32, "owner.descriptor")
                    .map_err(|e| CodegenError::new(format!("owner descriptor address: {e}")))?;
                self.builder.build_store(pointer, value)
                    .map_err(|e| CodegenError::new(format!("owner descriptor store: {e}")))?;
            }
        }
        let owner = RuntimeOwner { plan: Arc::new(plan), nodes, data: values };
        self.owner_states.insert(data, owner.clone());
        self.defers.last_mut().expect("active owner scope").push(DeferredEntry {
            action: DeferAction::OwnerCleanup(owner), flag: None,
        });
        Ok(true)
    }

    fn owner_value_addresses(
        &mut self,
        plan: &OwnerLocalPlan,
        data: PointerValue<'ctx>,
    ) -> CodegenResult<Vec<PointerValue<'ctx>>> {
        let zero = self.context.i32_type().const_zero();
        let mut values = vec![data];
        for node in plan.nodes.iter().skip(1) {
            let parent = node.parent.expect("validated nonroot node");
            let parent_type = self.lower_basic_type(&plan.nodes[parent].ty)?;
            let pointer = match node
                .projection
                .as_ref()
                .expect("resolved nonroot projection")
            {
                LayoutProjection::Field(index) => self.builder.build_struct_gep(
                    parent_type.into_struct_type(),
                    values[parent],
                    *index,
                    "owner.field",
                ),
                LayoutProjection::Element(index) => unsafe {
                    self.builder.build_in_bounds_gep(
                        parent_type,
                        values[parent],
                        &[
                            zero,
                            self.context.i32_type().const_int(*index as u64, false),
                        ],
                        "owner.element",
                    )
                },
            }
            .map_err(|e| CodegenError::new(format!("owner value projection: {e}")))?;
            values.push(pointer);
        }
        Ok(values)
    }

    fn borrowed_owner(
        &mut self,
        data: PointerValue<'ctx>,
        ty: &ast::Type,
        state: PointerValue<'ctx>,
        span: crate::lexer::Span,
    ) -> CodegenResult<RuntimeOwner<'ctx>> {
        let function = self.owner_function.as_ref()
            .ok_or_else(|| CodegenError::new("borrowed place outside a function"))?;
        let plan = self.ownership.owner_layout(function, OwnerLocalId {
            declaration: span.into(), name: "<borrowed>".into(),
        }, ty, false).map_err(|e| CodegenError::with_span(
            crate::diagnostics::messages::owner_init_plan_error(&format!("{e:?}")), span,
        ))?;
        plan.validate().map_err(|e| CodegenError::with_span(
            crate::diagnostics::messages::owner_init_plan_error(&format!("{e:?}")), span,
        ))?;
        let mut nodes = vec![state];
        for node in plan.nodes.iter().skip(1) {
            let parent = node.parent.expect("validated borrowed node");
            let offset = nodes.len() - plan.nodes[parent].first_child;
            nodes.push(self.tracked_reference_child(nodes[parent],
                self.context.i32_type().const_int(offset as u64, false))?);
        }
        let values = self.owner_value_addresses(&plan, data)?;
        Ok(RuntimeOwner { plan: Arc::new(plan), nodes, data: values })
    }

    pub(crate) fn emit_borrowed_cleanup(
        &mut self,
        data: PointerValue<'ctx>,
        ty: &ast::Type,
        state: PointerValue<'ctx>,
        span: crate::lexer::Span,
    ) -> CodegenResult<()> {
        if !self.type_needs_drop_glue(ty, &mut Vec::new())? { return Ok(()); }
        if state == self.initialized_reference_state() {
            return self.emit_drop_glue_at_pointer(ty, data);
        }
        let function = self.current_fn.ok_or_else(|| CodegenError::new("borrowed cleanup outside a function"))?;
        let foreign = self.context.append_basic_block(function, "borrowed.initialized.drop");
        let tracked = self.context.append_basic_block(function, "borrowed.tracked.drop");
        let done = self.context.append_basic_block(function, "borrowed.drop.done");
        let initialized = self.builder.build_int_compare(IntPredicate::EQ, state,
            self.initialized_reference_state(), "borrowed.initialized")
            .map_err(|e| CodegenError::new(e.to_string()))?;
        self.builder.build_conditional_branch(initialized, foreign, tracked)
            .map_err(|e| CodegenError::new(e.to_string()))?;
        self.builder.position_at_end(foreign);
        self.emit_drop_glue_at_pointer(ty, data)?;
        self.builder.build_unconditional_branch(done)
            .map_err(|e| CodegenError::new(e.to_string()))?;
        self.builder.position_at_end(tracked);
        let owner = self.borrowed_owner(data, ty, state, span)?;
        self.emit_owner_cleanup(&owner, 0)?;
        self.transition_owner(&owner, 0, false)?;
        self.recompute_borrowed_ancestors(state)?;
        self.builder.build_unconditional_branch(done)
            .map_err(|e| CodegenError::new(e.to_string()))?;
        self.builder.position_at_end(done);
        Ok(())
    }

    pub(crate) fn initialize_borrowed_place(
        &mut self,
        data: PointerValue<'ctx>,
        ty: &ast::Type,
        state: PointerValue<'ctx>,
        span: crate::lexer::Span,
    ) -> CodegenResult<()> {
        if state == self.initialized_reference_state() { return Ok(()); }
        let function = self.current_fn.ok_or_else(|| CodegenError::new("borrowed initialization outside a function"))?;
        let tracked = self.context.append_basic_block(function, "borrowed.initialize");
        let done = self.context.append_basic_block(function, "borrowed.initialized");
        let foreign = self.builder.build_int_compare(IntPredicate::EQ, state,
            self.initialized_reference_state(), "borrowed.external")
            .map_err(|e| CodegenError::new(e.to_string()))?;
        self.builder.build_conditional_branch(foreign, done, tracked)
            .map_err(|e| CodegenError::new(e.to_string()))?;
        self.builder.position_at_end(tracked);
        let owner = self.borrowed_owner(data, ty, state, span)?;
        self.transition_owner(&owner, 0, true)?;
        self.recompute_borrowed_ancestors(state)?;
        self.builder.build_unconditional_branch(done)
            .map_err(|e| CodegenError::new(e.to_string()))?;
        self.builder.position_at_end(done);
        Ok(())
    }

    pub(crate) fn move_borrowed_place(
        &mut self,
        data: PointerValue<'ctx>,
        ty: &ast::Type,
        state: PointerValue<'ctx>,
        span: crate::lexer::Span,
    ) -> CodegenResult<()> {
        if !self.type_needs_drop_glue(ty, &mut Vec::new())? { return Ok(()); }
        if state == self.initialized_reference_state() {
            return Err(CodegenError::with_span(
                crate::diagnostics::messages::reference_move_requires_owner_state(), span,
            ));
        }
        let function = self.current_fn.ok_or_else(|| CodegenError::new("borrowed move outside a function"))?;
        let invalid = self.context.append_basic_block(function, "borrowed.move.untracked");
        let tracked = self.context.append_basic_block(function, "borrowed.move.tracked");
        let foreign = self.builder.build_int_compare(IntPredicate::EQ, state,
            self.initialized_reference_state(), "borrowed.move.external")
            .map_err(|e| CodegenError::new(e.to_string()))?;
        self.builder.build_conditional_branch(foreign, invalid, tracked)
            .map_err(|e| CodegenError::new(e.to_string()))?;
        self.builder.position_at_end(invalid);
        // A thin foreign/container reference cannot leave its pointee partial.
        let trap = self.module.get_function("llvm.trap").unwrap_or_else(|| {
            self.module.add_function("llvm.trap", self.context.void_type().fn_type(&[], false), None)
        });
        self.builder.build_call(trap, &[], "")
            .map_err(|e| CodegenError::new(e.to_string()))?;
        self.builder.build_unreachable().map_err(|e| CodegenError::new(e.to_string()))?;
        self.builder.position_at_end(tracked);
        let owner = self.borrowed_owner(data, ty, state, span)?;
        self.transition_owner(&owner, 0, false)?;
        self.recompute_borrowed_ancestors(state)
    }

    fn recompute_borrowed_ancestors(&self, node: PointerValue<'ctx>) -> CodegenResult<()> {
        let pointer_type = self.context.ptr_type(AddressSpace::default());
        let i32_type = self.context.i32_type();
        let i8_type = self.context.i8_type();
        let function = self.current_fn.ok_or_else(|| CodegenError::new("borrowed transition outside a function"))?;
        let before = self.builder.get_insert_block().ok_or_else(|| CodegenError::new("missing transition block"))?;
        let parent_slot = self.builder.build_struct_gep(self.owner_node_type(), node, 0, "borrowed.parent.slot")
            .map_err(|e| CodegenError::new(e.to_string()))?;
        let first_parent = self.builder.build_load(pointer_type, parent_slot, "borrowed.parent")
            .map_err(|e| CodegenError::new(e.to_string()))?.into_pointer_value();
        let parents = self.context.append_basic_block(function, "borrowed.ancestors");
        let children = self.context.append_basic_block(function, "borrowed.siblings");
        let inspect = self.context.append_basic_block(function, "borrowed.sibling");
        let update = self.context.append_basic_block(function, "borrowed.parent.update");
        let done = self.context.append_basic_block(function, "borrowed.ancestors.done");
        self.builder.build_unconditional_branch(parents).map_err(|e| CodegenError::new(e.to_string()))?;
        self.builder.position_at_end(parents);
        let parent_phi = self.builder.build_phi(pointer_type, "borrowed.ancestor")
            .map_err(|e| CodegenError::new(e.to_string()))?;
        let parent = parent_phi.as_basic_value().into_pointer_value();
        let present = self.builder.build_is_not_null(parent, "borrowed.has.parent")
            .map_err(|e| CodegenError::new(e.to_string()))?;
        self.builder.build_conditional_branch(present, children, done).map_err(|e| CodegenError::new(e.to_string()))?;
        self.builder.position_at_end(children);
        let index = self.builder.build_phi(i32_type, "borrowed.sibling.index").map_err(|e| CodegenError::new(e.to_string()))?;
        let all = self.builder.build_phi(self.context.bool_type(), "borrowed.all").map_err(|e| CodegenError::new(e.to_string()))?;
        let any = self.builder.build_phi(self.context.bool_type(), "borrowed.any").map_err(|e| CodegenError::new(e.to_string()))?;
        let count_ptr = self.builder.build_struct_gep(self.owner_node_type(), parent, 2, "borrowed.count.ptr")
            .map_err(|e| CodegenError::new(e.to_string()))?;
        let count = self.builder.build_load(i32_type, count_ptr, "borrowed.count")
            .map_err(|e| CodegenError::new(e.to_string()))?.into_int_value();
        let more = self.builder.build_int_compare(IntPredicate::ULT, index.as_basic_value().into_int_value(), count, "borrowed.more")
            .map_err(|e| CodegenError::new(e.to_string()))?;
        self.builder.build_conditional_branch(more, inspect, update).map_err(|e| CodegenError::new(e.to_string()))?;
        self.builder.position_at_end(inspect);
        let child = self.tracked_reference_child(parent, index.as_basic_value().into_int_value())?;
        let state_ptr = self.builder.build_struct_gep(self.owner_node_type(), child, 3, "borrowed.child.state.ptr")
            .map_err(|e| CodegenError::new(e.to_string()))?;
        let state = self.builder.build_load(i8_type, state_ptr, "borrowed.child.state")
            .map_err(|e| CodegenError::new(e.to_string()))?.into_int_value();
        let live = self.builder.build_int_compare(IntPredicate::EQ, state, i8_type.const_int(INIT, false), "borrowed.child.live")
            .map_err(|e| CodegenError::new(e.to_string()))?;
        let exists = self.builder.build_int_compare(IntPredicate::NE, state, i8_type.const_zero(), "borrowed.child.exists")
            .map_err(|e| CodegenError::new(e.to_string()))?;
        let next_all = self.builder.build_and(all.as_basic_value().into_int_value(), live, "borrowed.next.all")
            .map_err(|e| CodegenError::new(e.to_string()))?;
        let next_any = self.builder.build_or(any.as_basic_value().into_int_value(), exists, "borrowed.next.any")
            .map_err(|e| CodegenError::new(e.to_string()))?;
        let next_index = self.builder.build_int_add(index.as_basic_value().into_int_value(), i32_type.const_int(1, false), "borrowed.next.index")
            .map_err(|e| CodegenError::new(e.to_string()))?;
        self.builder.build_unconditional_branch(children).map_err(|e| CodegenError::new(e.to_string()))?;
        index.add_incoming(&[(&i32_type.const_zero(), parents), (&next_index, inspect)]);
        all.add_incoming(&[(&self.context.bool_type().const_int(1, false), parents), (&next_all, inspect)]);
        any.add_incoming(&[(&self.context.bool_type().const_zero(), parents), (&next_any, inspect)]);
        self.builder.position_at_end(update);
        let partial = self.builder.build_select(any.as_basic_value().into_int_value(),
            i8_type.const_int(PARTIAL, false), i8_type.const_zero(), "borrowed.partial")
            .map_err(|e| CodegenError::new(e.to_string()))?;
        let state = self.builder.build_select(all.as_basic_value().into_int_value(),
            i8_type.const_int(INIT, false), partial.into_int_value(), "borrowed.summary")
            .map_err(|e| CodegenError::new(e.to_string()))?;
        let state_ptr = self.builder.build_struct_gep(self.owner_node_type(), parent, 3, "borrowed.parent.state")
            .map_err(|e| CodegenError::new(e.to_string()))?;
        self.builder.build_store(state_ptr, state).map_err(|e| CodegenError::new(e.to_string()))?;
        let parent_ptr = self.builder.build_struct_gep(self.owner_node_type(), parent, 0, "borrowed.next.parent.ptr")
            .map_err(|e| CodegenError::new(e.to_string()))?;
        let next_parent = self.builder.build_load(pointer_type, parent_ptr, "borrowed.next.parent")
            .map_err(|e| CodegenError::new(e.to_string()))?.into_pointer_value();
        self.builder.build_unconditional_branch(parents).map_err(|e| CodegenError::new(e.to_string()))?;
        parent_phi.add_incoming(&[(&first_parent, before), (&next_parent, update)]);
        self.builder.position_at_end(done);
        Ok(())
    }

    pub(crate) fn owner_place(
        &self,
        expression: &ast::Expression,
    ) -> CodegenResult<Option<(RuntimeOwner<'ctx>, usize)>> {
        let Some(place) = self
            .owner_function
            .as_ref()
            .and_then(|id| self.ownership.functions.get(id))
            .and_then(|f| f.places.get(&expression.span.into()))
        else {
            return Ok(None);
        };
        let Some(variable) = self.lookup_variable(&place.owner.name) else {
            return Ok(None);
        };
        let Some(owner) = self.owner_states.get(&variable.ptr) else {
            return Ok(None);
        };
        if owner.plan.id != place.owner {
            return Err(CodegenError::with_span(
                "owner declaration identity mismatch",
                expression.span,
            ));
        }
        Ok(Some((owner.clone(), place.node)))
    }

    pub(crate) fn transition_owner_expr(
        &mut self,
        expression: &ast::Expression,
        initialized: bool,
    ) -> CodegenResult<bool> {
        let Some((owner, node)) = self.owner_place(expression)? else {
            return Ok(false);
        };
        self.transition_owner(&owner, node, initialized)?;
        Ok(true)
    }

    pub(crate) fn transfer_owner_value(
        &mut self,
        expression: &ast::Expression,
    ) -> CodegenResult<()> {
        let transfer = self
            .owner_function
            .as_ref()
            .and_then(|id| self.ownership.functions.get(id))
            .is_some_and(|f| {
                f.transitions.iter().any(|t| {
                    t.site == expression.span.into()
                        && t.state == crate::semantic::move_path::InitState::Uninitialized
                })
            });
        if transfer {
            self.transition_owner_expr(expression, false)?;
        }
        Ok(())
    }

    pub(crate) fn transition_owner_path(
        &mut self,
        name: &str,
        path: &str,
        initialized: bool,
    ) -> CodegenResult<bool> {
        let Some(var) = self.lookup_variable(name) else {
            return Ok(false);
        };
        let Some(owner) = self.owner_states.get(&var.ptr).cloned() else {
            return Ok(false);
        };
        let node = owner.plan.node_for_path(path).map_err(|e| {
            CodegenError::new(crate::diagnostics::messages::owner_init_plan_error(
                &format!("{e:?}"),
            ))
        })?;
        let Some(node) = node else {
            return Ok(false);
        };
        self.transition_owner(&owner, node, initialized)?;
        Ok(true)
    }

    fn transition_owner(
        &mut self,
        owner: &RuntimeOwner<'ctx>,
        node: usize,
        initialized: bool,
    ) -> CodegenResult<()> {
        let target = &owner.plan.nodes[node].place;
        for (i, child) in owner.plan.nodes.iter().enumerate() {
            if target.is_prefix_of(&child.place) {
                self.builder
                    .build_store(
                        self.owner_state_ptr(owner, i)?,
                        self.context
                            .i8_type()
                            .const_int(if initialized { INIT } else { UNINIT }, false),
                    )
                    .map_err(|e| CodegenError::new(format!("owner transition: {e}")))?;
            }
        }
        let mut ancestor = owner.plan.nodes[node].parent;
        while let Some(index) = ancestor {
            let plan = &owner.plan.nodes[index];
            let mut all = self.context.bool_type().const_int(1, false);
            let mut any = self.context.bool_type().const_zero();
            for child in plan.first_child..plan.first_child + plan.child_count as usize {
                let state = self
                    .builder
                    .build_load(
                        self.context.i8_type(),
                        self.owner_state_ptr(owner, child)?,
                        "owner.child.state",
                    )
                    .map_err(|e| CodegenError::new(format!("owner state load: {e}")))?
                    .into_int_value();
                let live = self
                    .builder
                    .build_int_compare(
                        IntPredicate::EQ,
                        state,
                        self.context.i8_type().const_int(INIT, false),
                        "owner.child.init",
                    )
                    .map_err(|e| CodegenError::new(e.to_string()))?;
                let present = self
                    .builder
                    .build_int_compare(
                        IntPredicate::NE,
                        state,
                        self.context.i8_type().const_zero(),
                        "owner.child.present",
                    )
                    .map_err(|e| CodegenError::new(e.to_string()))?;
                all = self
                    .builder
                    .build_and(all, live, "owner.all")
                    .map_err(|e| CodegenError::new(e.to_string()))?;
                any = self
                    .builder
                    .build_or(any, present, "owner.any")
                    .map_err(|e| CodegenError::new(e.to_string()))?;
            }
            let partial = self
                .builder
                .build_select(
                    any,
                    self.context.i8_type().const_int(PARTIAL, false),
                    self.context.i8_type().const_zero(),
                    "owner.partial",
                )
                .map_err(|e| CodegenError::new(e.to_string()))?;
            let state = self
                .builder
                .build_select(
                    all,
                    self.context.i8_type().const_int(INIT, false),
                    partial.into_int_value(),
                    "owner.summary",
                )
                .map_err(|e| CodegenError::new(e.to_string()))?;
            self.builder
                .build_store(self.owner_state_ptr(owner, index)?, state)
                .map_err(|e| CodegenError::new(e.to_string()))?;
            ancestor = plan.parent;
        }
        Ok(())
    }

    pub(crate) fn emit_owner_cleanup(
        &mut self,
        owner: &RuntimeOwner<'ctx>,
        node: usize,
    ) -> CodegenResult<()> {
        let plan = &owner.plan.nodes[node];
        if !self.type_needs_drop_glue(&plan.ty, &mut Vec::new())? {
            return Ok(());
        }
        let function = self
            .current_fn
            .ok_or_else(|| CodegenError::new("owner cleanup outside function"))?;
        let state = self
            .builder
            .build_load(
                self.context.i8_type(),
                self.owner_state_ptr(owner, node)?,
                "owner.cleanup.state",
            )
            .map_err(|e| CodegenError::new(e.to_string()))?
            .into_int_value();
        let run = self.context.append_basic_block(function, "owner.cleanup");
        let after = self.context.append_basic_block(function, "owner.after");
        let present = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                state,
                self.context.i8_type().const_zero(),
                "owner.present",
            )
            .map_err(|e| CodegenError::new(e.to_string()))?;
        self.builder
            .build_conditional_branch(present, run, after)
            .map_err(|e| CodegenError::new(e.to_string()))?;
        self.builder.position_at_end(run);
        if plan.child_count == 0 {
            self.emit_drop_glue_at_pointer(&plan.ty, owner.data[node])?;
        } else {
            if let Some(drop_name) = self.get_drop_function_name(&plan.ty)? {
                let own = self.context.append_basic_block(function, "owner.own.drop");
                let cascade = self.context.append_basic_block(function, "owner.cascade");
                let full = self
                    .builder
                    .build_int_compare(
                        IntPredicate::EQ,
                        state,
                        self.context.i8_type().const_int(INIT, false),
                        "owner.full",
                    )
                    .map_err(|e| CodegenError::new(e.to_string()))?;
                self.builder
                    .build_conditional_branch(full, own, cascade)
                    .map_err(|e| CodegenError::new(e.to_string()))?;
                self.builder.position_at_end(own);
                let drop = self
                    .module
                    .get_function(&drop_name)
                    .ok_or_else(|| CodegenError::new("resolved destructor missing"))?;
                self.builder
                    .build_call(drop, &[owner.data[node].into()], "owner.drop")
                    .map_err(|e| CodegenError::new(e.to_string()))?;
                self.builder
                    .build_unconditional_branch(cascade)
                    .map_err(|e| CodegenError::new(e.to_string()))?;
                self.builder.position_at_end(cascade);
            }
            for child in plan.first_child..plan.first_child + plan.child_count as usize {
                self.emit_owner_cleanup(owner, child)?;
            }
        }
        self.builder
            .build_store(
                self.owner_state_ptr(owner, node)?,
                self.context.i8_type().const_zero(),
            )
            .map_err(|e| CodegenError::new(e.to_string()))?;
        self.builder
            .build_unconditional_branch(after)
            .map_err(|e| CodegenError::new(e.to_string()))?;
        self.builder.position_at_end(after);
        Ok(())
    }
}

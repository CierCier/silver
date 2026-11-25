//! Runtime owner trees for locals whose initialization state is dynamic.
//!
//! Each tracked local owns an array of nodes in the semantic plan's order.
//! A node is `{ i32 parent, i32 first_child, i32 child_count, i8 state }`;
//! `parent` and `first_child` are offsets in nodes relative to the node itself
//! (0 means none), so a tree is position independent: a declaration copies a
//! constant template, and borrowed references navigate the caller's tree.
//! Transitions call one shared helper that sets the subtree and refreshes the
//! ancestors' summaries, keeping each transition O(1) in emitted code.

use std::sync::Arc;

use inkwell::AddressSpace;
use inkwell::IntPredicate;
use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::module::Linkage;
use inkwell::types::StructType;
use inkwell::values::{FunctionValue, IntValue, PointerValue};

use super::{DeferAction, DeferredEntry, LlvmIrGenerator};
use crate::codegen::{CodegenError, CodegenResult};
use crate::parser::ast;
use crate::semantic::init_plan::{LayoutProjection, OwnerLocalId, OwnerLocalPlan};
use crate::semantic::move_path::InitState;

const UNINIT: u64 = 0;
const PARTIAL: u64 = 1;
pub(crate) const INIT: u64 = 2;

const NODE_PARENT: u32 = 0;
const NODE_FIRST_CHILD: u32 = 1;
const NODE_CHILD_COUNT: u32 = 2;
const NODE_STATE: u32 = 3;

const SET_HELPER: &str = "__silver_owner_set";
const TRANSITION_HELPER: &str = "__silver_owner_transition";
const PROJECT_HELPER: &str = "__silver_owner_project";

#[derive(Clone)]
enum OwnerNodes<'ctx> {
    /// The local's own node array; node `i` is element `i`.
    Local(PointerValue<'ctx>),
    /// Nodes reached through a borrowed reference, resolved at the use site.
    Borrowed(Vec<PointerValue<'ctx>>),
}

#[derive(Clone)]
pub(crate) struct RuntimeOwner<'ctx> {
    plan: Arc<OwnerLocalPlan>,
    nodes: OwnerNodes<'ctx>,
    /// Address of the root value; node values are projected from it on use.
    data: PointerValue<'ctx>,
}

fn builder_error(error: inkwell::builder::BuilderError) -> CodegenError {
    CodegenError::new(format!("owner state: {error}"))
}

impl<'ctx> LlvmIrGenerator<'ctx> {
    pub(crate) fn owner_node_for_storage(
        &self,
        data: PointerValue<'ctx>,
    ) -> Option<PointerValue<'ctx>> {
        self.owner_states
            .get(&data)
            .map(|owner| match &owner.nodes {
                OwnerNodes::Local(storage) => *storage,
                OwnerNodes::Borrowed(nodes) => nodes[0],
            })
            .or_else(|| self.borrowed_binding_states.get(&data).copied())
    }

    pub(crate) fn owner_node_for_expression(
        &self,
        expression: &ast::Expression,
    ) -> CodegenResult<Option<PointerValue<'ctx>>> {
        self.owner_place(expression)?
            .map(|(owner, node)| self.owner_node_ptr(&owner, node))
            .transpose()
    }

    pub(crate) fn owner_node_type(&self) -> StructType<'ctx> {
        let i32_type = self.context.i32_type();
        self.context.struct_type(
            &[
                i32_type.into(),
                i32_type.into(),
                i32_type.into(),
                self.context.i8_type().into(),
            ],
            false,
        )
    }

    fn owner_node_ptr(
        &self,
        owner: &RuntimeOwner<'ctx>,
        node: usize,
    ) -> CodegenResult<PointerValue<'ctx>> {
        match &owner.nodes {
            OwnerNodes::Local(storage) if node == 0 => Ok(*storage),
            OwnerNodes::Local(storage) => unsafe {
                self.builder.build_in_bounds_gep(
                    self.owner_node_type(),
                    *storage,
                    &[self.context.i64_type().const_int(node as u64, false)],
                    "owner.node",
                )
            }
            .map_err(builder_error),
            OwnerNodes::Borrowed(nodes) => Ok(nodes[node]),
        }
    }

    fn owner_state_ptr(
        &self,
        owner: &RuntimeOwner<'ctx>,
        node: usize,
    ) -> CodegenResult<PointerValue<'ctx>> {
        self.builder
            .build_struct_gep(
                self.owner_node_type(),
                self.owner_node_ptr(owner, node)?,
                NODE_STATE,
                "owner.state.ptr",
            )
            .map_err(builder_error)
    }

    /// Address of `child`'s value given its parent's value address.
    fn owner_child_value(
        &mut self,
        plan: &OwnerLocalPlan,
        child: usize,
        parent_value: PointerValue<'ctx>,
    ) -> CodegenResult<PointerValue<'ctx>> {
        let node = &plan.nodes[child];
        let parent = node.parent.expect("validated nonroot node");
        let parent_type = self.lower_basic_type(&plan.nodes[parent].ty)?;
        match node
            .projection
            .as_ref()
            .expect("resolved nonroot projection")
        {
            LayoutProjection::Field(index) => self.builder.build_struct_gep(
                parent_type.into_struct_type(),
                parent_value,
                *index,
                "owner.field",
            ),
            LayoutProjection::Element(index) => unsafe {
                self.builder.build_in_bounds_gep(
                    parent_type,
                    parent_value,
                    &[
                        self.context.i32_type().const_zero(),
                        self.context.i32_type().const_int(*index as u64, false),
                    ],
                    "owner.element",
                )
            },
        }
        .map_err(|e| CodegenError::new(format!("owner value projection: {e}")))
    }

    fn owner_value_ptr(
        &mut self,
        owner: &RuntimeOwner<'ctx>,
        node: usize,
    ) -> CodegenResult<PointerValue<'ctx>> {
        let mut path = Vec::new();
        let mut current = node;
        while let Some(parent) = owner.plan.nodes[current].parent {
            path.push(current);
            current = parent;
        }
        let mut value = owner.data;
        for &child in path.iter().rev() {
            value = self.owner_child_value(&owner.plan, child, value)?;
        }
        Ok(value)
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
        // Runs at the declaration, so every loop entry starts from the template.
        let template = self.owner_template(&plan);
        let layout = self.module.get_data_layout();
        let target_data = inkwell::targets::TargetData::create(
            layout
                .as_str()
                .to_str()
                .map_err(|e| CodegenError::new(e.to_string()))?,
        );
        let size = target_data.get_abi_size(&storage_type);
        self.build_memcpy(storage, 4, template, 4, size)?;
        let owner = RuntimeOwner {
            plan: Arc::new(plan),
            nodes: OwnerNodes::Local(storage),
            data,
        };
        self.owner_states.insert(data, owner.clone());
        self.defers
            .last_mut()
            .expect("active owner scope")
            .push(DeferredEntry {
                action: DeferAction::OwnerCleanup(owner),
                flag: None,
            });
        Ok(true)
    }

    /// The constant initial node array for `plan`, shared by equal layouts.
    fn owner_template(&self, plan: &OwnerLocalPlan) -> PointerValue<'ctx> {
        use std::hash::{Hash, Hasher};
        let fields: Vec<[i64; 4]> = plan
            .nodes
            .iter()
            .enumerate()
            .map(|(index, node)| {
                let index = index as i64;
                [
                    node.parent.map_or(0, |parent| parent as i64 - index),
                    if node.child_count == 0 {
                        0
                    } else {
                        node.first_child as i64 - index
                    },
                    i64::from(node.child_count),
                    match node.initial_state {
                        InitState::Initialized => INIT as i64,
                        InitState::Uninitialized => UNINIT as i64,
                        InitState::PartiallyInitialized => PARTIAL as i64,
                    },
                ]
            })
            .collect();
        let mut hasher = rustc_hash::FxHasher::default();
        fields.hash(&mut hasher);
        let name = format!(
            "__silver_owner_template.{}.{:016x}",
            fields.len(),
            hasher.finish()
        );
        if let Some(global) = self.module.get_global(&name) {
            return global.as_pointer_value();
        }
        let i32_type = self.context.i32_type();
        let i8_type = self.context.i8_type();
        let node_type = self.owner_node_type();
        let nodes: Vec<_> = fields
            .iter()
            .map(|[parent, first, count, state]| {
                node_type.const_named_struct(&[
                    i32_type.const_int(*parent as u64, true).into(),
                    i32_type.const_int(*first as u64, true).into(),
                    i32_type.const_int(*count as u64, false).into(),
                    i8_type.const_int(*state as u64, false).into(),
                ])
            })
            .collect();
        let global = self
            .module
            .add_global(node_type.array_type(nodes.len() as u32), None, &name);
        global.set_initializer(&node_type.const_array(&nodes));
        global.set_constant(true);
        global.set_linkage(Linkage::Private);
        global.set_unnamed_addr(true);
        global.as_pointer_value()
    }

    fn borrowed_owner(
        &mut self,
        data: PointerValue<'ctx>,
        ty: &ast::Type,
        state: PointerValue<'ctx>,
        span: crate::lexer::Span,
    ) -> CodegenResult<RuntimeOwner<'ctx>> {
        let function = self
            .owner_function
            .as_ref()
            .ok_or_else(|| CodegenError::new("borrowed place outside a function"))?;
        let plan = self
            .ownership
            .owner_layout(
                function,
                OwnerLocalId {
                    declaration: span.into(),
                    name: "<borrowed>".into(),
                },
                ty,
                false,
            )
            .map_err(|e| {
                CodegenError::with_span(
                    crate::diagnostics::messages::owner_init_plan_error(&format!("{e:?}")),
                    span,
                )
            })?;
        plan.validate().map_err(|e| {
            CodegenError::with_span(
                crate::diagnostics::messages::owner_init_plan_error(&format!("{e:?}")),
                span,
            )
        })?;
        let mut nodes = vec![state];
        for node in plan.nodes.iter().skip(1) {
            let parent = node.parent.expect("validated borrowed node");
            let offset = nodes.len() - plan.nodes[parent].first_child;
            nodes.push(self.tracked_reference_child(
                nodes[parent],
                self.context.i64_type().const_int(offset as u64, false),
            )?);
        }
        Ok(RuntimeOwner {
            plan: Arc::new(plan),
            nodes: OwnerNodes::Borrowed(nodes),
            data,
        })
    }

    pub(crate) fn emit_borrowed_cleanup(
        &mut self,
        data: PointerValue<'ctx>,
        ty: &ast::Type,
        state: PointerValue<'ctx>,
        span: crate::lexer::Span,
    ) -> CodegenResult<()> {
        if !self.type_needs_drop_glue(ty, &mut Vec::new())? {
            return Ok(());
        }
        if state == self.initialized_reference_state() {
            return self.emit_drop_glue_at_pointer(ty, data);
        }
        let function = self
            .current_fn
            .ok_or_else(|| CodegenError::new("borrowed cleanup outside a function"))?;
        let foreign = self
            .context
            .append_basic_block(function, "borrowed.initialized.drop");
        let tracked = self
            .context
            .append_basic_block(function, "borrowed.tracked.drop");
        let done = self
            .context
            .append_basic_block(function, "borrowed.drop.done");
        let initialized = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                state,
                self.initialized_reference_state(),
                "borrowed.initialized",
            )
            .map_err(builder_error)?;
        self.builder
            .build_conditional_branch(initialized, foreign, tracked)
            .map_err(builder_error)?;
        self.builder.position_at_end(foreign);
        self.emit_drop_glue_at_pointer(ty, data)?;
        self.builder
            .build_unconditional_branch(done)
            .map_err(builder_error)?;
        self.builder.position_at_end(tracked);
        let owner = self.borrowed_owner(data, ty, state, span)?;
        self.emit_owner_cleanup(&owner, 0)?;
        self.emit_owner_transition(state, false)?;
        self.builder
            .build_unconditional_branch(done)
            .map_err(builder_error)?;
        self.builder.position_at_end(done);
        Ok(())
    }

    pub(crate) fn initialize_borrowed_place(
        &mut self,
        _data: PointerValue<'ctx>,
        _ty: &ast::Type,
        state: PointerValue<'ctx>,
        _span: crate::lexer::Span,
    ) -> CodegenResult<()> {
        if state == self.initialized_reference_state() {
            return Ok(());
        }
        let function = self
            .current_fn
            .ok_or_else(|| CodegenError::new("borrowed initialization outside a function"))?;
        let tracked = self
            .context
            .append_basic_block(function, "borrowed.initialize");
        let done = self
            .context
            .append_basic_block(function, "borrowed.initialized");
        let foreign = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                state,
                self.initialized_reference_state(),
                "borrowed.external",
            )
            .map_err(builder_error)?;
        self.builder
            .build_conditional_branch(foreign, done, tracked)
            .map_err(builder_error)?;
        self.builder.position_at_end(tracked);
        self.emit_owner_transition(state, true)?;
        self.builder
            .build_unconditional_branch(done)
            .map_err(builder_error)?;
        self.builder.position_at_end(done);
        Ok(())
    }

    pub(crate) fn move_borrowed_place(
        &mut self,
        _data: PointerValue<'ctx>,
        ty: &ast::Type,
        state: PointerValue<'ctx>,
        span: crate::lexer::Span,
    ) -> CodegenResult<()> {
        if !self.type_needs_drop_glue(ty, &mut Vec::new())? {
            return Ok(());
        }
        if state == self.initialized_reference_state() {
            return Err(CodegenError::with_span(
                crate::diagnostics::messages::reference_move_requires_owner_state(),
                span,
            ));
        }
        let function = self
            .current_fn
            .ok_or_else(|| CodegenError::new("borrowed move outside a function"))?;
        let invalid = self
            .context
            .append_basic_block(function, "borrowed.move.untracked");
        let tracked = self
            .context
            .append_basic_block(function, "borrowed.move.tracked");
        let foreign = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                state,
                self.initialized_reference_state(),
                "borrowed.move.external",
            )
            .map_err(builder_error)?;
        self.builder
            .build_conditional_branch(foreign, invalid, tracked)
            .map_err(builder_error)?;
        self.builder.position_at_end(invalid);
        // A thin foreign/container reference cannot leave its pointee partial.
        let trap = self.module.get_function("llvm.trap").unwrap_or_else(|| {
            self.module.add_function(
                "llvm.trap",
                self.context.void_type().fn_type(&[], false),
                None,
            )
        });
        self.builder
            .build_call(trap, &[], "")
            .map_err(builder_error)?;
        self.builder.build_unreachable().map_err(builder_error)?;
        self.builder.position_at_end(tracked);
        self.emit_owner_transition(state, false)
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
        self.emit_owner_transition(self.owner_node_ptr(&owner, node)?, initialized)?;
        Ok(true)
    }

    /// Releases a moved field of an rvalue's spilled temporary, such as
    /// `make().leaf`, so the temporary drops only the fields it still owns.
    /// A Copy field leaves the temporary whole.
    pub(crate) fn release_rvalue_field(
        &mut self,
        expression: &ast::Expression,
    ) -> CodegenResult<bool> {
        let mut path = Vec::new();
        let mut root = expression;
        loop {
            match root.kind.as_ref() {
                ast::ExpressionKind::FieldAccess { object, field } => {
                    path.push(field.name.clone());
                    root = object;
                }
                ast::ExpressionKind::Index { object, index } => {
                    let ast::ExpressionKind::Literal(ast::Literal::Integer(i)) =
                        index.kind.as_ref()
                    else {
                        return Ok(false);
                    };
                    path.push(i.to_string());
                    root = object;
                }
                _ => break,
            }
        }
        if path.is_empty() {
            return Ok(false);
        }
        let Some(data) = self.rvalue_temporaries.get(&root.span.into()).copied() else {
            return Ok(false);
        };
        let Some(owner) = self.owner_states.get(&data).cloned() else {
            return Ok(false);
        };
        let Some(ty) = self.resolve_argument_type(expression) else {
            return Ok(false);
        };
        if !self.type_needs_drop_glue(&ty, &mut Vec::new())? {
            return Ok(false);
        }
        path.reverse();
        let node = owner.plan.node_for_path(&path.join(".")).map_err(|e| {
            CodegenError::new(crate::diagnostics::messages::owner_init_plan_error(
                &format!("{e:?}"),
            ))
        })?;
        let Some(node) = node else {
            return Ok(false);
        };
        let mut ancestor = owner.plan.nodes[node].parent;
        while let Some(index) = ancestor {
            let ancestor_ty = &owner.plan.nodes[index].ty;
            if let ast::TypeKind::Named(named) = ancestor_ty.kind.as_ref()
                && self.get_drop_function_name(ancestor_ty)?.is_some()
            {
                return Err(CodegenError::with_span(
                    crate::diagnostics::messages::cannot_move_field_out_of_drop_type(
                        &path.join("."),
                        &Self::named_type_name(named),
                    ),
                    expression.span,
                ));
            }
            ancestor = owner.plan.nodes[index].parent;
        }
        self.emit_owner_transition(self.owner_node_ptr(&owner, node)?, false)?;
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
                    t.site == expression.span.into() && t.state == InitState::Uninitialized
                })
            });
        if transfer {
            self.transition_owner_expr(expression, false)?;
        } else {
            self.release_rvalue_field(expression)?;
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
        self.emit_owner_transition(self.owner_node_ptr(&owner, node)?, initialized)?;
        Ok(true)
    }

    /// Sets `node`'s subtree to fully (un)initialized and refreshes the
    /// summaries of its ancestors, including ancestors in a caller's tree.
    fn emit_owner_transition(
        &mut self,
        node: PointerValue<'ctx>,
        initialized: bool,
    ) -> CodegenResult<()> {
        let helper = self.owner_helper(TRANSITION_HELPER)?;
        let state = self
            .context
            .i8_type()
            .const_int(if initialized { INIT } else { UNINIT }, false);
        self.builder
            .build_call(helper, &[node.into(), state.into()], "")
            .map_err(builder_error)?;
        Ok(())
    }

    /// The node for child `index` of `state`, or `state` itself when it is
    /// the initialized capability of a value without an owner tree.
    pub(crate) fn project_reference_state(
        &mut self,
        state: PointerValue<'ctx>,
        index: IntValue<'ctx>,
    ) -> CodegenResult<PointerValue<'ctx>> {
        if state == self.initialized_reference_state() {
            return Ok(state);
        }
        let helper = self.owner_helper(PROJECT_HELPER)?;
        let index = self
            .builder
            .build_int_s_extend_or_bit_cast(index, self.context.i64_type(), "owner.index")
            .map_err(builder_error)?;
        self.builder
            .build_call(helper, &[state.into(), index.into()], "owner.child")
            .map_err(builder_error)?
            .try_as_basic_value()
            .basic()
            .map(|value| value.into_pointer_value())
            .ok_or_else(|| CodegenError::new("owner projection returned void"))
    }

    pub(crate) fn emit_owner_cleanup(
        &mut self,
        owner: &RuntimeOwner<'ctx>,
        node: usize,
    ) -> CodegenResult<()> {
        let value = self.owner_value_ptr(owner, node)?;
        self.emit_owner_cleanup_at(owner, node, value)
    }

    fn emit_owner_cleanup_at(
        &mut self,
        owner: &RuntimeOwner<'ctx>,
        node: usize,
        value: PointerValue<'ctx>,
    ) -> CodegenResult<()> {
        let plan = &owner.plan.nodes[node];
        if !self.type_needs_drop_glue(&plan.ty, &mut Vec::new())? {
            return Ok(());
        }
        let function = self
            .current_fn
            .ok_or_else(|| CodegenError::new("owner cleanup outside function"))?;
        let state_ptr = self.owner_state_ptr(owner, node)?;
        let state = self
            .builder
            .build_load(self.context.i8_type(), state_ptr, "owner.cleanup.state")
            .map_err(builder_error)?
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
            .map_err(builder_error)?;
        self.builder
            .build_conditional_branch(present, run, after)
            .map_err(builder_error)?;
        self.builder.position_at_end(run);
        if plan.child_count == 0 {
            self.emit_drop_glue_at_pointer(&plan.ty, value)?;
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
                    .map_err(builder_error)?;
                self.builder
                    .build_conditional_branch(full, own, cascade)
                    .map_err(builder_error)?;
                self.builder.position_at_end(own);
                let drop = self
                    .module
                    .get_function(&drop_name)
                    .ok_or_else(|| CodegenError::new("resolved destructor missing"))?;
                self.builder
                    .build_call(drop, &[value.into()], "owner.drop")
                    .map_err(builder_error)?;
                self.builder
                    .build_unconditional_branch(cascade)
                    .map_err(builder_error)?;
                self.builder.position_at_end(cascade);
            }
            let children = plan.first_child..plan.first_child + plan.child_count as usize;
            for child in children {
                if !self.type_needs_drop_glue(&owner.plan.nodes[child].ty, &mut Vec::new())? {
                    continue;
                }
                let child_value = self.owner_child_value(&owner.plan, child, value)?;
                self.emit_owner_cleanup_at(owner, child, child_value)?;
            }
        }
        self.builder
            .build_store(state_ptr, self.context.i8_type().const_zero())
            .map_err(builder_error)?;
        self.builder
            .build_unconditional_branch(after)
            .map_err(builder_error)?;
        self.builder.position_at_end(after);
        Ok(())
    }

    /// Returns the module's owner runtime helper `name`, emitting it on first use.
    fn owner_helper(&mut self, name: &str) -> CodegenResult<FunctionValue<'ctx>> {
        if let Some(function) = self.module.get_function(name) {
            return Ok(function);
        }
        let pointer = self.context.ptr_type(AddressSpace::default());
        let i8_type = self.context.i8_type();
        let fn_type = match name {
            SET_HELPER | TRANSITION_HELPER => self
                .context
                .void_type()
                .fn_type(&[pointer.into(), i8_type.into()], false),
            PROJECT_HELPER => {
                pointer.fn_type(&[pointer.into(), self.context.i64_type().into()], false)
            }
            _ => return Err(CodegenError::new(format!("unknown owner helper `{name}`"))),
        };
        let function = self
            .module
            .add_function(name, fn_type, Some(Linkage::Internal));
        for attribute in ["nounwind", "nofree", "nosync"] {
            let kind = Attribute::get_named_enum_kind_id(attribute);
            function.add_attribute(
                AttributeLoc::Function,
                self.context.create_enum_attribute(kind, 0),
            );
        }
        if name == PROJECT_HELPER {
            // -O2's always-inline pass restores the inline projection; -O0
            // keeps one call per projection.
            let kind = Attribute::get_named_enum_kind_id("alwaysinline");
            function.add_attribute(
                AttributeLoc::Function,
                self.context.create_enum_attribute(kind, 0),
            );
        }
        // The helper is emitted while another body is open: keep that body's
        // debug scope off these instructions.
        let parked = self.park_builder();
        let result = match name {
            SET_HELPER => self.build_owner_set_helper(function),
            TRANSITION_HELPER => self.build_owner_transition_helper(function),
            _ => self.build_owner_project_helper(function),
        };
        self.restore_builder(parked);
        result.map(|()| function)
    }

    fn load_node_field(
        &self,
        node: PointerValue<'ctx>,
        field: u32,
        name: &str,
    ) -> CodegenResult<IntValue<'ctx>> {
        let ty = if field == NODE_STATE {
            self.context.i8_type()
        } else {
            self.context.i32_type()
        };
        let pointer = self
            .builder
            .build_struct_gep(self.owner_node_type(), node, field, name)
            .map_err(builder_error)?;
        Ok(self
            .builder
            .build_load(ty, pointer, name)
            .map_err(builder_error)?
            .into_int_value())
    }

    /// `node + offset` in nodes; `offset` is a signed i32 or i64.
    fn offset_node(
        &self,
        node: PointerValue<'ctx>,
        offset: IntValue<'ctx>,
        name: &str,
    ) -> CodegenResult<PointerValue<'ctx>> {
        let offset = self
            .builder
            .build_int_s_extend_or_bit_cast(offset, self.context.i64_type(), name)
            .map_err(builder_error)?;
        unsafe {
            self.builder
                .build_in_bounds_gep(self.owner_node_type(), node, &[offset], name)
        }
        .map_err(builder_error)
    }

    /// `set(node, state)`: stores `state` into `node` and every descendant.
    fn build_owner_set_helper(&mut self, function: FunctionValue<'ctx>) -> CodegenResult<()> {
        let i32_type = self.context.i32_type();
        let entry = self.context.append_basic_block(function, "entry");
        let children = self.context.append_basic_block(function, "children");
        let child = self.context.append_basic_block(function, "child");
        let done = self.context.append_basic_block(function, "done");
        let node = function
            .get_nth_param(0)
            .expect("set node")
            .into_pointer_value();
        let state = function
            .get_nth_param(1)
            .expect("set state")
            .into_int_value();

        self.builder.position_at_end(entry);
        let state_ptr = self
            .builder
            .build_struct_gep(self.owner_node_type(), node, NODE_STATE, "state.ptr")
            .map_err(builder_error)?;
        self.builder
            .build_store(state_ptr, state)
            .map_err(builder_error)?;
        let count = self.load_node_field(node, NODE_CHILD_COUNT, "count")?;
        let has_children = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                count,
                i32_type.const_zero(),
                "has.children",
            )
            .map_err(builder_error)?;
        self.builder
            .build_conditional_branch(has_children, children, done)
            .map_err(builder_error)?;

        self.builder.position_at_end(children);
        let first = self.load_node_field(node, NODE_FIRST_CHILD, "first")?;
        let base = self.offset_node(node, first, "base")?;
        self.builder
            .build_unconditional_branch(child)
            .map_err(builder_error)?;

        self.builder.position_at_end(child);
        let index = self
            .builder
            .build_phi(i32_type, "index")
            .map_err(builder_error)?;
        let index_value = index.as_basic_value().into_int_value();
        let target = self.offset_node(base, index_value, "target")?;
        self.builder
            .build_call(function, &[target.into(), state.into()], "")
            .map_err(builder_error)?;
        let next = self
            .builder
            .build_int_add(index_value, i32_type.const_int(1, false), "next")
            .map_err(builder_error)?;
        let more = self
            .builder
            .build_int_compare(IntPredicate::ULT, next, count, "more")
            .map_err(builder_error)?;
        self.builder
            .build_conditional_branch(more, child, done)
            .map_err(builder_error)?;
        index.add_incoming(&[(&i32_type.const_zero(), children), (&next, child)]);

        self.builder.position_at_end(done);
        self.builder.build_return(None).map_err(builder_error)?;
        Ok(())
    }

    /// `transition(node, state)`: `set(node, state)`, then recompute each
    /// ancestor as Init when every child is Init, Partial when any child is
    /// not Uninit, and Uninit otherwise.
    fn build_owner_transition_helper(
        &mut self,
        function: FunctionValue<'ctx>,
    ) -> CodegenResult<()> {
        let set = self.owner_helper(SET_HELPER)?;
        let pointer = self.context.ptr_type(AddressSpace::default());
        let i32_type = self.context.i32_type();
        let i8_type = self.context.i8_type();
        let bool_type = self.context.bool_type();
        let entry = self.context.append_basic_block(function, "entry");
        let ascend = self.context.append_basic_block(function, "ascend");
        let parent_block = self.context.append_basic_block(function, "parent");
        let scan = self.context.append_basic_block(function, "scan");
        let inspect = self.context.append_basic_block(function, "inspect");
        let update = self.context.append_basic_block(function, "update");
        let done = self.context.append_basic_block(function, "done");
        let node = function
            .get_nth_param(0)
            .expect("transition node")
            .into_pointer_value();
        let state = function
            .get_nth_param(1)
            .expect("transition state")
            .into_int_value();

        self.builder.position_at_end(entry);
        self.builder
            .build_call(set, &[node.into(), state.into()], "")
            .map_err(builder_error)?;
        self.builder
            .build_unconditional_branch(ascend)
            .map_err(builder_error)?;

        self.builder.position_at_end(ascend);
        let current = self
            .builder
            .build_phi(pointer, "current")
            .map_err(builder_error)?;
        let current_value = current.as_basic_value().into_pointer_value();
        let parent_offset = self.load_node_field(current_value, NODE_PARENT, "parent.offset")?;
        let has_parent = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                parent_offset,
                i32_type.const_zero(),
                "has.parent",
            )
            .map_err(builder_error)?;
        self.builder
            .build_conditional_branch(has_parent, parent_block, done)
            .map_err(builder_error)?;

        self.builder.position_at_end(parent_block);
        let parent = self.offset_node(current_value, parent_offset, "parent")?;
        let count = self.load_node_field(parent, NODE_CHILD_COUNT, "count")?;
        let first = self.load_node_field(parent, NODE_FIRST_CHILD, "first")?;
        let base = self.offset_node(parent, first, "base")?;
        self.builder
            .build_unconditional_branch(scan)
            .map_err(builder_error)?;

        self.builder.position_at_end(scan);
        let index = self
            .builder
            .build_phi(i32_type, "index")
            .map_err(builder_error)?;
        let all = self
            .builder
            .build_phi(bool_type, "all")
            .map_err(builder_error)?;
        let any = self
            .builder
            .build_phi(bool_type, "any")
            .map_err(builder_error)?;
        let index_value = index.as_basic_value().into_int_value();
        let all_value = all.as_basic_value().into_int_value();
        let any_value = any.as_basic_value().into_int_value();
        let more = self
            .builder
            .build_int_compare(IntPredicate::ULT, index_value, count, "more")
            .map_err(builder_error)?;
        self.builder
            .build_conditional_branch(more, inspect, update)
            .map_err(builder_error)?;

        self.builder.position_at_end(inspect);
        let child = self.offset_node(base, index_value, "child")?;
        let child_state = self.load_node_field(child, NODE_STATE, "child.state")?;
        let live = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                child_state,
                i8_type.const_int(INIT, false),
                "live",
            )
            .map_err(builder_error)?;
        let present = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                child_state,
                i8_type.const_zero(),
                "present",
            )
            .map_err(builder_error)?;
        let next_all = self
            .builder
            .build_and(all_value, live, "next.all")
            .map_err(builder_error)?;
        let next_any = self
            .builder
            .build_or(any_value, present, "next.any")
            .map_err(builder_error)?;
        let next_index = self
            .builder
            .build_int_add(index_value, i32_type.const_int(1, false), "next.index")
            .map_err(builder_error)?;
        self.builder
            .build_unconditional_branch(scan)
            .map_err(builder_error)?;
        index.add_incoming(&[
            (&i32_type.const_zero(), parent_block),
            (&next_index, inspect),
        ]);
        all.add_incoming(&[
            (&bool_type.const_int(1, false), parent_block),
            (&next_all, inspect),
        ]);
        any.add_incoming(&[
            (&bool_type.const_zero(), parent_block),
            (&next_any, inspect),
        ]);

        self.builder.position_at_end(update);
        let partial = self
            .builder
            .build_select(
                any_value,
                i8_type.const_int(PARTIAL, false),
                i8_type.const_zero(),
                "partial",
            )
            .map_err(builder_error)?
            .into_int_value();
        let summary = self
            .builder
            .build_select(
                all_value,
                i8_type.const_int(INIT, false),
                partial,
                "summary",
            )
            .map_err(builder_error)?;
        let parent_state = self
            .builder
            .build_struct_gep(self.owner_node_type(), parent, NODE_STATE, "parent.state")
            .map_err(builder_error)?;
        self.builder
            .build_store(parent_state, summary)
            .map_err(builder_error)?;
        self.builder
            .build_unconditional_branch(ascend)
            .map_err(builder_error)?;
        current.add_incoming(&[(&node, entry), (&parent, update)]);

        self.builder.position_at_end(done);
        self.builder.build_return(None).map_err(builder_error)?;
        Ok(())
    }

    /// `project(state, index)`: `state` itself for the initialized capability,
    /// otherwise its child `index`.
    fn build_owner_project_helper(&mut self, function: FunctionValue<'ctx>) -> CodegenResult<()> {
        let entry = self.context.append_basic_block(function, "entry");
        let tracked = self.context.append_basic_block(function, "tracked");
        let external = self.context.append_basic_block(function, "external");
        let state = function
            .get_nth_param(0)
            .expect("project state")
            .into_pointer_value();
        let index = function
            .get_nth_param(1)
            .expect("project index")
            .into_int_value();

        self.builder.position_at_end(entry);
        let initialized = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                state,
                self.initialized_reference_state(),
                "initialized",
            )
            .map_err(builder_error)?;
        self.builder
            .build_conditional_branch(initialized, external, tracked)
            .map_err(builder_error)?;

        self.builder.position_at_end(external);
        self.builder
            .build_return(Some(&state))
            .map_err(builder_error)?;

        self.builder.position_at_end(tracked);
        let child = self.tracked_reference_child(state, index)?;
        self.builder
            .build_return(Some(&child))
            .map_err(builder_error)?;
        Ok(())
    }

    /// Child `index` of a tracked node: `node + first_child + index`.
    pub(crate) fn tracked_reference_child(
        &self,
        node: PointerValue<'ctx>,
        index: IntValue<'ctx>,
    ) -> CodegenResult<PointerValue<'ctx>> {
        let first = self.load_node_field(node, NODE_FIRST_CHILD, "reference.first")?;
        let base = self.offset_node(node, first, "reference.children")?;
        self.offset_node(base, index, "reference.child")
    }
}

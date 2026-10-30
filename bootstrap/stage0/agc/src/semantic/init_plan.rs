//! Typed owner-local initialization layouts produced by move analysis.
//!
//! The plan is keyed by source declaration identity and concrete function
//! signature. Codegen may consume it only for the matching body.

use rustc_hash::FxHashMap;

use crate::lexer::Span;
use crate::parser::ast;
use crate::semantic::move_path::InitState;
use crate::semantic::place::Place;
use crate::types::Type;

pub const MAX_OWNER_NODES: usize = 65536;

/// Only inline, statically resolved projections can own runtime state.
#[derive(Clone, Debug)]
pub enum LayoutProjection {
    Field(u32),
    Element(u32),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct SpanId {
    pub file: u32,
    pub start: usize,
    pub end: usize,
    pub expansion: u32,
}

impl From<Span> for SpanId {
    fn from(span: Span) -> Self {
        Self {
            file: span.file,
            start: span.start,
            end: span.end,
            expansion: span.expansion,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FunctionId {
    pub declaration: SpanId,
    /// Concrete canonical signature. Generic instances cannot consume a
    /// template plan because their lowered field types differ.
    pub signature: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct OwnerLocalId {
    pub declaration: SpanId,
    pub name: String,
}

#[derive(Clone, Debug)]
pub struct InitNodePlan {
    pub place: Place,
    pub ty: ast::Type,
    pub parent: Option<usize>,
    pub first_child: usize,
    pub child_count: u32,
    pub initial_state: InitState,
    pub projection: Option<LayoutProjection>,
}

#[derive(Clone, Debug)]
pub struct OwnerLocalPlan {
    pub id: OwnerLocalId,
    pub ty: ast::Type,
    pub nodes: Vec<InitNodePlan>,
    pub initialized_at_declaration: bool,
}

#[derive(Clone, Debug)]
pub struct OwnerTransition {
    pub site: SpanId,
    pub place: Place,
    pub state: InitState,
}

#[derive(Clone, Debug, Default)]
pub struct FunctionInitPlan {
    pub owners: Vec<OwnerLocalPlan>,
    pub transitions: Vec<OwnerTransition>,
    pub places: FxHashMap<SpanId, ResolvedOwnerPlace>,
}

#[derive(Clone, Debug)]
pub struct ResolvedOwnerPlace {
    pub owner: OwnerLocalId,
    pub node: usize,
}

#[derive(Clone, Debug, Default)]
pub struct ProgramInitPlan {
    pub functions: FxHashMap<FunctionId, FunctionInitPlan>,
    pub(crate) layouts: super::move_check::OwnerLayouts,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum InitPlanError {
    EmptyOwnerLayout,
    InvalidChildRange(usize),
    MissingPlace(Place),
    TooManyNodes,
    RecursiveLayout,
}

impl FunctionId {
    pub fn for_body(
        parameters: &[ast::Parameter],
        return_type: Option<&ast::Type>,
        body: &ast::Block,
    ) -> Self {
        Self {
            declaration: body.span.into(),
            signature: format!(
                "({})->{}",
                parameters
                    .iter()
                    .map(|p| Type::from_ast(&p.param_type).canonical_key())
                    .collect::<Vec<_>>()
                    .join(","),
                return_type
                    .map(|t| Type::from_ast(t).canonical_key())
                    .unwrap_or_else(|| "void".into())
            ),
        }
    }
}

impl ProgramInitPlan {
    pub fn ensure_function(
        &mut self,
        parameters: &[ast::Parameter],
        return_type: Option<&ast::Type>,
        body: &ast::Block,
    ) -> Result<(), String> {
        let id = FunctionId::for_body(parameters, return_type, body);
        if !self.functions.contains_key(&id) {
            let plan = self.layouts.analyze_body(parameters, return_type, body)?;
            self.functions.insert(id, plan);
        }
        Ok(())
    }
    /// Complete synthesized/lazily monomorphized locals using the same typed
    /// layout resolver as move analysis, never the function's final move state.
    pub fn owner_layout(
        &self,
        function: &FunctionId,
        id: OwnerLocalId,
        ty: &ast::Type,
        initialized: bool,
    ) -> Result<OwnerLocalPlan, InitPlanError> {
        if let Some(owner) = self.functions.get(function).and_then(|f| f.owner(&id)) {
            if owner.initialized_at_declaration == initialized
                && Type::from_ast(&owner.ty).canonical_key() == Type::from_ast(ty).canonical_key()
            {
                return Ok(owner.clone());
            }
        }
        self.layouts.owner(id, ty, initialized)
    }
}

impl OwnerLocalPlan {
    pub(crate) fn resolve_expression(&self, expr: &ast::Expression) -> Option<usize> {
        match expr.kind.as_ref() {
            ast::ExpressionKind::Identifier(name) if name.name == self.id.name => Some(0),
            ast::ExpressionKind::FieldAccess { object, field } => {
                let parent = self.resolve_expression(object)?;
                let node = &self.nodes[parent];
                (node.first_child..node.first_child + node.child_count as usize).find(|&i| {
                    match self.nodes[i].place.projections.last() {
                        Some(super::place::Projection::Field(name)) => name == &field.name,
                        Some(super::place::Projection::TupleField(index)) => {
                            index.to_string() == field.name
                        }
                        _ => false,
                    }
                })
            }
            ast::ExpressionKind::Index { object, index } => {
                let parent = self.resolve_expression(object)?;
                let ast::ExpressionKind::Literal(ast::Literal::Integer(index)) =
                    index.kind.as_ref()
                else {
                    return None;
                };
                let index = usize::try_from(*index).ok()?;
                let node = &self.nodes[parent];
                (node.first_child..node.first_child + node.child_count as usize).find(|&i| {
                    match self.nodes[i].place.projections.last() {
                        Some(
                            super::place::Projection::TupleField(element)
                            | super::place::Projection::ArrayElement(element),
                        ) => *element == index,
                        _ => false,
                    }
                })
            }
            _ => None,
        }
    }
    /// Compatibility lookup for existing move emitters. Paths are derived from
    /// resolved semantic nodes; LLVM never resolves field names or field types.
    pub fn node_for_path(&self, path: &str) -> Result<Option<usize>, InitPlanError> {
        let mut index = 0;
        if path.is_empty() {
            return Ok(Some(index));
        }
        for component in path.split('.') {
            let node = &self.nodes[index];
            if matches!(
                node.ty.kind.as_ref(),
                ast::TypeKind::Pointer(_) | ast::TypeKind::Reference(_) | ast::TypeKind::Slice(_)
            ) {
                return Ok(None);
            }
            let element = component.parse::<usize>().ok();
            index = (node.first_child..node.first_child + node.child_count as usize)
                .find(|&child| match self.nodes[child].place.projections.last() {
                    Some(super::place::Projection::Field(name)) => name == component,
                    Some(super::place::Projection::TupleField(i)
                        | super::place::Projection::ArrayElement(i)) => element == Some(*i),
                    _ => false,
                })
                .ok_or_else(|| InitPlanError::MissingPlace(node.place.clone()))?;
        }
        Ok(Some(index))
    }
}

impl FunctionInitPlan {
    pub fn owner(&self, id: &OwnerLocalId) -> Option<&OwnerLocalPlan> {
        self.owners.iter().find(|owner| &owner.id == id)
    }
}

impl OwnerLocalPlan {
    pub fn validate(&self) -> Result<(), InitPlanError> {
        if self.nodes.is_empty() {
            return Err(InitPlanError::EmptyOwnerLayout);
        }
        if self.nodes.len() > MAX_OWNER_NODES {
            return Err(InitPlanError::TooManyNodes);
        }
        if self.nodes[0].parent.is_some() || self.nodes[0].projection.is_some() {
            return Err(InitPlanError::InvalidChildRange(0));
        }
        let mut next_child = 1;
        for (index, node) in self.nodes.iter().enumerate() {
            let end = node
                .first_child
                .checked_add(node.child_count as usize)
                .ok_or(InitPlanError::InvalidChildRange(index))?;
            if end > self.nodes.len() || (node.child_count > 0 && node.first_child <= index) {
                return Err(InitPlanError::InvalidChildRange(index));
            }
            if node.child_count > 0 {
                if node.first_child != next_child {
                    return Err(InitPlanError::InvalidChildRange(index));
                }
                for child in &self.nodes[node.first_child..end] {
                    if child.parent != Some(index)
                        || child.place.parent().as_ref() != Some(&node.place)
                        || child.projection.is_none()
                    {
                        return Err(InitPlanError::InvalidChildRange(index));
                    }
                }
                next_child = end;
            }
            if let Some(parent) = node.parent {
                if parent >= index {
                    return Err(InitPlanError::InvalidChildRange(index));
                }
            } else if index != 0 {
                return Err(InitPlanError::InvalidChildRange(index));
            }
        }
        if next_child != self.nodes.len() {
            return Err(InitPlanError::InvalidChildRange(next_child));
        }
        Ok(())
    }
}

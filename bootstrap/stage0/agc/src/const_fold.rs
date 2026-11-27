//! Compile-time folding of integer and bool constant initializers, shared by
//! code generation and exported module metadata.

use rustc_hash::FxHashMap as HashMap;

use crate::parser::ast;

/// Width and signedness of an integer or bool constant. Values are held as
/// `i128`; `wrap` truncates them the way the runtime operation would.
#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) struct ConstIntShape {
    pub(crate) bits: u32,
    pub(crate) unsigned: bool,
}

impl ConstIntShape {
    /// No truncation: bare literals compared with each other or under a cast.
    pub(crate) const WIDE: Self = Self {
        bits: 128,
        unsigned: false,
    };
    pub(crate) const BOOL: Self = Self {
        bits: 1,
        unsigned: true,
    };
    const CHAR: Self = Self {
        bits: 32,
        unsigned: true,
    };

    /// Shape of a primitive integer, `bool` or `char` type.
    pub(crate) fn primitive(ty: &ast::Type) -> Option<Self> {
        use ast::PrimitiveType as P;
        let ast::TypeKind::Primitive(primitive) = ty.kind.as_ref() else {
            return None;
        };
        let (bits, unsigned) = match primitive {
            P::I8 => (8, false),
            P::I16 => (16, false),
            P::I32 => (32, false),
            P::I64 => (64, false),
            P::I128 => (128, false),
            P::U8 => (8, true),
            P::U16 => (16, true),
            P::U32 => (32, true),
            P::U64 => (64, true),
            P::U128 => (128, true),
            P::Bool => return Some(Self::BOOL),
            P::Char => return Some(Self::CHAR),
            _ => return None,
        };
        Some(Self { bits, unsigned })
    }

    pub(crate) fn mask(self) -> u128 {
        if self.bits >= 128 {
            u128::MAX
        } else {
            (1u128 << self.bits) - 1
        }
    }

    pub(crate) fn wrap(self, value: i128) -> i128 {
        if self.bits >= 128 {
            return value;
        }
        let truncated = (value as u128) & self.mask();
        let sign_bit = 1u128 << (self.bits - 1);
        if !self.unsigned && truncated & sign_bit != 0 {
            (truncated | !self.mask()) as i128
        } else {
            truncated as i128
        }
    }
}

/// A folded constant operand. `shape` is its type; `None` marks a bare
/// literal, which takes the type of the operation around it.
#[derive(Clone, Copy, Debug)]
pub(crate) struct ConstOperand {
    pub(crate) value: i128,
    pub(crate) shape: Option<ConstIntShape>,
}

impl ConstOperand {
    pub(crate) fn typed(value: i128, shape: ConstIntShape) -> Self {
        Self {
            value,
            shape: Some(shape),
        }
    }
}

/// What folding needs from the code around it.
pub(crate) trait ConstEnv {
    /// Shape of integer or bool type `ty`; `None` for a type that does not fold.
    fn shape(&mut self, ty: &ast::Type) -> Option<ConstIntShape>;
    /// Value of constant `name` in its declared type; `None` when `name` is
    /// not a foldable constant here (including a cycle).
    fn reference(&mut self, name: &str) -> Option<ConstOperand>;
}

/// Folds an integer or bool constant expression and converts it to the
/// declared type `shape`. Operands are literals, other constants, casts, and
/// unary or binary operators; `None` means the expression is not a
/// compile-time constant (or divides by zero).
pub(crate) fn fold_const_int(
    env: &mut impl ConstEnv,
    expr: &ast::Expression,
    shape: ConstIntShape,
) -> Option<i128> {
    let folded = fold_const_operand(env, expr, shape)?;
    Some(shape.wrap(folded.value))
}

/// Folds `expr` in its own type, like the runtime operation: a typed operand
/// (a constant, cast, bool or char) fixes the type of the operation, and a
/// bare literal takes the type of `context`.
fn fold_const_operand(
    env: &mut impl ConstEnv,
    expr: &ast::Expression,
    context: ConstIntShape,
) -> Option<ConstOperand> {
    let operand = match expr.kind.as_ref() {
        ast::ExpressionKind::Literal(ast::Literal::Integer(value)) => ConstOperand {
            value: *value,
            shape: None,
        },
        ast::ExpressionKind::Literal(ast::Literal::Bool(value)) => {
            ConstOperand::typed(i128::from(*value), ConstIntShape::BOOL)
        }
        ast::ExpressionKind::Literal(ast::Literal::Char(value)) => {
            ConstOperand::typed(i128::from(u32::from(*value)), ConstIntShape::CHAR)
        }
        ast::ExpressionKind::Identifier(identifier) => env.reference(&identifier.name)?,
        ast::ExpressionKind::Cast {
            expression,
            target_type,
        } => {
            let cast_shape = env.shape(target_type)?;
            let inner = fold_const_operand(env, expression, ConstIntShape::WIDE)?;
            let source = inner.shape.unwrap_or(ConstIntShape::WIDE);
            ConstOperand::typed(cast_shape.wrap(source.wrap(inner.value)), cast_shape)
        }
        ast::ExpressionKind::Unary { operator, operand } => {
            let inner = fold_const_operand(env, operand, context)?;
            // A bare literal stays unwrapped until an operation types it.
            let shape = inner.shape.unwrap_or(ConstIntShape::WIDE);
            let value = shape.wrap(inner.value);
            let value = match operator {
                ast::UnaryOperator::Plus => value,
                ast::UnaryOperator::Minus => value.wrapping_neg(),
                ast::UnaryOperator::Not if shape.bits == 1 => i128::from(value == 0),
                ast::UnaryOperator::Not | ast::UnaryOperator::BitwiseNot => !value,
                _ => return None,
            };
            ConstOperand {
                value: shape.wrap(value),
                shape: inner.shape,
            }
        }
        ast::ExpressionKind::Binary {
            left,
            operator,
            right,
        } => {
            // Comparison operands are integers, not the bool result.
            let operand_context = if is_comparison(operator) {
                ConstIntShape::WIDE
            } else {
                context
            };
            let lhs = fold_const_operand(env, left, operand_context)?;
            let rhs = fold_const_operand(env, right, operand_context)?;
            fold_const_binary(operator, lhs, rhs, context)?
        }
        _ => return None,
    };
    Some(operand)
}

/// Folds `lhs operator rhs` in the operands' type (`context` when both are
/// bare literals), wrapping like the runtime operation. Comparisons and
/// logical operators produce a bool.
fn fold_const_binary(
    operator: &ast::BinaryOperator,
    lhs: ConstOperand,
    rhs: ConstOperand,
    context: ConstIntShape,
) -> Option<ConstOperand> {
    use ast::BinaryOperator as Op;
    // Mixed widths widen to the wider operand; at equal width unsigned wins.
    let typed = match (lhs.shape, rhs.shape) {
        (Some(l), Some(r)) => Some(ConstIntShape {
            bits: l.bits.max(r.bits),
            unsigned: if l.bits == r.bits {
                l.unsigned || r.unsigned
            } else if l.bits > r.bits {
                l.unsigned
            } else {
                r.unsigned
            },
        }),
        (l, r) => l.or(r),
    };
    let compares = is_comparison(operator);
    let shape = typed.unwrap_or(if compares {
        ConstIntShape::WIDE
    } else {
        context
    });
    let (l, r) = (shape.wrap(lhs.value), shape.wrap(rhs.value));
    // Unsigned operands compare and divide by their bit patterns.
    let (ul, ur) = ((l as u128) & shape.mask(), (r as u128) & shape.mask());
    if compares {
        let truth = match operator {
            Op::Equal => l == r,
            Op::NotEqual => l != r,
            Op::Less if shape.unsigned => ul < ur,
            Op::Greater if shape.unsigned => ul > ur,
            Op::LessEqual if shape.unsigned => ul <= ur,
            Op::GreaterEqual if shape.unsigned => ul >= ur,
            Op::Less => l < r,
            Op::Greater => l > r,
            Op::LessEqual => l <= r,
            Op::GreaterEqual => l >= r,
            Op::LogicalAnd => l != 0 && r != 0,
            _ => l != 0 || r != 0,
        };
        return Some(ConstOperand::typed(i128::from(truth), ConstIntShape::BOOL));
    }
    let value = match operator {
        Op::Add => l.wrapping_add(r),
        Op::Subtract => l.wrapping_sub(r),
        Op::Multiply => l.wrapping_mul(r),
        Op::Divide | Op::Modulo if r == 0 => return None,
        Op::Divide if shape.unsigned => (ul / ur) as i128,
        Op::Modulo if shape.unsigned => (ul % ur) as i128,
        Op::Divide => l.wrapping_div(r),
        Op::Modulo => l.wrapping_rem(r),
        Op::BitwiseAnd => l & r,
        Op::BitwiseOr => l | r,
        Op::BitwiseXor => l ^ r,
        Op::LeftShift | Op::RightShift => {
            let amount = u32::try_from(r).ok().filter(|n| *n < shape.bits)?;
            match operator {
                Op::LeftShift => l.wrapping_shl(amount),
                _ if shape.unsigned => (ul >> amount) as i128,
                _ => l >> amount,
            }
        }
        _ => return None,
    };
    Some(ConstOperand {
        value: shape.wrap(value),
        shape: typed,
    })
}

/// Folds the immutable integer and bool globals of one program by name,
/// each once, for code that has no code generator (module metadata).
pub(crate) struct ProgramConsts<'a> {
    initializers: HashMap<&'a str, (&'a ast::Type, &'a ast::Expression)>,
    values: HashMap<String, i128>,
    active: Vec<String>,
}

impl<'a> ProgramConsts<'a> {
    pub(crate) fn new(program: &'a ast::Program) -> Self {
        let mut initializers = HashMap::default();
        for item in &program.items {
            if let ast::ItemKind::GlobalVariable(global) = &item.kind
                && !global.is_mutable
                && !global.is_volatile
                && let Some(init) = &global.initializer
            {
                initializers.insert(global.name.name.as_str(), (&global.var_type, init));
            }
        }
        Self {
            initializers,
            values: HashMap::default(),
            active: Vec::new(),
        }
    }

    /// Folded value of constant `name` in its declared type, if it folds.
    pub(crate) fn value(&mut self, name: &str) -> Option<i128> {
        self.reference(name).map(|operand| operand.value)
    }
}

impl ConstEnv for ProgramConsts<'_> {
    fn shape(&mut self, ty: &ast::Type) -> Option<ConstIntShape> {
        ConstIntShape::primitive(ty)
    }

    fn reference(&mut self, name: &str) -> Option<ConstOperand> {
        let (ty, init) = *self.initializers.get(name)?;
        let shape = ConstIntShape::primitive(ty)?;
        if let Some(value) = self.values.get(name) {
            return Some(ConstOperand::typed(*value, shape));
        }
        if self.active.iter().any(|active| active == name) {
            return None;
        }
        self.active.push(name.to_string());
        let value = fold_const_int(self, init, shape);
        self.active.pop();
        let value = value?;
        self.values.insert(name.to_string(), value);
        Some(ConstOperand::typed(value, shape))
    }
}

/// True for operators whose result is a bool: comparisons and logical
/// operators.
fn is_comparison(operator: &ast::BinaryOperator) -> bool {
    use ast::BinaryOperator as Op;
    matches!(
        operator,
        Op::Equal
            | Op::NotEqual
            | Op::Less
            | Op::Greater
            | Op::LessEqual
            | Op::GreaterEqual
            | Op::LogicalAnd
            | Op::LogicalOr
    )
}

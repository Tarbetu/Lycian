use crate::{Expression, ExpressionKind};
pub use rug::Float as RugFloat;
use std::mem::discriminant;
use std::ops::*;
use std::rc::Rc;

pub const PRECISION: u32 = 53;

#[derive(Clone, Debug)]
pub enum Literal {
    Integer(RugFloat),
    Float(RugFloat),
    Boolean(bool),
    Char(u32),
    Str(Rc<String>),
    LiteralList(Vec<Expression>),
    LiteralArray(Vec<Expression>),
}

impl Add for &Literal {
    type Output = Option<Literal>;

    fn add(self, rhs: Self) -> Self::Output {
        use Literal::{Float, Integer, LiteralList, Str};

        match (self, rhs) {
            (Integer(lhs), Integer(rhs)) => Some(Integer(alloc_num(lhs + rhs))),
            (Float(lhs), Float(rhs)) => Some(Float(alloc_num(lhs + rhs))),
            (Str(lhs), Str(rhs)) => Some(Str(Rc::new(format!("{}{}", lhs, rhs)))),
            (LiteralList(lhs), LiteralList(rhs)) => {
                Some(LiteralList([lhs.clone(), rhs.clone()].concat()))
            }
            _ => None,
        }
    }
}

impl Sub for &Literal {
    type Output = Option<Literal>;

    fn sub(self, rhs: Self) -> Self::Output {
        use Literal::{Float, Integer};

        match (self, rhs) {
            (Integer(lhs), Integer(rhs)) => Some(Integer(alloc_num(lhs - rhs))),
            (Float(lhs), Float(rhs)) => Some(Float(alloc_num(lhs - rhs))),
            _ => None,
        }
    }
}

impl Mul for &Literal {
    type Output = Option<Literal>;

    fn mul(self, rhs: Self) -> Self::Output {
        use Literal::{Float, Integer};

        match (self, rhs) {
            (Integer(lhs), Integer(rhs)) => Some(Integer(alloc_num(lhs * rhs))),
            (Float(lhs), Float(rhs)) => Some(Float(alloc_num(lhs * rhs))),
            _ => None,
        }
    }
}

impl Div for &Literal {
    type Output = Option<Literal>;

    fn div(self, rhs: Self) -> Self::Output {
        use Literal::{Float, Integer};

        match (self, rhs) {
            (Integer(lhs), Integer(rhs)) => Some(Float(alloc_num(lhs / rhs))),
            (Float(lhs), Float(rhs)) => Some(Float(alloc_num(lhs / rhs))),
            _ => None,
        }
    }
}

impl Rem for &Literal {
    type Output = Option<Literal>;

    fn rem(self, rhs: Self) -> Self::Output {
        use Literal::{Float, Integer};

        match (self, rhs) {
            (Integer(lhs), Integer(rhs)) => Some(Integer(alloc_num(lhs % rhs))),
            (Float(lhs), Float(rhs)) => Some(Float(alloc_num(lhs % rhs))),
            _ => None,
        }
    }
}

// Used for logical and
impl BitAnd for &Literal {
    type Output = Option<Literal>;

    fn bitand(self, rhs: Self) -> Self::Output {
        use Literal::Boolean;

        match (self, rhs) {
            (Boolean(lhs), Boolean(rhs)) => Some(Boolean(*lhs && *rhs)),
            _ => None,
        }
    }
}

// Used for logical or
impl BitOr for &Literal {
    type Output = Option<Literal>;

    fn bitor(self, rhs: Self) -> Self::Output {
        use Literal::Boolean;

        match (self, rhs) {
            (Boolean(lhs), Boolean(rhs)) => Some(Boolean(*lhs || *rhs)),
            _ => None,
        }
    }
}

impl PartialEq for Literal {
    fn eq(&self, other: &Self) -> bool {
        use core::cmp::Ordering;
        use Literal::*;

        // The comparision for lists and maps is might seem uneffective, due to it's done in O(n)
        // But they are just literals, so I don't expect them to be too large
        match (self, other) {
            (Integer(lhs), Integer(rhs)) => lhs == rhs,
            (Float(lhs), Float(rhs)) => lhs == rhs,
            (Boolean(lhs), Boolean(rhs)) => lhs == rhs,
            (Str(lhs), Str(rhs)) => lhs == rhs,
            (LiteralList(lhs), LiteralList(rhs)) => {
                lhs.len() == rhs.len() && lhs.iter().eq(rhs.iter())
            }
            (LiteralArray(lhs), LiteralArray(rhs)) => {
                lhs.len() == rhs.len() && lhs.iter().eq(rhs.iter())
            }
            _ => false,
        }
    }
}

impl Eq for Literal {}

impl std::hash::Hash for Literal {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        use Literal::*;
        discriminant(self).hash(state);

        match self {
            Integer(num) | Float(num) => {
                if num.is_nan() {
                    0u8.hash(state);
                    return;
                }

                if num.is_zero() {
                    1u8.hash(state);
                    return;
                }

                if num.is_infinite() {
                    2u8.hash(state);
                    return;
                }

                let (mantissa, exp) = num.to_integer_exp().unwrap();

                3u8.hash(state);
                exp.hash(state);
                mantissa.hash(state);
            }
            Boolean(bool) => bool.hash(state),
            Char(char) => char.hash(state),
            Str(str) => str.hash(state),
            LiteralList(expr_list) | LiteralArray(expr_list) => expr_list.hash(state),
        }
    }
}

impl Not for &Literal {
    type Output = Option<Literal>;

    fn not(self) -> Self::Output {
        use Literal::Boolean;

        match self {
            Boolean(b) => Some(Boolean(!b)),
            _ => None,
        }
    }
}

impl Neg for &Literal {
    type Output = Option<Literal>;

    fn neg(self) -> Self::Output {
        use Literal::{Float, Integer};

        match self {
            Integer(i) => Some(Integer(alloc_num(-i))),
            Float(f) => Some(Float(alloc_num(-f))),
            _ => None,
        }
    }
}

impl Literal {
    pub fn and(&self, rhs: &Self) -> Option<Self> {
        use Literal::Boolean;

        match (self, rhs) {
            (Boolean(lhs), Boolean(rhs)) => Some(Boolean(*lhs && *rhs)),
            _ => None,
        }
    }

    pub fn or(&self, rhs: &Self) -> Option<Self> {
        use Literal::Boolean;

        match (self, rhs) {
            (Boolean(lhs), Boolean(rhs)) => Some(Boolean(*lhs || *rhs)),
            _ => None,
        }
    }

    pub fn in_operator(&self, rhs: &Self) -> Option<Self> {
        use Literal::{Boolean, LiteralArray, LiteralList};

        match (self, rhs) {
            (lhs, LiteralList(rhs) | LiteralArray(rhs)) => Some(Boolean(rhs.iter().any(|i| {
                if let ExpressionKind::Literal(i) = i.kind.as_ref() {
                    i.as_ref() == lhs
                } else {
                    false
                }
            }))),
            _ => None,
        }
    }

    pub fn is_equal(&self, rhs: &Self) -> Option<Self> {
        use Literal::Boolean;

        Some(Boolean(self == rhs))
    }

    pub fn is_not_equal(&self, rhs: &Self) -> Option<Self> {
        use Literal::Boolean;

        Some(Boolean(self != rhs))
    }

    pub fn greater(&self, rhs: &Self) -> Option<Self> {
        use Literal::{Boolean, Float, Integer, Str};

        match (self, rhs) {
            (Integer(lhs), Integer(rhs)) => Some(Boolean(lhs > rhs)),
            (Float(lhs), Integer(rhs)) => Some(Boolean(lhs > rhs)),
            (Integer(lhs), Float(rhs)) => Some(Boolean(lhs > rhs)),
            (Float(lhs), Float(rhs)) => Some(Boolean(lhs > rhs)),
            (Str(lhs), Str(rhs)) => Some(Boolean(lhs > rhs)),
            _ => unimplemented!(),
        }
    }

    pub fn greater_equal(&self, rhs: &Self) -> Option<Self> {
        self.greater(rhs).and_then(|x| x.or(&self.is_equal(rhs)?))
    }

    pub fn less(&self, rhs: &Self) -> Option<Self> {
        self.greater_equal(rhs).and_then(|x| x.not())
    }

    pub fn less_equal(&self, rhs: &Self) -> Option<Self> {
        self.greater(rhs).and_then(|x| x.not())
    }

    pub fn get(&self, key: &Self) -> Option<Expression> {
        use Literal::{Integer, LiteralArray, LiteralList};

        match (self, key) {
            (LiteralList(list) | LiteralArray(list), Integer(num)) => num
                .to_integer()
                .and_then(|int| int.to_usize())
                .and_then(|index| list.get(index).cloned()),
            _ => unreachable!(),
        }
    }
}

fn alloc_num<T>(value: T) -> RugFloat
where
    RugFloat: rug::Assign<T>,
{
    RugFloat::with_val(PRECISION, value)
}

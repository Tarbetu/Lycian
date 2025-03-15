use ahash::AHashMap;
pub use rug::Float as RugFloat;
use std::ops::*;

pub const PRECISION: u32 = 52;

use crate::Expression;
use either::Either;

pub type MaybeLiteral = Either<Expression, Literal>;

#[derive(Clone, Debug)]
pub enum Literal {
    Integer(RugFloat),
    Float(RugFloat),
    Boolean(bool),
    Char(u32),
    Str(String),
    LiteralList(Vec<MaybeLiteral>),
    LiteralMap(AHashMap<MaybeLiteral, MaybeLiteral>),
}

impl Add for &Literal {
    type Output = Option<Literal>;

    fn add(self, rhs: Self) -> Self::Output {
        use Literal::{Float, Integer, LiteralList, Str};

        match (self, rhs) {
            (Integer(lhs), Integer(rhs)) => Some(Integer(rug(lhs + rhs))),
            (Float(lhs), Float(rhs)) => Some(Float(rug(lhs + rhs))),
            (Str(lhs), Str(rhs)) => Some(Str(format!("{}{}", lhs, rhs))),
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
            (Integer(lhs), Integer(rhs)) => Some(Integer(rug(lhs - rhs))),
            (Float(lhs), Float(rhs)) => Some(Float(rug(lhs - rhs))),
            _ => None,
        }
    }
}

impl Mul for &Literal {
    type Output = Option<Literal>;

    fn mul(self, rhs: Self) -> Self::Output {
        use Literal::{Float, Integer};

        match (self, rhs) {
            (Integer(lhs), Integer(rhs)) => Some(Integer(rug(lhs * rhs))),
            (Float(lhs), Float(rhs)) => Some(Float(rug(lhs * rhs))),
            _ => None,
        }
    }
}

impl Div for &Literal {
    type Output = Option<Literal>;

    fn div(self, rhs: Self) -> Self::Output {
        use Literal::{Float, Integer};

        match (self, rhs) {
            (Integer(lhs), Integer(rhs)) => Some(Float(rug(lhs / rhs))),
            (Float(lhs), Float(rhs)) => Some(Float(rug(lhs / rhs))),
            _ => None,
        }
    }
}

impl Rem for &Literal {
    type Output = Option<Literal>;

    fn rem(self, rhs: Self) -> Self::Output {
        use Literal::{Float, Integer};

        match (self, rhs) {
            (Integer(lhs), Integer(rhs)) => Some(Integer(rug(lhs % rhs))),
            (Float(lhs), Float(rhs)) => Some(Float(rug(lhs % rhs))),
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
        use Literal::*;

        // The comparision for lists and maps is might seem uneffective, due to it done in O(n)
        // But they are just literals, so I don't expect them to be too large
        match (self, other) {
            (Integer(lhs), Integer(rhs)) => lhs == rhs,
            (Float(lhs), Float(rhs)) => lhs == rhs,
            (Boolean(lhs), Boolean(rhs)) => lhs == rhs,
            (Str(lhs), Str(rhs)) => lhs == rhs,
            (LiteralList(lhs), LiteralList(rhs)) => {
                lhs.len() == rhs.len() && lhs.iter().eq(rhs.iter())
            }
            (LiteralMap(lhs), LiteralMap(rhs)) => {
                lhs.len() == rhs.len() && lhs.iter().eq(rhs.iter())
            }
            _ => false,
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
            Integer(i) => Some(Integer(rug(-i))),
            Float(f) => Some(Float(rug(-f))),
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
        use Literal::{Boolean, LiteralList, LiteralMap};

        match (self, rhs) {
            (lhs, LiteralList(rhs)) => {
                Some(Boolean(rhs.iter().any(|x| {
                    x.as_ref().right().map(|x| x == lhs).unwrap_or(false)
                })))
            }
            (lhs, LiteralMap(rhs)) => {
                Some(Boolean(rhs.keys().any(|x| {
                    x.as_ref().right().map(|x| x == lhs).unwrap_or(false)
                })))
            }
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

    pub fn get(&self, key: &Self) -> Option<MaybeLiteral> {
        use Literal::{Integer, LiteralList, LiteralMap};

        match (self, key) {
            (LiteralList(list), Integer(num)) => num
                .to_integer()
                .and_then(|int| int.to_usize())
                .and_then(|index| list.get(index).cloned()),
            (LiteralMap(map), ref key) => map
                .iter()
                .find(|(k, _)| k.as_ref().right().map(|k| &k == key).unwrap_or(false))
                .map(|(_, v)| v.clone()),
            _ => unreachable!(),
        }
    }
}

fn rug<T>(value: T) -> RugFloat
where
    RugFloat: rug::Assign<T>,
{
    RugFloat::with_val(PRECISION, value)
}

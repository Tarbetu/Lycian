use crate::definition::TypeId;

#[allow(non_snake_case)]
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct EmbeddedTypes {
    pub object: TypeId,
    pub int8: TypeId,
    pub int16: TypeId,
    pub int32: TypeId,
    pub int64: TypeId,
    pub int128: TypeId,
    pub intSize: TypeId,
    pub uInt8: TypeId,
    pub uInt16: TypeId,
    pub uInt32: TypeId,
    pub uInt64: TypeId,
    pub uInt128: TypeId,
    pub uIntSize: TypeId,
    pub float32: TypeId,
    pub float64: TypeId,
    pub boolean: TypeId,
    pub char: TypeId,
    pub void: TypeId,
    pub array: TypeId,
    pub linked_list: TypeId,
    pub string: TypeId,
    pub function: TypeId,
    pub literal_integer: TypeId,
    pub literal_float: TypeId,
    pub literal_true: TypeId,
    pub literal_false: TypeId,
}

impl EmbeddedTypes {
    pub fn is_integer(&self, type_id: TypeId) -> bool {
        (self.int8.0..=self.uIntSize.0).contains(&type_id.0) || type_id == self.literal_integer
    }

    pub fn is_float(&self, type_id: TypeId) -> bool {
        (self.float32.0..=self.float64.0).contains(&type_id.0) || type_id == self.literal_float
    }

    pub fn is_number(&self, type_id: TypeId) -> bool {
        self.is_integer(type_id) || self.is_float(type_id)
    }

    pub fn is_literal_number(&self, type_id: TypeId) -> bool {
        type_id == self.literal_integer || type_id == self.literal_float
    }
}

pub const EMBEDDED_TYPES: EmbeddedTypes = EmbeddedTypes {
    object: TypeId(0),
    int8: TypeId(1),
    int16: TypeId(2),
    int32: TypeId(3),
    int64: TypeId(4),
    int128: TypeId(5),
    intSize: TypeId(6),
    uInt8: TypeId(7),
    uInt16: TypeId(8),
    uInt32: TypeId(9),
    uInt64: TypeId(10),
    uInt128: TypeId(11),
    uIntSize: TypeId(12),
    float32: TypeId(13),
    float64: TypeId(14),
    boolean: TypeId(15),
    char: TypeId(16),
    void: TypeId(17),
    array: TypeId(18),
    linked_list: TypeId(19),
    string: TypeId(20),
    function: TypeId(21),
    literal_integer: TypeId(22),
    literal_float: TypeId(23),
    literal_true: TypeId(24),
    literal_false: TypeId(25),
};

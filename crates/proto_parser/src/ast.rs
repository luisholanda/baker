/// A complete identifier.
pub type FullIdent<'i> = Vec<&'i str>;

/// A protobuf constant.
#[derive(Debug, Clone, PartialEq)]
pub enum Constant<'i> {
    /// An identifier value.
    FullIdent(FullIdent<'i>),
    /// A float value.
    FloatLit(f64),
    /// A string value.
    StrLit(String),
    /// A boolean value.
    BoolLit(bool),
    /// An uninterpreted protobuf value.
    ///
    /// This will come from complex options.
    UninterpretedValue(&'i str),
}

/// Possible labels for fields.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FieldLabel {
    /// The field was marked as `repeated`.
    Repeated,
    /// The field was marked as `optional`.
    Optional,
}

/// Possible field types.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FieldType<'i> {
    /// Double type
    Double,
    /// Float type
    Float,
    /// Int type.
    Int32,
    /// Long type.
    Int64,
    /// Unsigned int type.
    UInt32,
    /// Unsigned long type.
    UInt64,
    /// Signed long type.
    SInt32,
    /// Signed long type.
    SInt64,
    /// Unsigned int type, fixed length.
    Fixed32,
    /// Unsigned long type, fixed length.
    Fixed64,
    /// Signed int type, fixed length.
    SFixed32,
    /// Signed long type, fixed length.
    SFixed64,
    /// Boolean type.
    Bool,
    /// String type.
    String,
    /// Bytes type.
    Bytes,
    /// A custom type, it can be a message or an enum.
    Custom(FullIdent<'i>),
}

/// Comments in a protobuf structure.
pub type Comments<'i> = Vec<&'i str>;
/// OPtions in a protobuf structure.
pub type Options<'i> = Vec<(FullIdent<'i>, Constant<'i>)>;

/// A message's field.
#[derive(Debug, Clone, PartialEq)]
pub struct Field<'i> {
    /// Comments of this field.
    pub comments: Comments<'i>,
    /// Key type if this field is a map field.
    pub key_type: Option<FieldType<'i>>,
    /// Type of the value of this field.
    pub type_: FieldType<'i>,
    /// Name of the field.
    pub name: &'i str,
    /// Number of the field.
    pub num: u64,
    /// Options of the field.
    pub options: Options<'i>,
    /// Label of the field, if any.
    pub label: Option<FieldLabel>,
}

/// An `oneof` message field.
#[derive(Debug, Clone, PartialEq)]
pub struct OneOf<'i> {
    /// Comments on the field.
    pub comments: Comments<'i>,
    /// Name of the field.
    pub name: &'i str,
    /// Options of the field.
    pub options: Options<'i>,
    /// Possible fields in this oneof.
    pub fields: Vec<Field<'i>>,
}

/// An protobuf enum.
#[derive(Debug, Clone, PartialEq)]
pub struct Enum<'i> {
    /// Comments on the enum.
    pub comments: Comments<'i>,
    /// Name of the enum.
    pub name: &'i str,
    /// Options of the enum.
    pub options: Options<'i>,
    /// Possible values of the enum.
    pub values: Vec<EnumValue<'i>>,
}

/// An enum value.
#[derive(Debug, Clone, PartialEq)]
pub struct EnumValue<'i> {
    /// Comments on the value.
    pub comments: Comments<'i>,
    /// Name of the value.
    pub name: &'i str,
    /// Integer correspondent of the value.
    pub value: i16,
    /// Options of the value.
    pub options: Options<'i>,
}

/// A protobuf message.
#[derive(Debug, Clone, PartialEq)]
pub struct Message<'i> {
    /// Comments on the message.
    pub comments: Comments<'i>,
    /// Name of the message.
    pub name: &'i str,
    /// Fields in the message.
    ///
    /// These are normal fields or map fields.
    pub fields: Vec<Field<'i>>,
    /// Oneof fields in the message.
    pub oneofs: Vec<OneOf<'i>>,
    /// Enums declared inside this message.
    pub enums: Vec<Enum<'i>>,
    /// Other messages declared inside this messages.
    pub messages: Vec<Message<'i>>,
    /// Options of the message.
    pub options: Options<'i>,
}

/// A RPC method in a [`Service`].
#[derive(Debug, Clone, PartialEq)]
pub struct Rpc<'i> {
    /// Comments in this method.
    pub comments: Comments<'i>,
    /// Name of the method.
    pub name: &'i str,
    /// Request type.
    pub request: FieldType<'i>,
    /// Response type.
    pub response: FieldType<'i>,
    /// Options of this method.
    pub options: Options<'i>,
}

/// A protobuf service interface.
#[derive(Debug, Clone, PartialEq)]
pub struct Service<'i> {
    /// Comments of this service.
    pub comments: Comments<'i>,
    /// Name of the service.
    pub name: &'i str,
    /// RPC methods of the service.
    pub rpcs: Vec<Rpc<'i>>,
    /// Options of the service.
    pub options: Options<'i>,
}

/// A full protobuf file.
#[derive(Debug, Clone, PartialEq)]
pub struct File<'i> {
    /// Initial file comments.
    pub comments: Comments<'i>,
    /// Name of the package where this file is.
    pub package: FullIdent<'i>,
    /// Imports of this file.
    pub imports: Vec<String>,
    /// Options of this file.
    pub options: Options<'i>,
    /// Enums declared in this file root.
    pub enums: Vec<Enum<'i>>,
    /// Messages declared in this file root.
    pub messages: Vec<Message<'i>>,
    /// Service declared in this file.
    pub services: Vec<Service<'i>>,
}

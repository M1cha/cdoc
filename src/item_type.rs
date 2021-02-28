#![allow(dead_code)]

#[derive(Copy, PartialEq, Eq, Clone, Debug, PartialOrd, Ord)]
pub(crate) enum ItemType {
    Struct = 3,
    Enum = 4,
    Function = 5,
    Typedef = 6,
    Static = 7,
    Method = 11,
    StructField = 12,
    Variant = 13,
    Macro = 14,
    Primitive = 15,
    Constant = 17,
    Union = 19,
    Keyword = 21,
    Variable = 100,
    Namespace = 101,
    Class = 102,
}

impl From<&crate::parsed::ItemKind> for ItemType {
    fn from(kind: &crate::parsed::ItemKind) -> ItemType {
        match kind {
            crate::parsed::StructKind(_) => ItemType::Struct,
            crate::parsed::UnionKind(_) => ItemType::Union,
            crate::parsed::EnumKind(_) => ItemType::Enum,
            crate::parsed::FunctionKind(_) => ItemType::Function,
            crate::parsed::TypedefKind(_) => ItemType::Typedef,
            crate::parsed::VariableKind(_) => ItemType::Variable,
            crate::parsed::NamespaceKind(_) => ItemType::Namespace,
        }
    }
}

impl ItemType {
    pub(crate) fn as_str(&self) -> &'static str {
        match *self {
            ItemType::Struct => "struct",
            ItemType::Union => "union",
            ItemType::Enum => "enum",
            ItemType::Function => "fn",
            ItemType::Typedef => "type",
            ItemType::Static => "static",
            ItemType::Method => "method",
            ItemType::StructField => "structfield",
            ItemType::Variant => "variant",
            ItemType::Macro => "macro",
            ItemType::Primitive => "primitive",
            ItemType::Constant => "constant",
            ItemType::Keyword => "keyword",
            ItemType::Variable => "variable",
            ItemType::Namespace => "namespace",
            ItemType::Class => "class",
        }
    }

    pub(crate) fn to_strs(ty: &ItemType) -> (&'static str, &'static str) {
        match *ty {
            ItemType::Struct => ("structs", "Structs"),
            ItemType::Union => ("unions", "Unions"),
            ItemType::Enum => ("enums", "Enums"),
            ItemType::Function => ("functions", "Functions"),
            ItemType::Typedef => ("types", "Type Definitions"),
            ItemType::Static => ("statics", "Statics"),
            ItemType::Constant => ("constants", "Constants"),
            ItemType::Method => ("methods", "Methods"),
            ItemType::StructField => ("fields", "Struct Fields"),
            ItemType::Variant => ("variants", "Variants"),
            ItemType::Macro => ("macros", "Macros"),
            ItemType::Primitive => ("primitives", "Primitive Types"),
            ItemType::Keyword => ("keywords", "Keywords"),
            ItemType::Variable => ("variables", "Variables"),
            ItemType::Namespace => ("namespaces", "Namespaces"),
            ItemType::Class => ("classes", "Classes"),
        }
    }
}

impl std::fmt::Display for ItemType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

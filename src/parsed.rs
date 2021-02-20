pub(crate) use self::ItemKind::*;

#[derive(Debug, PartialEq)]
pub(crate) struct EnumVariant {
    pub(crate) name: String,
}

#[derive(Debug, PartialEq)]
pub(crate) struct Enum {
    pub(crate) variants: Vec<EnumVariant>,
}

#[derive(Debug, PartialEq)]
pub(crate) struct Function {
    pub(crate) ty: String,
}

#[derive(Debug, PartialEq)]
pub(crate) struct StructField {
    pub(crate) name: String,
    pub(crate) ty: String,
}

#[derive(Debug, PartialEq)]
pub(crate) struct Struct {
    pub(crate) fields: Vec<StructField>,
}

#[derive(Debug, PartialEq)]
pub(crate) struct Typedef {
    pub(crate) ty: String,
}

#[derive(Debug, PartialEq)]
pub(crate) struct Union {}

#[derive(Debug, PartialEq)]
pub(crate) struct Variable {
    pub(crate) ty: String,
}

#[derive(Debug, PartialEq)]
pub(crate) enum ItemKind {
    EnumKind(Enum),
    FunctionKind(Function),
    StructKind(Struct),
    TypedefKind(Typedef),
    UnionKind(Union),
    VariableKind(Variable),
}

#[derive(Debug)]
pub(crate) struct Item {
    pub(crate) compilationunit: std::path::PathBuf,
    pub(crate) name: Option<String>,
    pub(crate) comment: Option<String>,
    pub(crate) kind: ItemKind,
}

impl Item {
    pub(crate) fn type_(&self) -> crate::item_type::ItemType {
        (&self.kind).into()
    }
}

#[derive(Default, Debug)]
pub(crate) struct File {
    pub(crate) items: Vec<Item>,
}

#[derive(Default, Debug)]
pub(crate) struct Files {
    pub(crate) list: std::collections::HashMap<std::path::PathBuf, File>,
}

impl Files {
    pub fn for_path<'a, P: AsRef<std::path::Path>>(&'a mut self, path: P) -> &'a mut File {
        let path = path.as_ref();

        if !self.list.contains_key(path) {
            self.list.insert(path.to_path_buf(), File::default());
        }

        self.list.get_mut(path).unwrap()
    }
}

pub(crate) use self::ItemKind::*;

#[derive(Debug, PartialEq)]
pub(crate) struct EnumVariant {
    pub(crate) name: String,
    pub(crate) comment: Option<String>,
}

#[derive(Debug, PartialEq)]
pub(crate) struct Enum {
    pub(crate) variants: Vec<EnumVariant>,
}

impl From<&clang::Entity<'_>> for Enum {
    fn from(entity: &clang::Entity<'_>) -> Self {
        Self {
            variants: entity
                .get_children()
                .iter()
                .filter(|entity| entity.get_kind() == clang::EntityKind::EnumConstantDecl)
                .map(|entity| EnumVariant {
                    name: entity.get_display_name().unwrap(),
                    comment: entity.get_comment(),
                })
                .collect(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct Function {
    pub(crate) ty: String,
}

impl From<&clang::Entity<'_>> for Function {
    fn from(entity: &clang::Entity<'_>) -> Self {
        Self {
            ty: entity.get_type().unwrap().get_display_name(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub(crate) enum FieldType {
    String(String),
    Struct(Struct),
    Union(Union),
}

#[derive(Debug, PartialEq)]
pub(crate) struct Field {
    pub(crate) name: String,
    pub(crate) comment: Option<String>,
    pub(crate) ty: FieldType,
}

#[derive(Debug, PartialEq)]
pub(crate) struct Struct {
    pub(crate) fields: Vec<Field>,
}

fn get_fields(entity: &clang::Entity<'_>) -> Vec<Field> {
    entity
        .get_children()
        .iter()
        .filter(|entity| entity.get_kind() == clang::EntityKind::FieldDecl)
        .map(|entity| Field {
            name: entity.get_display_name().unwrap(),
            comment: entity.get_comment(),
            ty: if let Some(subdecl) = entity.get_child(0) {
                match subdecl.get_kind() {
                    clang::EntityKind::StructDecl => FieldType::Struct(Struct::from(&subdecl)),
                    clang::EntityKind::UnionDecl => FieldType::Union(Union::from(&subdecl)),
                    _ => FieldType::String(entity.get_type().unwrap().get_display_name()),
                }
            } else {
                FieldType::String(entity.get_type().unwrap().get_display_name())
            },
        })
        .collect()
}

impl From<&clang::Entity<'_>> for Struct {
    fn from(entity: &clang::Entity<'_>) -> Self {
        Self {
            fields: get_fields(entity),
        }
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct Typedef {
    pub(crate) ty: String,
}

impl From<&clang::Entity<'_>> for Typedef {
    fn from(entity: &clang::Entity<'_>) -> Self {
        Self {
            ty: entity
                .get_typedef_underlying_type()
                .unwrap()
                .get_display_name(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct Union {
    pub(crate) fields: Vec<Field>,
}

impl From<&clang::Entity<'_>> for Union {
    fn from(entity: &clang::Entity<'_>) -> Self {
        Self {
            fields: get_fields(entity),
        }
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct Variable {
    pub(crate) ty: String,
}

impl From<&clang::Entity<'_>> for Variable {
    fn from(entity: &clang::Entity<'_>) -> Self {
        Self {
            ty: entity.get_type().unwrap().get_display_name(),
        }
    }
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
    pub(crate) code: String,
}

impl PartialEq for Item {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
            && self.comment == other.comment
            && self.kind == other.kind
            && self.code == other.code
    }
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

impl File {
    pub(crate) fn add_item(&mut self, item: Item) {
        if !self.items.contains(&item) {
            self.items.push(item);
        }
    }
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

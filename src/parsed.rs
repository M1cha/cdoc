pub(crate) use self::ItemKind::*;

macro_rules! unwrap_enum {
    ($expression:expr, $ident:ident) => {
        match $expression {
            $ident(v) => v,
            _ => unreachable!(),
        }
    };
}

#[enum_dispatch::enum_dispatch]
trait ItemKindFns {
    /// if the other itemkind is similar enough, extend this item with it's data and return true
    /// Otherwise, return false
    fn try_update(&mut self, other: &ItemKind) -> bool;
}

#[derive(Debug, PartialEq)]
pub(crate) struct EnumVariant {
    pub(crate) name: String,
    pub(crate) comment: Option<String>,
}

#[derive(Debug)]
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

impl ItemKindFns for Enum {
    fn try_update(&mut self, other: &ItemKind) -> bool {
        let other = unwrap_enum!(other, EnumKind);

        self.variants == other.variants
    }
}

#[derive(Debug)]
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

impl ItemKindFns for Function {
    fn try_update(&mut self, other: &ItemKind) -> bool {
        let other = unwrap_enum!(other, FunctionKind);

        if self.ty != other.ty {
            return false;
        }

        true
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

impl ItemKindFns for Struct {
    fn try_update(&mut self, other: &ItemKind) -> bool {
        let other = unwrap_enum!(other, StructKind);
        self.fields == other.fields
    }
}

#[derive(Debug)]
pub(crate) struct Typedef {
    pub(crate) ty: String,
    pub(crate) anonymous_ty: Option<(Box<ItemKind>, String)>,
}

impl From<&clang::Entity<'_>> for Typedef {
    fn from(entity: &clang::Entity<'_>) -> Self {
        let ty = entity.get_typedef_underlying_type().unwrap();

        // store anonymous type behind typedef
        let mut anonymous_ty = None;
        if let Some(elaborated_type) = ty.get_elaborated_type() {
            if let Some(decl) = elaborated_type.get_declaration() {
                if decl.get_name().is_none() {
                    let code = decl.get_pretty_printer().print();
                    anonymous_ty = ItemKind::from_entity(&decl).map(|o| (Box::new(o), code));
                }
            }
        }

        Self {
            ty: ty.get_display_name(),
            anonymous_ty,
        }
    }
}

impl ItemKindFns for Typedef {
    fn try_update(&mut self, other: &ItemKind) -> bool {
        let other = unwrap_enum!(other, TypedefKind);
        self.ty == other.ty
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

impl ItemKindFns for Union {
    fn try_update(&mut self, other: &ItemKind) -> bool {
        let other = unwrap_enum!(other, UnionKind);
        self.fields == other.fields
    }
}

#[derive(Debug)]
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

impl ItemKindFns for Variable {
    fn try_update(&mut self, other: &ItemKind) -> bool {
        let other = unwrap_enum!(other, VariableKind);
        self.ty == other.ty
    }
}

#[enum_dispatch::enum_dispatch(ItemKindFns)]
#[derive(Debug)]
pub(crate) enum ItemKind {
    EnumKind(Enum),
    FunctionKind(Function),
    StructKind(Struct),
    TypedefKind(Typedef),
    UnionKind(Union),
    VariableKind(Variable),
}

impl ItemKind {
    pub(crate) fn from_entity(entity: &clang::Entity<'_>) -> Option<Self> {
        Some(match entity.get_kind() {
            clang::EntityKind::EnumDecl => EnumKind(Enum::from(entity)),
            clang::EntityKind::FunctionDecl => FunctionKind(Function::from(entity)),
            clang::EntityKind::StructDecl => StructKind(Struct::from(entity)),
            clang::EntityKind::TypedefDecl => TypedefKind(Typedef::from(entity)),
            clang::EntityKind::UnionDecl => UnionKind(Union::from(entity)),
            clang::EntityKind::VarDecl => VariableKind(Variable::from(entity)),
            _ => return None,
        })
    }

    pub(crate) fn eq_outer(&self, other: &Self) -> bool {
        match self {
            EnumKind(..) => matches!(other, Self::EnumKind(..)),
            FunctionKind(..) => matches!(other, Self::FunctionKind(..)),
            StructKind(..) => matches!(other, Self::StructKind(..)),
            TypedefKind(..) => matches!(other, Self::TypedefKind(..)),
            UnionKind(..) => matches!(other, Self::UnionKind(..)),
            VariableKind(..) => matches!(other, Self::VariableKind(..)),
        }
    }
}

#[derive(Debug)]
pub(crate) struct Item {
    /// the file that was compiled to obtain this item
    pub(crate) compilationunit: std::sync::Arc<std::path::PathBuf>,
    /// the file that this item was found in
    pub(crate) file: std::path::PathBuf,
    pub(crate) name: Option<String>,
    pub(crate) comment: Option<String>,
    pub(crate) kind: ItemKind,
    pub(crate) code: String,

    pub(crate) duplicates: Vec<Item>,
}

impl Item {
    pub(crate) fn type_(&self) -> crate::item_type::ItemType {
        (&self.kind).into()
    }
}

#[derive(Default, Debug)]
pub(crate) struct ItemList {
    pub(crate) items: Vec<Item>,
}

impl ItemList {
    pub(crate) fn push(&mut self, new_item: Item) {
        for item in &mut self.items {
            if !item.kind.eq_outer(&new_item.kind) {
                continue;
            }
            if item.name != new_item.name {
                continue;
            }

            // check if they are the same. Depending on the implementation this
            // add information from the new one to the old one if they were missing there.
            if item.kind.try_update(&new_item.kind) {
                // sometimes the comment is on the implementation.
                // libclang will give us the documentation on the header item anyway
                // so we can copy that into the public documentation
                if item.comment.is_none() && new_item.comment.is_some() {
                    item.comment = new_item.comment;
                }
                return;
            }

            item.duplicates.push(new_item);
            return;
        }

        self.items.push(new_item);
    }
}

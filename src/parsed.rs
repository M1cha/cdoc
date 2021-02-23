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

impl ItemKindFns for Enum {
    fn try_update(&mut self, other: &ItemKind) -> bool {
        let other = unwrap_enum!(other, EnumKind);

        self.variants == other.variants
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
pub(crate) enum TypeKind {
    Unknown(String),
    Struct(String),
    Union(String),
    Enum(String),
    Anonymous(ItemKind),
    Pointer { isconst: bool, ty: Box<Type> },
    ConstantArray { len: usize, ty: Box<Type> },
}

impl TypeKind {
    pub(crate) fn innermost_anonymous(&self) -> Option<&ItemKind> {
        match self {
            Self::Anonymous(itemkind) => Some(itemkind),
            Self::Pointer { ty, .. } | Self::ConstantArray { ty, .. } => {
                ty.kind.innermost_anonymous()
            }
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct Type {
    //attributes: Vec<>,
    pub(crate) kind: TypeKind,
}

impl From<&clang::Type<'_>> for Type {
    fn from(clangty: &clang::Type<'_>) -> Self {
        Self {
            kind: {
                let tydecl = clangty.get_declaration();
                let is_anonymous = tydecl
                    .as_ref()
                    .map(|tydecl| tydecl.is_anonymous())
                    .unwrap_or(false);
                if is_anonymous {
                    TypeKind::Anonymous(ItemKind::from_entity(&tydecl.unwrap()).unwrap())
                } else {
                    match &clangty.get_kind() {
                        clang::TypeKind::ConstantArray => TypeKind::ConstantArray {
                            len: clangty.get_size().unwrap(),
                            ty: Box::new(Self::from(&clangty.get_element_type().unwrap())),
                        },
                        clang::TypeKind::Pointer => TypeKind::Pointer {
                            isconst: clangty.is_const_qualified(),
                            ty: Box::new(Self::from(&clangty.get_pointee_type().unwrap())),
                        },
                        clang::TypeKind::Elaborated => {
                            TypeKind::Unknown(format!("{:?}", clangty.get_elaborated_type()))
                        }
                        _ => TypeKind::Unknown(format!("{:?}", clangty)),
                    }
                }
            },
        }
    }
}

impl std::fmt::Display for TypeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeKind::Unknown(s) => f.write_str(s),
            TypeKind::Struct(s) => write!(f, "struct {}", s),
            TypeKind::Union(s) => write!(f, "union {}", s),
            TypeKind::Enum(s) => write!(f, "enum {}", s),
            TypeKind::Anonymous(itemkind) => write!(
                f,
                "{}",
                crate::escape::Escape(match itemkind {
                    StructKind(..) => "<<anonymous struct>>",
                    EnumKind(..) => "<<anonymous enum>>",
                    UnionKind(..) => "<<anonymous union>>",
                    _ => "<<unsupported anonymous>>",
                })
            ),
            TypeKind::Pointer { isconst, ty } => {
                write!(f, "{}ptr({})", if *isconst { "const" } else { "" }, ty.kind)
            }
            TypeKind::ConstantArray { len, ty } => write!(f, "[{}; {}]", ty.kind, len),
        }
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct FieldDeclaration {
    pub(crate) name: String,
    pub(crate) comment: Option<String>,
    pub(crate) kind: ItemKind,
}

#[derive(Debug, PartialEq)]
pub(crate) struct Field {
    pub(crate) name: Option<String>,
    pub(crate) comment: Option<String>,
    pub(crate) declarations: Vec<FieldDeclaration>,
    pub(crate) ty: Type,
}

#[derive(Debug, PartialEq)]
pub(crate) struct Struct {
    pub(crate) fields: Vec<Field>,
}

fn get_fields(container_entity: &clang::Entity<'_>) -> Vec<Field> {
    /// we do want to include anonymous entities if they're not part of a named field
    fn is_entity_from_fielddecl(
        container_entity: &clang::Entity<'_>,
        entity: &clang::Entity<'_>,
    ) -> bool {
        container_entity
            .get_children()
            .iter()
            .filter(|e| e.get_kind() == clang::EntityKind::FieldDecl)
            .filter(|e| {
                e.get_children()
                    .iter()
                    .filter(|e| *e == entity)
                    .next()
                    .is_some()
            })
            .next()
            .is_some()
    }

    container_entity
        .get_children()
        .iter()
        .filter_map(|entity| match entity.get_kind() {
            clang::EntityKind::FieldDecl => Some(Field {
                name: entity.get_display_name(),
                comment: entity.get_comment(),
                declarations: entity
                    .get_children()
                    .iter()
                    .filter(|child| child.is_declaration() && !child.is_anonymous())
                    .map(|child| FieldDeclaration {
                        name: child.get_name().unwrap(),
                        comment: child.get_comment(),
                        kind: ItemKind::from_entity(child).unwrap(),
                    })
                    .collect(),
                ty: Type::from(&entity.get_type().unwrap()), /*{
                                                                 let ty = entity.get_type().unwrap();
                                                                 let tydecl = ty.get_declaration();
                                                                 let children = entity.get_children();

                                                                 println!(
                                                                     "name={:?} ty={:?} decl:{:?} anon={:?}",
                                                                     entity.get_display_name(),
                                                                     ty,
                                                                     tydecl,
                                                                     tydecl.map(|d| d.is_anonymous())
                                                                 );

                                                                 Type {
                                                                     kind: TypeKind::Primitive(ty.get_display_name()),
                                                                 }

                                                                 /*let mut kind = None;
                                                                 for child in &children {
                                                                     let newkind = if child.is_declaration() {
                                                                         ItemKind::from_entity(child)
                                                                     } else {
                                                                         match child.get_kind() {
                                                                             //clang::TypeKind
                                                                             _ => None,
                                                                         }
                                                                         //_ => FieldType::String(entity.get_type().unwrap().get_display_name()),
                                                                     };

                                                                     if newkind.is_some() {
                                                                         if kind.is_some() {
                                                                             panic!("a field can't have multiple type kinds");
                                                                         }
                                                                         kind = newkind;
                                                                     }
                                                                 }

                                                                 if let Some(kind) = kind {
                                                                     Type {
                                                                         kind: TypeKind::Anonymous(kind),
                                                                     }
                                                                 } else {
                                                                     panic!("unsupported type: {:#?}", entity);
                                                                 }*/
                                                             },*/
            }),
            _ => {
                if entity.is_declaration() {
                    if is_entity_from_fielddecl(container_entity, entity) {
                        None
                    } else {
                        Some(Field {
                            name: None,
                            comment: entity.get_comment(),
                            declarations: Vec::new(),
                            ty: Type {
                                kind: TypeKind::Anonymous(
                                    if let Some(itemkind) = ItemKind::from_entity(entity) {
                                        itemkind
                                    } else {
                                        panic!("unsupported anonymous type: {:#?}", entity);
                                    },
                                ),
                            },
                        })
                    }
                } else if entity.is_attribute() {
                    None
                } else {
                    unimplemented!("NONFIELD: {:#?}", entity)
                }
            }
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

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub(crate) struct Variable {
    pub(crate) ty: String,
}

impl From<&clang::Entity<'_>> for Variable {
    fn from(entity: &clang::Entity<'_>) -> Self {
        {
            println!("{:?}", entity.get_type());
            Self {
                ty: entity.get_type().unwrap().get_display_name(),
            }
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
#[derive(Debug, PartialEq)]
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

    pub(crate) fn keyword(&self) -> Option<&str> {
        match self {
            StructKind(..) => Some("struct"),
            UnionKind(..) => Some("union"),
            EnumKind(..) => Some("enum"),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq)]
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

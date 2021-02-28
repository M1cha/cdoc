pub(crate) use self::ItemKind::*;

use std::convert::TryInto;
use std::io::BufRead;

macro_rules! unwrap_enum {
    ($expression:expr, $ident:ident) => {
        match $expression {
            $ident(v) => v,
            _ => unreachable!(),
        }
    };
}

fn get_comment(entity: &clang::Entity<'_>) -> Result<Option<String>, crate::Error> {
    let mut comment = entity.get_comment();
    if comment.is_some() {
        let entity_loc = entity.get_location().unwrap().get_presumed_location();
        let comment_range = entity.get_comment_range().unwrap();
        let comment_loc = comment_range.get_end().get_presumed_location();

        if entity_loc.0 != comment_loc.0 {
            //eprintln!("comment is in a different file than the declaration");
            comment = None;
        } else if entity_loc.1 < comment_loc.1 {
            //eprintln!("comment comes after declaration");
            comment = None;
        } else {
            let f = std::fs::File::open(comment_loc.0)?;
            let reader = std::io::BufReader::new(f);
            let line_from: usize = comment_loc.1.try_into()?;
            let line_to: usize = entity_loc.1.try_into()?;

            for (_, line) in reader
                .lines()
                .enumerate()
                .skip(line_from)
                .take_while(|(i, _)| *i < line_to - 1)
            {
                let line = line.as_ref().unwrap().trim();
                if line.starts_with('#') {
                    let name = line[1..].trim();
                    if name.starts_with("define") {
                        //eprintln!("there's a define between the comment and the declaration");
                        comment = None;
                        break;
                    }
                }
            }
        }
    }

    Ok(comment)
}

pub(crate) struct ItemRef {
    path: Vec<(String, crate::ItemType)>,
}

impl From<&clang::Entity<'_>> for ItemRef {
    fn from(entity: &clang::Entity<'_>) -> Self {
        let mut path = Vec::new();

        let mut current = entity.clone();
        loop {
            let kind = current.get_kind();
            match kind {
                clang::EntityKind::TranslationUnit => break,
                _ => {
                    let itemtype = match kind {
                        clang::EntityKind::StructDecl => crate::ItemType::Struct,
                        clang::EntityKind::UnionDecl => crate::ItemType::Union,
                        clang::EntityKind::ClassDecl => crate::ItemType::Class,
                        clang::EntityKind::Namespace => crate::ItemType::Namespace,
                        _ => unimplemented!(),
                    };

                    path.push((current.get_name().unwrap(), itemtype));
                    current = current.get_semantic_parent().unwrap();
                }
            }
        }
        path.reverse();

        Self { path }
    }
}

struct Context {
    pub(crate) compilationunit: std::sync::Arc<std::path::PathBuf>,
    pub(crate) root: Item,
}

impl Context {
    fn parse_children(&mut self, parent_entity: &clang::Entity<'_>) {
        for entity in parent_entity.get_children() {
            // apparently is_anonymous can return false for nameless typedef
            // structs.
            if entity.get_name().is_none() {
                continue;
            }

            if let Some(item) = Item::from_entity(&entity, &self.compilationunit).unwrap() {
                let is_container = item.is_container();
                self.root.kind.push_to_semantic_parent(item, &entity);
                if is_container {
                    self.parse_children(&entity);
                }
            }
        }
    }
}

pub(crate) fn parse_tu(
    tu: &clang::Entity<'_>,
    compilationunit: &std::sync::Arc<std::path::PathBuf>,
) -> Item {
    let mut ctx = Context {
        compilationunit: compilationunit.clone(),
        root: Item::root(compilationunit),
    };

    ctx.parse_children(tu);

    ctx.root
}

#[enum_dispatch::enum_dispatch]
trait ItemKindFns {
    /// if the other itemkind is similar enough, extend this item with it's data and return true
    /// Otherwise, return false
    fn try_update(&mut self, other: &mut ItemKind) -> bool;

    fn items(&self) -> Option<&[Item]> {
        None
    }

    fn items_mut(&mut self) -> Option<&mut Vec<Item>> {
        None
    }
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
                    comment: get_comment(entity).unwrap(),
                })
                .collect(),
        }
    }
}

impl ItemKindFns for Enum {
    fn try_update(&mut self, other: &mut ItemKind) -> bool {
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
    fn try_update(&mut self, other: &mut ItemKind) -> bool {
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
pub(crate) struct Field {
    pub(crate) name: Option<String>,
    //pub(crate) ty: Type,
    pub(crate) ty: String,
    pub(crate) comment: Option<String>,
}

#[derive(Debug, PartialEq)]
pub(crate) struct Struct {
    pub(crate) fields: Vec<Field>,
    pub(crate) items: Vec<Item>,
}

fn get_fields(container_entity: &clang::Entity<'_>) -> Vec<Field> {
    let ty = container_entity.get_type().unwrap();
    let fields = ty.get_fields().unwrap();

    fields
        .iter()
        .map(|entity| Field {
            name: entity.get_display_name(),
            //ty: Type::from(&entity.get_type().unwrap()),
            ty: entity.get_type().unwrap().get_display_name(),
            comment: entity.get_comment(),
        })
        .collect()
}

impl From<&clang::Entity<'_>> for Struct {
    fn from(entity: &clang::Entity<'_>) -> Self {
        Self {
            fields: get_fields(entity),
            items: Vec::new(),
        }
    }
}

impl ItemKindFns for Struct {
    fn try_update(&mut self, other: &mut ItemKind) -> bool {
        let other = unwrap_enum!(other, StructKind);
        self.fields == other.fields
    }

    fn items(&self) -> Option<&[Item]> {
        Some(&self.items)
    }

    fn items_mut(&mut self) -> Option<&mut Vec<Item>> {
        Some(&mut self.items)
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
    fn try_update(&mut self, other: &mut ItemKind) -> bool {
        let other = unwrap_enum!(other, TypedefKind);
        self.ty == other.ty
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct Union {
    pub(crate) fields: Vec<Field>,
    pub(crate) items: Vec<Item>,
}

impl From<&clang::Entity<'_>> for Union {
    fn from(entity: &clang::Entity<'_>) -> Self {
        Self {
            fields: get_fields(entity),
            items: Vec::new(),
        }
    }
}

impl ItemKindFns for Union {
    fn try_update(&mut self, other: &mut ItemKind) -> bool {
        let other = unwrap_enum!(other, UnionKind);
        self.fields == other.fields
    }

    fn items(&self) -> Option<&[Item]> {
        Some(&self.items)
    }

    fn items_mut(&mut self) -> Option<&mut Vec<Item>> {
        Some(&mut self.items)
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct Variable {
    pub(crate) ty: String,
}

impl From<&clang::Entity<'_>> for Variable {
    fn from(entity: &clang::Entity<'_>) -> Self {
        {
            Self {
                ty: entity.get_type().unwrap().get_display_name(),
            }
        }
    }
}

impl ItemKindFns for Variable {
    fn try_update(&mut self, other: &mut ItemKind) -> bool {
        let other = unwrap_enum!(other, VariableKind);
        self.ty == other.ty
    }
}

#[derive(Debug, Default, PartialEq)]
pub(crate) struct Namespace {
    items: Vec<Item>,
}

impl ItemKindFns for Namespace {
    fn try_update(&mut self, other: &mut ItemKind) -> bool {
        let other = unwrap_enum!(other, NamespaceKind);
        for item in other.items.drain(..) {
            self.items.push(item);
        }
        true
    }

    fn items(&self) -> Option<&[Item]> {
        Some(&self.items)
    }

    fn items_mut(&mut self) -> Option<&mut Vec<Item>> {
        Some(&mut self.items)
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
    NamespaceKind(Namespace),
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
            NamespaceKind(..) => matches!(other, Self::NamespaceKind(..)),
        }
    }

    pub(crate) fn eq_clangkind(&self, other: &clang::EntityKind) -> bool {
        match self {
            EnumKind(..) => matches!(other, clang::EntityKind::EnumDecl),
            FunctionKind(..) => matches!(other, clang::EntityKind::FunctionDecl),
            StructKind(..) => matches!(other, clang::EntityKind::StructDecl),
            TypedefKind(..) => matches!(other, clang::EntityKind::TypedefDecl),
            UnionKind(..) => matches!(other, clang::EntityKind::UnionDecl),
            VariableKind(..) => matches!(other, clang::EntityKind::VarDecl),
            NamespaceKind(..) => matches!(other, clang::EntityKind::Namespace),
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

    /// pushes a new item to the kind if it supports children
    /// if it exists, either merge them or push it as a duplicate.
    /// returns the index of the new item or existing item ignoring duplicates
    fn push_deduplicate(&mut self, mut new_item: Item) -> usize {
        let items = self.items_mut().unwrap();
        for (index, item) in items.iter_mut().enumerate() {
            if !item.kind.eq_outer(&new_item.kind) {
                continue;
            }
            if item.name != new_item.name {
                continue;
            }

            // check if they are the same. Depending on the implementation this
            // add information from the new one to the old one if they were missing there.
            if item.kind.try_update(&mut new_item.kind) {
                // sometimes the comment is on the implementation.
                // libclang will give us the documentation on the header item anyway
                // so we can copy that into the public documentation
                if item.comment.is_none() && new_item.comment.is_some() {
                    item.comment = new_item.comment;
                }
                return index;
            }

            item.duplicates.push(new_item);
            return index;
        }

        items.push(new_item);
        items.len() - 1
    }

    /// push the new item to the given relative path
    fn push_to_path(&mut self, new_item: Item, path: &[(String, clang::Entity<'_>)]) {
        if let Some((name, entity)) = path.first() {
            if let Some(item) =
                self.items_mut().unwrap().iter_mut().find(|e| {
                    e.kind.eq_clangkind(&entity.get_kind()) && e.name.as_ref() == Some(name)
                })
            {
                item.kind.push_to_path(new_item, &path[1..]);
            } else {
                let index = self.push_deduplicate(
                    Item::from_entity(entity, &new_item.compilationunit)
                        .unwrap()
                        .unwrap(),
                );

                self.items_mut().unwrap()[index]
                    .kind
                    .push_to_path(new_item, &path[1..]);
            }
        } else {
            self.push_deduplicate(new_item);
        }
    }

    /// under the assumption that this itemkind dis the root namespace,
    /// push it to the semantic path creating missing parents along the way
    pub(crate) fn push_to_semantic_parent(
        &mut self,
        new_item: Item,
        new_entity: &clang::Entity<'_>,
    ) {
        let mut path = Vec::new();
        {
            let mut current = new_entity.get_semantic_parent().unwrap();
            loop {
                let kind = current.get_kind();
                match kind {
                    clang::EntityKind::TranslationUnit => break,
                    _ => {
                        path.push((current.get_name().unwrap(), current.clone()));
                        current = current.get_semantic_parent().unwrap();
                    }
                }
            }
            path.reverse();
        }
        self.push_to_path(new_item, &path);
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

    pub(crate) duplicates: Vec<Item>,
}

impl Item {
    pub(crate) fn root(compilationunit: &std::sync::Arc<std::path::PathBuf>) -> Item {
        Item {
            compilationunit: compilationunit.clone(),
            file: std::path::PathBuf::new(),
            name: None,
            comment: None,
            kind: ItemKind::NamespaceKind(Namespace::default()),
            duplicates: Vec::new(),
        }
    }

    pub(crate) fn from_entity(
        entity: &clang::Entity<'_>,
        compilationunit: &std::sync::Arc<std::path::PathBuf>,
    ) -> Result<Option<Self>, crate::Error> {
        if !entity.is_declaration() {
            return Ok(None);
        }
        if entity.is_anonymous() {
            return Ok(None);
        }

        let loc = if let Some(loc) = entity.get_location().map(|loc| loc.get_presumed_location()) {
            loc
        } else {
            return Ok(None);
        };

        let abspath = std::fs::canonicalize(loc.0)?;
        if abspath.extension() != Some(std::ffi::OsStr::new("h")) {
            // TODO: maybe we wnat to remove this to extract documentation from source files
            return Ok(None);
        }

        if entity != &entity.get_canonical_entity() {
            return Ok(None);
        }

        let kind = match entity.get_kind() {
            clang::EntityKind::EnumDecl => EnumKind(Enum::from(entity)),
            clang::EntityKind::FunctionDecl => FunctionKind(Function::from(entity)),
            clang::EntityKind::StructDecl => StructKind(Struct::from(entity)),
            clang::EntityKind::TypedefDecl => TypedefKind(Typedef::from(entity)),
            clang::EntityKind::UnionDecl => UnionKind(Union::from(entity)),
            clang::EntityKind::VarDecl => VariableKind(Variable::from(entity)),
            clang::EntityKind::Namespace => NamespaceKind(Namespace::default()),
            _ => return Ok(None),
        };

        Ok(Some(Item {
            compilationunit: compilationunit.clone(),
            file: abspath,
            name: entity.get_name(),
            comment: get_comment(entity)?,
            kind,
            duplicates: Vec::new(),
        }))
    }

    pub(crate) fn type_(&self) -> crate::item_type::ItemType {
        (&self.kind).into()
    }

    pub(crate) fn items(&self) -> Option<&[Item]> {
        self.kind.items()
    }

    pub(crate) fn extend(&mut self, mut other: Item) {
        for item in other.kind.items_mut().unwrap().drain(..) {
            self.kind.push_deduplicate(item);
        }
    }

    pub(crate) fn is_container(&self) -> bool {
        self.items().is_some()
    }
}

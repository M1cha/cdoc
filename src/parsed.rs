pub(crate) use self::ItemKind::*;

use crate::Error;
use std::convert::TryInto;
use std::fmt::Write;
use std::io::BufRead;

macro_rules! unwrap_enum {
    ($expression:expr, $ident:ident) => {
        match $expression {
            $ident(v) => v,
            _ => unreachable!(),
        }
    };
}

fn get_comment(entity: &clang::Entity<'_>) -> Result<Option<String>, Error> {
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

#[derive(Debug, PartialEq)]
pub(crate) struct ItemRef {
    pub(crate) path: Vec<(String, crate::ItemType)>,
    pub(crate) subref: Option<String>,
}

impl ItemRef {
    fn from_entity(entity: &clang::Entity<'_>) -> Option<Self> {
        let mut path = Vec::new();
        let mut subref = None;

        let mut current = entity.clone();

        if current.get_kind() == clang::EntityKind::EnumConstantDecl {
            subref = Some(current.get_name().unwrap());
            current = current.get_semantic_parent()?;
        }

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
                        clang::EntityKind::EnumDecl => crate::ItemType::Enum,
                        clang::EntityKind::TypedefDecl => crate::ItemType::Typedef,
                        _ => unimplemented!("{:?}", kind),
                    };

                    path.push((current.get_name()?, itemtype));
                    current = current.get_semantic_parent()?;
                }
            }
        }
        path.reverse();

        Some(Self { path, subref })
    }
}

struct Context {
    pub(crate) compilationunit: std::sync::Arc<std::path::PathBuf>,
    pub(crate) root: Item,
}

impl Context {
    fn parse_children(&mut self, parent_entity: &clang::Entity<'_>) -> Result<(), Error> {
        for entity in parent_entity.get_children() {
            // apparently is_anonymous can return false for nameless typedef
            // structs.
            if entity.is_anonymous() || entity.get_name().is_none() {
                // we still want to scan the children because e.g. an anonymous
                // struct could contain a named struct whose semantic parent is
                // the translation unit
                self.parse_children(&entity)?;
                continue;
            }

            if let Some(item) = Item::from_entity(&entity, &self.compilationunit)? {
                let is_container = item.is_container();
                match self.root.kind.push_to_semantic_parent(item, &entity) {
                    // with C++ it is possible to define unreachable types,
                    // so just skip them.
                    Err(Error::CantResolveEntityPath) => (),
                    Err(e) => return Err(e),
                    Ok(()) => (),
                }
                if is_container {
                    self.parse_children(&entity)?;
                }
            }
        }
        Ok(())
    }
}

pub(crate) fn parse_tu(
    tu: &clang::Entity<'_>,
    compilationunit: &std::sync::Arc<std::path::PathBuf>,
) -> Result<Item, Error> {
    let mut ctx = Context {
        compilationunit: compilationunit.clone(),
        root: Item::root(compilationunit),
    };

    ctx.parse_children(tu)?;

    Ok(ctx.root)
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

    fn outertype(&self) -> Option<&Type> {
        None
    }
}

#[derive(Debug, PartialEq)]
pub(crate) enum EnumValue {
    Signed(i64),
    Unsigned(u64),
}

impl std::fmt::Display for EnumValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Self::Signed(n) => n.fmt(f),
            Self::Unsigned(n) => n.fmt(f),
        }
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct EnumVariant {
    pub(crate) name: String,
    pub(crate) comment: Option<String>,
    pub(crate) value: Option<EnumValue>,
}

#[derive(Debug, PartialEq)]
pub(crate) struct Enum {
    pub(crate) variants: Vec<EnumVariant>,
    pub(crate) outerty: Box<Type>,
}

impl Enum {
    fn from_entity(
        entity: &clang::Entity<'_>,
        compilationunit: &std::sync::Arc<std::path::PathBuf>,
    ) -> Result<Self, Error> {
        let mut variants = Vec::new();
        for variant_entity in entity
            .get_children()
            .iter()
            .filter(|entity| entity.get_kind() == clang::EntityKind::EnumConstantDecl)
        {
            variants.push(EnumVariant {
                name: variant_entity.get_display_name().unwrap(),
                comment: get_comment(variant_entity)?,
                value: variant_entity
                    .get_enum_constant_value()
                    .map(|(signed, unsigned)| {
                        if entity
                            .get_enum_underlying_type()
                            .unwrap()
                            .is_signed_integer()
                        {
                            EnumValue::Signed(signed)
                        } else {
                            EnumValue::Unsigned(unsigned)
                        }
                    }),
            })
        }

        Ok(Self {
            variants,
            outerty: Box::new(Type::from_entity(entity, compilationunit)?),
        })
    }
}

impl ItemKindFns for Enum {
    fn try_update(&mut self, other: &mut ItemKind) -> bool {
        let other = unwrap_enum!(other, EnumKind);

        self.variants == other.variants
    }

    fn outertype(&self) -> Option<&Type> {
        Some(&self.outerty)
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct Function {
    pub(crate) arguments: Vec<(Option<String>, Type)>,
    pub(crate) result: Box<Type>,
    pub(crate) is_variadic: bool,
    pub(crate) outerty: Box<Type>,
}

impl Function {
    fn from_entity(
        entity: &clang::Entity<'_>,
        compilationunit: &std::sync::Arc<std::path::PathBuf>,
    ) -> Result<Self, Error> {
        let mut arguments = Vec::new();
        for arg_entity in entity.get_arguments().unwrap() {
            arguments.push((
                arg_entity.get_name(),
                Type::from_clangtype(&arg_entity.get_type().unwrap(), compilationunit)?,
            ));
        }

        Ok(Self {
            arguments,
            result: Box::new(Type::from_clangtype(
                &entity.get_result_type().unwrap(),
                compilationunit,
            )?),
            is_variadic: entity.is_variadic(),
            outerty: Box::new(Type::from_entity(entity, compilationunit)?),
        })
    }
}

impl ItemKindFns for Function {
    fn try_update(&mut self, other: &mut ItemKind) -> bool {
        let other = unwrap_enum!(other, FunctionKind);

        self == other
    }

    fn outertype(&self) -> Option<&Type> {
        Some(&self.outerty)
    }
}

#[derive(Debug)]
enum PrintResultType<'tu> {
    Type(clang::Type<'tu>),
    DeclRef(clang::Entity<'tu>),
}

#[derive(Debug, Default)]
struct PrintResult<'tu> {
    items: Vec<PrintResultType<'tu>>,
}

#[derive(Debug, Default)]
struct PrintCallbacks<'tu>(std::rc::Rc<std::cell::RefCell<PrintResult<'tu>>>);

impl<'tu> PrintCallbacks<'tu> {
    fn handle_common_start(&self, out: &mut clang::ClangOutputStream, pr: PrintResultType<'tu>) {
        let mut res = self.0.borrow_mut();

        out.write_byte(0x1b);
        write!(out, "{}", res.items.len()).unwrap();
        out.write_byte(0x1b);

        res.items.push(pr);
    }
}

impl<'tu> clang::PrintingPolicyCallback<'tu> for PrintCallbacks<'tu> {
    fn handle_type(&self, out: &mut clang::ClangOutputStream, ty: &'tu clang::Type, end: bool) {
        if end {
            out.write_byte(0x1b);
        } else {
            self.handle_common_start(out, PrintResultType::Type(*ty));
        }
    }

    fn handle_declref(
        &self,
        out: &mut clang::ClangOutputStream,
        entity: &'tu clang::Entity,
        end: bool,
    ) {
        if end {
            out.write_byte(0x1b);
        } else {
            self.handle_common_start(out, PrintResultType::DeclRef(*entity));
        }
    }
}

pub(crate) trait TypeFormatter {
    fn fmt_item(&self, f: &mut std::fmt::Formatter, itemref: &ItemRef) -> std::fmt::Result;
    fn fmt_anonymous_item(&self, f: &mut std::fmt::Formatter, item: &Item) -> std::fmt::Result;
}

#[derive(Debug, PartialEq)]
enum TypeSegment {
    String(String),
    ItemRef(ItemRef),
    Anonymous(Item),
}

#[derive(Debug, PartialEq)]
pub(crate) struct Type {
    segments: Vec<TypeSegment>,
}

impl Type {
    pub(crate) fn iter_anonymous(&self) -> impl std::iter::Iterator<Item = &Item> + '_ {
        self.segments.iter().filter_map(|seg| match seg {
            TypeSegment::Anonymous(item) => Some(item),
            _ => None,
        })
    }

    fn from_prettyprinter(
        entity: Option<&clang::Entity<'_>>,
        mut pp: clang::PrettyPrinter,
        compilationunit: &std::sync::Arc<std::path::PathBuf>,
    ) -> Result<Self, Error> {
        let cb = PrintCallbacks::default();
        let res = cb.0.clone();
        pp.set_callbacks(Some(cb));

        let s = pp.print();
        let res = res.borrow();

        let mut segments = Vec::new();
        let mut split = s.split("\x1b");
        segments.push(TypeSegment::String(
            split.next().ok_or(Error::TypeParseError)?.to_string(),
        ));
        while let Some(id) = split.next() {
            let id: usize = id.parse()?;
            let name = split.next().ok_or(Error::TypeParseError)?;
            let item = &res.items[id];

            segments.push(match item {
                PrintResultType::Type(clangty) => clangty
                    .get_declaration()
                    .map_or_else(
                        || match clangty.get_kind() {
                            clang::TypeKind::Void
                            | clang::TypeKind::Bool
                            | clang::TypeKind::CharS
                            | clang::TypeKind::CharU
                            | clang::TypeKind::SChar
                            | clang::TypeKind::UChar
                            | clang::TypeKind::Char16
                            | clang::TypeKind::Char32
                            | clang::TypeKind::Short
                            | clang::TypeKind::UShort
                            | clang::TypeKind::Int
                            | clang::TypeKind::UInt
                            | clang::TypeKind::Long
                            | clang::TypeKind::ULong
                            | clang::TypeKind::LongLong
                            | clang::TypeKind::ULongLong
                            | clang::TypeKind::Int128
                            | clang::TypeKind::UInt128
                            | clang::TypeKind::Float
                            | clang::TypeKind::Double
                            | clang::TypeKind::Nullptr
                            | clang::TypeKind::Float128 => Some(TypeSegment::ItemRef(ItemRef {
                                path: vec![(name.to_string(), crate::ItemType::Primitive)],
                                subref: None,
                            })),
                            _ => None,
                        },
                        |decl| {
                            if decl.is_anonymous() {
                                Some(TypeSegment::Anonymous(
                                    Item::from_entity(&decl, compilationunit).unwrap()?,
                                ))
                            } else if entity == Some(&decl) {
                                None
                            } else {
                                Some(TypeSegment::ItemRef(ItemRef::from_entity(&decl)?))
                            }
                        },
                    )
                    .unwrap_or_else(|| TypeSegment::String(name.to_string())),
                PrintResultType::DeclRef(entity) => ItemRef::from_entity(entity)
                    .map(|itemref| TypeSegment::ItemRef(itemref))
                    .unwrap_or_else(|| TypeSegment::String(name.to_string())),
            });

            segments.push(TypeSegment::String(
                split.next().ok_or(Error::TypeParseError)?.to_string(),
            ));
        }

        Ok(Self { segments })
    }

    pub(crate) fn from_clangtype(
        clangty: &clang::Type<'_>,
        compilationunit: &std::sync::Arc<std::path::PathBuf>,
    ) -> Result<Self, Error> {
        Self::from_prettyprinter(None, clangty.get_pretty_printer(), compilationunit)
    }

    pub(crate) fn from_entity(
        entity: &clang::Entity<'_>,
        compilationunit: &std::sync::Arc<std::path::PathBuf>,
    ) -> Result<Self, Error> {
        let pp = entity.get_pretty_printer();

        // by default, clang does not print anonymous declarations of typedefs
        if entity.get_kind() == clang::EntityKind::TypedefDecl {
            if let Some(ela) = entity
                .get_typedef_underlying_type()
                .unwrap()
                .get_elaborated_type()
            {
                if ela.get_declaration().unwrap().get_name().is_none() {
                    pp.set_flag(clang::PrintingPolicyFlag::IncludeTagDefinition, true);
                }
            }
        }

        Self::from_prettyprinter(Some(entity), pp, compilationunit)
    }

    pub(crate) fn fmt<F: TypeFormatter>(
        &self,
        f: &mut std::fmt::Formatter,
        tf: &F,
    ) -> std::fmt::Result {
        for segment in &self.segments {
            match segment {
                TypeSegment::String(s) => f.write_str(s)?,
                TypeSegment::ItemRef(itemref) => tf.fmt_item(f, itemref)?,
                TypeSegment::Anonymous(item) => tf.fmt_anonymous_item(f, item)?,
            }
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct Field {
    pub(crate) name: Option<String>,
    pub(crate) ty: Type,
    pub(crate) comment: Option<String>,
}

#[derive(Debug, PartialEq)]
pub(crate) struct Struct {
    pub(crate) fields: Vec<Field>,
    pub(crate) items: Vec<Item>,
    pub(crate) outerty: Type,
}

fn get_fields(
    container_entity: &clang::Entity<'_>,
    compilationunit: &std::sync::Arc<std::path::PathBuf>,
) -> Result<Vec<Field>, Error> {
    let ty = container_entity.get_type().unwrap();
    let fields = ty.get_fields().unwrap();

    let mut v = Vec::new();
    for entity in &fields {
        v.push(Field {
            name: entity.get_display_name(),
            ty: Type::from_clangtype(&entity.get_type().unwrap(), compilationunit)?,
            comment: entity.get_comment(),
        });
    }

    Ok(v)
}

impl Struct {
    fn from_entity(
        entity: &clang::Entity<'_>,
        compilationunit: &std::sync::Arc<std::path::PathBuf>,
    ) -> Result<Self, Error> {
        Ok(Self {
            fields: get_fields(entity, compilationunit)?,
            items: Vec::new(),
            outerty: Type::from_entity(entity, compilationunit)?,
        })
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

    fn outertype(&self) -> Option<&Type> {
        Some(&self.outerty)
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct Typedef {
    pub(crate) outerty: Box<Type>,
}

impl Typedef {
    fn from_entity(
        entity: &clang::Entity<'_>,
        compilationunit: &std::sync::Arc<std::path::PathBuf>,
    ) -> Result<Self, Error> {
        let _ty = entity.get_typedef_underlying_type().unwrap();
        Ok(Self {
            outerty: Box::new(Type::from_entity(entity, compilationunit)?),
        })
    }
}

impl ItemKindFns for Typedef {
    fn try_update(&mut self, other: &mut ItemKind) -> bool {
        let other = unwrap_enum!(other, TypedefKind);
        self.outerty == other.outerty
    }

    fn outertype(&self) -> Option<&Type> {
        Some(&self.outerty)
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct Union {
    pub(crate) fields: Vec<Field>,
    pub(crate) items: Vec<Item>,
    pub(crate) outerty: Box<Type>,
}

impl Union {
    fn from_entity(
        entity: &clang::Entity<'_>,
        compilationunit: &std::sync::Arc<std::path::PathBuf>,
    ) -> Result<Self, Error> {
        Ok(Self {
            fields: get_fields(entity, compilationunit)?,
            items: Vec::new(),
            outerty: Box::new(Type::from_entity(entity, compilationunit)?),
        })
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

    fn outertype(&self) -> Option<&Type> {
        Some(&self.outerty)
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct Variable {
    pub(crate) outerty: Box<Type>,
}

impl Variable {
    fn from_entity(
        entity: &clang::Entity<'_>,
        compilationunit: &std::sync::Arc<std::path::PathBuf>,
    ) -> Result<Self, Error> {
        Ok(Self {
            outerty: Box::new(Type::from_entity(entity, compilationunit)?),
        })
    }
}

impl ItemKindFns for Variable {
    fn try_update(&mut self, other: &mut ItemKind) -> bool {
        let other = unwrap_enum!(other, VariableKind);
        self.outerty == other.outerty
    }

    fn outertype(&self) -> Option<&Type> {
        Some(&self.outerty)
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

    /// pushes a new item to the kind if it supports children
    /// if it exists, either merge them or push it as a duplicate.
    /// returns the index of the new item or existing item ignoring duplicates
    fn push_deduplicate(&mut self, mut new_item: Item) -> usize {
        let items = self.items_mut().unwrap();
        for (index, item) in items.iter_mut().enumerate() {
            if item.name != new_item.name {
                continue;
            }

            if item.kind.eq_outer(&new_item.kind) {
                // check if they are the same. Depending on the implementation this can
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

                // some compilation units might have never seen the definition
                // so overwrite the non definition if we have one now.
                // we only do this if the itemkind is the same to not loose
                // duplicates with different kinds.
                if !item.is_definition {
                    let comment = item.comment.take();
                    *item = new_item;

                    // in case the declaration has a comment and the definition doesn't
                    if item.comment.is_none() && comment.is_some() {
                        item.comment = comment;
                    }
                    return index;
                }
            }

            item.duplicates.push(new_item);
            return index;
        }

        items.push(new_item);
        items.len() - 1
    }

    /// push the new item to the given relative path
    fn push_to_path(
        &mut self,
        new_item: Item,
        path: &[(String, clang::Entity<'_>)],
    ) -> Result<(), Error> {
        if let Some((name, entity)) = path.first() {
            if let Some(item) =
                self.items_mut().unwrap().iter_mut().find(|e| {
                    e.kind.eq_clangkind(&entity.get_kind()) && e.name.as_ref() == Some(name)
                })
            {
                item.kind.push_to_path(new_item, &path[1..])?;
            } else {
                let index = self.push_deduplicate(
                    Item::from_entity(entity, &new_item.compilationunit)?.unwrap(),
                );

                self.items_mut().unwrap()[index]
                    .kind
                    .push_to_path(new_item, &path[1..])?;
            }
        } else {
            self.push_deduplicate(new_item);
        }

        Ok(())
    }

    /// under the assumption that this itemkind is the root namespace,
    /// push it to the semantic path creating missing parents along the way
    pub(crate) fn push_to_semantic_parent(
        &mut self,
        new_item: Item,
        new_entity: &clang::Entity<'_>,
    ) -> Result<(), Error> {
        let mut path = Vec::new();
        {
            let mut current = new_entity
                .get_semantic_parent()
                .ok_or(Error::CantResolveEntityPath)?;
            loop {
                let kind = current.get_kind();
                match kind {
                    clang::EntityKind::TranslationUnit => break,
                    _ => {
                        path.push((
                            current.get_name().ok_or(Error::CantResolveEntityPath)?,
                            current.clone(),
                        ));
                        current = current
                            .get_semantic_parent()
                            .ok_or(Error::CantResolveEntityPath)?;
                    }
                }
            }
            path.reverse();
        }
        self.push_to_path(new_item, &path)?;

        Ok(())
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
    pub(crate) is_definition: bool,

    pub(crate) duplicates: Vec<Item>,
}

impl Item {
    pub(crate) fn root(compilationunit: &std::sync::Arc<std::path::PathBuf>) -> Item {
        Item {
            compilationunit: compilationunit.clone(),
            file: std::path::PathBuf::new(),
            name: Some("<<global>>".to_string()),
            comment: None,
            kind: ItemKind::NamespaceKind(Namespace::default()),
            is_definition: true,
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

        let loc = if let Some(loc) = entity.get_location().map(|loc| loc.get_presumed_location()) {
            loc
        } else {
            return Ok(None);
        };

        let abspath = std::fs::canonicalize(loc.0)?;
        if abspath.extension() != Some(std::ffi::OsStr::new("h")) {
            // TODO: maybe we want to remove this to extract documentation from source files
            return Ok(None);
        }

        // wait for the definition
        // clang only considers the first comment anyway so we don't have to merge them either
        if !entity.is_definition() && entity.get_definition().is_some() {
            return Ok(None);
        }

        let kind = match entity.get_kind() {
            clang::EntityKind::EnumDecl => EnumKind(Enum::from_entity(entity, compilationunit)?),
            clang::EntityKind::FunctionDecl => {
                FunctionKind(Function::from_entity(entity, compilationunit)?)
            }
            clang::EntityKind::StructDecl => {
                StructKind(Struct::from_entity(entity, compilationunit)?)
            }
            clang::EntityKind::TypedefDecl => {
                TypedefKind(Typedef::from_entity(entity, compilationunit)?)
            }
            clang::EntityKind::UnionDecl => UnionKind(Union::from_entity(entity, compilationunit)?),
            clang::EntityKind::VarDecl => {
                VariableKind(Variable::from_entity(entity, compilationunit)?)
            }
            clang::EntityKind::Namespace => NamespaceKind(Namespace::default()),
            _ => return Ok(None),
        };

        Ok(Some(Item {
            compilationunit: compilationunit.clone(),
            file: abspath,
            name: entity.get_name(),
            comment: get_comment(entity)?,
            kind,
            is_definition: entity.is_definition(),
            duplicates: Vec::new(),
        }))
    }

    pub(crate) fn type_(&self) -> crate::item_type::ItemType {
        (&self.kind).into()
    }

    pub(crate) fn items(&self) -> Option<&[Item]> {
        self.kind.items()
    }

    pub(crate) fn outertype(&self) -> Option<&Type> {
        self.kind.outertype()
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

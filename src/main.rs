// TODO: the same function might be declared twice - one inline and one normally.
//       ignore the non-inline one.
// TODO: function forward declarations might have additional attributes, merge
// TODO: consider TypeRefs

mod escape;
mod format;
mod item_type;
mod layout;
mod parsed;
mod static_files;

use item_type::ItemType;
use itertools::Itertools;
use rayon::prelude::*;
use std::io::Write as _;

#[derive(Debug, thiserror::Error)]
enum Error {
    #[error(transparent)]
    Io(#[from] std::io::Error),
    #[error(transparent)]
    Fmt(#[from] std::fmt::Error),
    #[error(transparent)]
    Json(#[from] serde_json::Error),
    #[error(transparent)]
    TryFromIntError(#[from] std::num::TryFromIntError),
    #[error(transparent)]
    ParseIntError(#[from] std::num::ParseIntError),

    #[error("exit status: {0}")]
    ExitStatus(std::process::ExitStatus),
    #[error("can't resolve entity path")]
    CantResolveEntityPath,
    #[error("type parsing error")]
    TypeParseError,
}

#[derive(Debug, serde::Deserialize)]
struct CompileCommand {
    directory: std::path::PathBuf,
    command: String,
    file: std::path::PathBuf,
}

fn load_cdb<P: AsRef<std::path::Path>>(path: P) -> Result<Vec<CompileCommand>, Error> {
    let file = std::fs::File::open(path)?;
    let reader = std::io::BufReader::new(file);
    let cdb: Vec<CompileCommand> = serde_json::from_reader(reader)?;
    Ok(cdb)
}

fn process_file(command: &CompileCommand, clang: &clang::Clang) -> Result<parsed::Item, Error> {
    std::env::set_current_dir(&command.directory)?;
    let compilationunit = std::sync::Arc::new(std::fs::canonicalize(&command.file)?);
    let outfile = tempfile::Builder::new()
        .suffix(&format!(
            ".{}",
            command.file.extension().unwrap().to_str().unwrap()
        ))
        .tempfile()?;

    // TODO: use argument parser which supports quoting
    let mut args: Vec<_> = command.command.split_whitespace().collect();

    // disable object creation
    args.retain(|&arg| arg != "-c");

    // remove output files
    while let Ok(index) = args.binary_search(&"-o") {
        args.remove(index);
        args.remove(index);
    }

    // preprocessor only
    args.push("-E");
    // retain comments
    args.push("-C");

    args.push("-o");
    args.push(outfile.path().to_str().unwrap());

    //println!("ARGS: {:?}", args);
    let status = std::process::Command::new(args[0])
        .args(&args[1..])
        .status()?;
    if !status.success() {
        return Err(Error::ExitStatus(status));
    }

    let index = clang::Index::new(&clang, false, true);
    let tu = index
        .parser(outfile.path())
        .skip_function_bodies(true)
        .arguments(&[
            // TODO: detect target
            "-target",
            "armv7a",
            // disable static assertions because they might fail
            "-D_Static_assert(...)=",
            // remove all clang functions and include directories
            "-fno-builtin",
            "-nostdinc",
            "-ffreestanding",
            //"-xc++",
        ])
        .parse()
        .unwrap();

    parsed::parse_tu(&tu.get_entity(), &compilationunit)
}

fn open_file<P: AsRef<std::path::Path>>(dst: P) -> Result<std::fs::File, Error> {
    std::fs::create_dir_all(dst.as_ref().parent().unwrap())?;
    Ok(std::fs::File::create(dst)?)
}

fn write<P: AsRef<std::path::Path>>(dst: P, contents: &[u8]) -> Result<(), Error> {
    let mut file = open_file(dst)?;
    file.write_all(contents)?;
    Ok(())
}

fn write_minify<P: AsRef<std::path::Path>>(dst: P, contents: &str) -> Result<(), Error> {
    write(dst, contents.as_bytes())
}

fn copy_statics<P: AsRef<std::path::Path>>(dst: P) -> Result<(), Error> {
    let dst = dst.as_ref();

    write_minify(dst.join("rustdoc.css"), static_files::RUSTDOC_CSS)?;
    write_minify(dst.join("settings.css"), static_files::SETTINGS_CSS)?;
    write_minify(dst.join("noscript.css"), static_files::NOSCRIPT_CSS)?;

    // TODO: theme selection
    let themes = vec!["light"];
    write_minify(dst.join("light.css"), static_files::themes::LIGHT)?;

    // TODO: customization
    write(dst.join("rust-logo.png"), static_files::RUST_LOGO)?;
    write(dst.join("favicon.svg"), static_files::RUST_FAVICON_SVG)?;
    write(
        dst.join("favicon-16x16.png"),
        static_files::RUST_FAVICON_PNG_16,
    )?;
    write(
        dst.join("favicon-32x32.png"),
        static_files::RUST_FAVICON_PNG_32,
    )?;

    write(dst.join("brush.svg"), static_files::BRUSH_SVG)?;
    write(dst.join("wheel.svg"), static_files::WHEEL_SVG)?;
    write(dst.join("down-arrow.svg"), static_files::DOWN_ARROW_SVG)?;

    // To avoid theme switch latencies as much as possible, we put everything theme related
    // at the beginning of the html files into another js file.
    let theme_js = format!(
        r#"var themes = document.getElementById("theme-choices");
var themePicker = document.getElementById("theme-picker");

function showThemeButtonState() {{
    themes.style.display = "block";
    themePicker.style.borderBottomRightRadius = "0";
    themePicker.style.borderBottomLeftRadius = "0";
}}

function hideThemeButtonState() {{
    themes.style.display = "none";
    themePicker.style.borderBottomRightRadius = "3px";
    themePicker.style.borderBottomLeftRadius = "3px";
}}

function switchThemeButtonState() {{
    if (themes.style.display === "block") {{
        hideThemeButtonState();
    }} else {{
        showThemeButtonState();
    }}
}};

function handleThemeButtonsBlur(e) {{
    var active = document.activeElement;
    var related = e.relatedTarget;

    if (active.id !== "theme-picker" &&
        (!active.parentNode || active.parentNode.id !== "theme-choices") &&
        (!related ||
         (related.id !== "theme-picker" &&
          (!related.parentNode || related.parentNode.id !== "theme-choices")))) {{
        hideThemeButtonState();
    }}
}}

themePicker.onclick = switchThemeButtonState;
themePicker.onblur = handleThemeButtonsBlur;
{}.forEach(function(item) {{
    var but = document.createElement("button");
    but.textContent = item;
    but.onclick = function(el) {{
        switchTheme(currentTheme, mainTheme, item, true);
        useSystemTheme(false);
    }};
    but.onblur = handleThemeButtonsBlur;
    themes.appendChild(but);
}});"#,
        serde_json::to_string(&themes).unwrap()
    );

    write_minify(dst.join("theme.js"), &theme_js)?;

    write_minify(dst.join("main.js"), static_files::MAIN_JS)?;
    write_minify(dst.join("settings.js"), static_files::SETTINGS_JS)?;
    // TODO: source-script.js

    write_minify(
        dst.join("storage.js"),
        &format!("var resourcesSuffix = \"\";{}", static_files::STORAGE_JS),
    )?;

    write_minify(dst.join("normalize.css"), static_files::NORMALIZE_CSS)?;
    write(
        dst.join("FiraSans-Regular.woff"),
        static_files::fira_sans::REGULAR,
    )?;
    write(
        dst.join("FiraSans-Medium.woff"),
        static_files::fira_sans::MEDIUM,
    )?;
    write(
        dst.join("FiraSans-LICENSE.txt"),
        static_files::fira_sans::LICENSE,
    )?;
    write(
        dst.join("SourceSerifPro-Regular.ttf.woff"),
        static_files::source_serif_pro::REGULAR,
    )?;
    write(
        dst.join("SourceSerifPro-Bold.ttf.woff"),
        static_files::source_serif_pro::BOLD,
    )?;
    write(
        dst.join("SourceSerifPro-It.ttf.woff"),
        static_files::source_serif_pro::ITALIC,
    )?;
    write(
        dst.join("SourceSerifPro-LICENSE.md"),
        static_files::source_serif_pro::LICENSE,
    )?;
    write(
        dst.join("SourceCodePro-Regular.woff"),
        static_files::source_code_pro::REGULAR,
    )?;
    write(
        dst.join("SourceCodePro-Semibold.woff"),
        static_files::source_code_pro::SEMIBOLD,
    )?;
    write(
        dst.join("SourceCodePro-LICENSE.txt"),
        static_files::source_code_pro::LICENSE,
    )?;
    write(dst.join("LICENSE-MIT.txt"), static_files::LICENSE_MIT)?;
    write(dst.join("LICENSE-APACHE.txt"), static_files::LICENSE_APACHE)?;
    write(dst.join("COPYRIGHT.txt"), static_files::COPYRIGHT)?;

    Ok(())
}

#[derive(Default, Debug)]
struct Context {
    current: Vec<String>,
    dst: std::path::PathBuf,
}

impl Context {
    /// String representation of how to get back to the root path of the 'doc/'
    /// folder in terms of a relative URL.
    fn root_path(&self) -> String {
        "../".repeat(self.current.len())
    }
}

fn wrap_into_docblock<W: std::fmt::Write, F>(w: &mut W, f: F) -> std::fmt::Result
where
    F: FnOnce(&mut W) -> std::fmt::Result,
{
    write!(
        w,
        "<div class=\"docblock type-decl hidden-by-usual-hider\">"
    )?;
    f(w)?;
    write!(w, "</div>")
}

fn write_documentation<W: std::fmt::Write>(
    f: &mut W,
    comment: Option<&String>,
) -> std::fmt::Result {
    if let Some(comment) = comment {
        let mut singleline = true;
        let mut comment = comment.trim();
        if comment.starts_with("/**") {
            singleline = false;
            comment = comment[3..comment.len() - 2].trim();
        }

        write!(f, "<div class=\"docblock\"><p>")?;
        let mut first = true;
        for mut line in comment.lines() {
            line = line.trim();

            if singleline {
                if line.starts_with("///") {
                    line = line[3..].trim();
                }
            } else {
                if line.starts_with("*") {
                    line = line[1..].trim();
                }
            }

            if line.is_empty() {
                if !first {
                    write!(f, "</p><p>")?;
                }
                continue;
            }

            write!(f, "{} ", line)?;

            if first {
                write!(f, "</p><p>")?;
                first = false;
            }
        }
        write!(f, "</p></div>")?;
    }
    Ok(())
}

fn write_itemkind_fields<W: std::fmt::Write, N: std::fmt::Display>(
    cx: &Context,
    f: &mut W,
    name: N,
    itemkind: &parsed::ItemKind,
) -> std::fmt::Result {
    match &itemkind {
        parsed::StructKind(parsed::Struct { fields, .. })
        | parsed::UnionKind(parsed::Union { fields, .. }) => {
            if !fields.is_empty() {
                write!(
                    f,
                    "<div class=\"autohide sub-variant\" id=\"{id}\">",
                    id = ""
                )?;
                write!(f, "<h3>Fields of <b>{name}</b></h3><div>", name = name)?;

                write_fields(cx, f, fields)?;

                write!(f, "</div></div>")?;
            }
        }
        _ => panic!("unsupported field declaration type: {:#?}", itemkind),
    }

    Ok(())
}

fn anonymous_item_name(f: &mut std::fmt::Formatter, item: &parsed::Item) -> std::fmt::Result {
    write!(
        f,
        "&lt;&lt;anonymous {}&gt;&gt;",
        ItemType::from(&item.kind)
    )
}

struct HtmlTypeFormatter<'a> {
    cx: &'a Context,
    ty: &'a parsed::Type,
}

impl<'a> HtmlTypeFormatter<'a> {
    pub(crate) fn new(cx: &'a Context, ty: &'a parsed::Type) -> Self {
        Self { cx, ty }
    }
}

impl<'a> parsed::TypeFormatter for HtmlTypeFormatter<'a> {
    fn fmt_item(&self, f: &mut std::fmt::Formatter, itemref: &parsed::ItemRef) -> std::fmt::Result {
        let (main_name, main_type) = itemref.path.last().unwrap();
        write!(
            f,
            "<a{} class=\"{ty}\" title=\"{title}\">{name}</a>",
            href = format::display_fn(|f| match main_type {
                ItemType::Primitive => Ok(()),
                _ => write!(
                    f,
                    " href=\"{root}{path}{html}{anchor}\"",
                    root = self.cx.root_path(),
                    path = format::ensure_trailing_slash(
                        &itemref.path[..itemref.path.len() - 1]
                            .iter()
                            .map(|(name, _)| name)
                            .join("/")
                    ),
                    html = format::display_fn(|f| match main_type {
                        ItemType::Namespace => write!(f, "{}/index.html", main_name),
                        _ => write!(f, "{}.{}.html", main_type, main_name),
                    }),
                    anchor = format::display_fn(|f| {
                        if let Some(subref) = itemref.subref.as_ref() {
                            match main_type {
                                ItemType::Enum => write!(f, "#variant.{}", subref)?,
                                _ => unimplemented!(),
                            }
                        }
                        Ok(())
                    })
                ),
            }),
            ty = main_type,
            name = if let Some(subref) = itemref.subref.as_ref() {
                subref
            } else {
                main_name
            },
            title = format::display_fn(|f| {
                if *main_type != ItemType::Primitive {
                    write!(
                        f,
                        "{} {}",
                        main_type,
                        itemref.path.iter().map(|(name, _)| name).join("::")
                    )?;
                }

                Ok(())
            })
        )?;
        Ok(())
    }

    fn fmt_anonymous_item(
        &self,
        f: &mut std::fmt::Formatter,
        item: &parsed::Item,
    ) -> std::fmt::Result {
        anonymous_item_name(f, item)
    }
}

impl<'a> std::fmt::Display for HtmlTypeFormatter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.ty.fmt(f, self)
    }
}

fn write_fields<W: std::fmt::Write>(
    cx: &Context,
    f: &mut W,
    fields: &[parsed::Field],
) -> std::fmt::Result {
    for (index, field) in fields.iter().enumerate() {
        let id = if let Some(name) = &field.name {
            format!("structfield.{}", name)
        } else {
            format!("structfield-anonymous.{}", index)
        };
        let name = field
            .name
            .as_ref()
            .map(|s| s.as_str())
            .unwrap_or("&lt;&lt;anonymous&gt;&gt;");
        write!(
            f,
            "<span id=\"{id}\" class=\"{item_type} small-section-header\">\
                         <a href=\"#{id}\" class=\"anchor field\"></a>\
                         <code>{name}: {ty}</code>\
                     </span>",
            item_type = ItemType::StructField,
            id = id,
            name = name,
            ty = HtmlTypeFormatter::new(cx, &field.ty),
        )?;
        write_documentation(f, field.comment.as_ref())?;

        for anon in field.ty.iter_anonymous() {
            write_documentation(f, anon.comment.as_ref())?;
            write_itemkind_fields(
                cx,
                f,
                format::display_fn(|f| anonymous_item_name(f, &anon)),
                &anon.kind,
            )?;
        }
    }

    Ok(())
}

fn write_fields_with_header<W: std::fmt::Write>(
    cx: &Context,
    f: &mut W,
    fields: &[parsed::Field],
) -> std::fmt::Result {
    write!(
        f,
        "<h2 id=\"fields\" class=\"fields small-section-header\">
                       Fields<a href=\"#fields\" class=\"anchor\"></a></h2>",
    )?;
    write_fields(cx, f, fields)?;
    Ok(())
}

fn write_struct_or_union<W: std::fmt::Write>(
    cx: &Context,
    f: &mut W,
    item: &parsed::Item,
    fields: &[parsed::Field],
) -> std::fmt::Result {
    wrap_into_docblock(f, |f| {
        write!(
            f,
            "<pre class=\"rust {ty}\">{code};</pre>",
            ty = ItemType::from(&item.kind),
            code = HtmlTypeFormatter::new(cx, item.outertype().unwrap()),
        )
    })?;
    write_documentation(f, item.comment.as_ref())?;

    if !fields.is_empty() {
        write_fields_with_header(cx, f, fields)?;
    }

    Ok(())
}

fn write_enum_variants<W: std::fmt::Write>(
    f: &mut W,
    variants: &[parsed::EnumVariant],
) -> std::fmt::Result {
    if !variants.is_empty() {
        write!(
            f,
            "<h2 id=\"variants\" class=\"variants small-section-header\">
                   Variants<a href=\"#variants\" class=\"anchor\"></a></h2>\n"
        )?;

        for variant in variants {
            let id = format!("{}.{}", ItemType::Variant, variant.name);
            write!(
                f,
                "<div id=\"{id}\" class=\"variant small-section-header\">\
                    <a href=\"#{id}\" class=\"anchor field\"></a>\
                    <code>{name}</code></div>",
                id = id,
                name = variant.name,
            )?;
            write_documentation(f, variant.comment.as_ref())?;
        }
    }
    Ok(())
}

fn write_enum<W: std::fmt::Write>(
    cx: &Context,
    f: &mut W,
    item: &parsed::Item,
    e: &parsed::Enum,
) -> std::fmt::Result {
    wrap_into_docblock(f, |f| {
        write!(
            f,
            "<pre class=\"rust enum\">{};</pre>",
            HtmlTypeFormatter::new(cx, &e.outerty),
        )
    })?;
    write_documentation(f, item.comment.as_ref())?;

    write_enum_variants(f, &e.variants)?;

    Ok(())
}

fn write_function<W: std::fmt::Write>(
    cx: &Context,
    f: &mut W,
    item: &parsed::Item,
    func: &parsed::Function,
) -> std::fmt::Result {
    write!(
        f,
        "<pre class=\"rust fn\">{};</pre>",
        HtmlTypeFormatter::new(cx, &func.outerty),
    )?;
    write_documentation(f, item.comment.as_ref())?;

    Ok(())
}

fn write_typedef<W: std::fmt::Write>(
    cx: &Context,
    f: &mut W,
    item: &parsed::Item,
    typedef: &parsed::Typedef,
) -> std::fmt::Result {
    write!(
        f,
        "<pre class=\"rust type\">{};</pre>",
        HtmlTypeFormatter::new(cx, &typedef.outerty),
    )?;
    write_documentation(f, item.comment.as_ref())?;

    Ok(())
}

fn write_variable<W: std::fmt::Write>(
    cx: &Context,
    f: &mut W,
    item: &parsed::Item,
    variable: &parsed::Variable,
) -> std::fmt::Result {
    write!(
        f,
        "<pre class=\"rust variable\">{};</pre>",
        HtmlTypeFormatter::new(cx, &variable.outerty),
    )?;
    write_documentation(f, item.comment.as_ref())?;

    Ok(())
}

/// Compare two strings treating multi-digit numbers as single units (i.e. natural sort order).
fn compare_names(mut lhs: &str, mut rhs: &str) -> std::cmp::Ordering {
    /// Takes a non-numeric and a numeric part from the given &str.
    fn take_parts<'a>(s: &mut &'a str) -> (&'a str, &'a str) {
        let i = s.find(|c: char| c.is_ascii_digit());
        let (a, b) = s.split_at(i.unwrap_or(s.len()));
        let i = b.find(|c: char| !c.is_ascii_digit());
        let (b, c) = b.split_at(i.unwrap_or(b.len()));
        *s = c;
        (a, b)
    }

    while !lhs.is_empty() || !rhs.is_empty() {
        let (la, lb) = take_parts(&mut lhs);
        let (ra, rb) = take_parts(&mut rhs);
        // First process the non-numeric part.
        match la.cmp(ra) {
            std::cmp::Ordering::Equal => (),
            x => return x,
        }
        // Then process the numeric part, if both sides have one (and they fit in a u64).
        if let (Ok(ln), Ok(rn)) = (lb.parse::<u64>(), rb.parse::<u64>()) {
            match ln.cmp(&rn) {
                std::cmp::Ordering::Equal => (),
                x => return x,
            }
        }
        // Then process the numeric part again, but this time as strings.
        match lb.cmp(rb) {
            std::cmp::Ordering::Equal => (),
            x => return x,
        }
    }

    std::cmp::Ordering::Equal
}

fn item_path(ty: ItemType, name: &str) -> String {
    match ty {
        ItemType::Namespace => format!("{}index.html", format::ensure_trailing_slash(name)),
        _ => format!("{}.{}.html", ty, name),
    }
}

fn full_path(cx: &Context, item: &parsed::Item) -> String {
    let mut s = cx.current.join("::");
    if !cx.current.is_empty() {
        s.push_str("::");
    }
    s.push_str(&item.name.as_ref().unwrap().as_str());
    s
}

fn write_index<W: std::fmt::Write>(
    cx: &Context,
    f: &mut W,
    item: &parsed::Item,
) -> std::fmt::Result {
    write_documentation(f, item.comment.as_ref())?;

    let items = item.items().unwrap();
    let mut indices = (0..items.len()).collect::<Vec<usize>>();

    // the order of item types in the listing
    fn reorder(ty: ItemType) -> u8 {
        match ty {
            ItemType::Primitive => 2,
            ItemType::Namespace => 3,
            ItemType::Macro => 4,
            ItemType::Struct => 5,
            ItemType::Enum => 6,
            ItemType::Constant => 7,
            ItemType::Static => 8,
            ItemType::Function => 10,
            ItemType::Typedef => 12,
            ItemType::Union => 13,
            _ => 14 + ty as u8,
        }
    }

    fn cmp(i1: &parsed::Item, i2: &parsed::Item, idx1: usize, idx2: usize) -> std::cmp::Ordering {
        let ty1 = i1.type_();
        let ty2 = i2.type_();
        if ty1 != ty2 {
            return (reorder(ty1), idx1).cmp(&(reorder(ty2), idx2));
        }
        let lhs = i1.name.as_ref().unwrap().as_str();
        let rhs = i2.name.as_ref().unwrap().as_str();
        compare_names(&lhs, &rhs)
    }

    indices.sort_by(|&i1, &i2| cmp(&items[i1], &items[i2], i1, i2));

    let mut curty = None;
    for &idx in &indices {
        let myitem = &items[idx];
        let myty = Some(myitem.type_());
        if myty != curty {
            if curty.is_some() {
                write!(f, "</table>")?;
            }
            curty = myty;
            let (short, name) = ItemType::to_strs(&myty.unwrap());
            write!(
                f,
                "<h2 id=\"{id}\" class=\"section-header\">\
                       <a href=\"#{id}\">{name}</a></h2>\n<table>",
                id = short.to_owned(),
                name = name
            )?;
        }

        // TODO: parse comments and extract brief
        let _doc_value = myitem.comment.as_ref().map(|c| c.as_str()).unwrap_or("");
        write!(
            f,
            "<tr class=\"module-item\">\
                         <td><a class=\"{class}\" href=\"{href}\" \
                             title=\"{title}\">{name}</a></td>\
                         <td class=\"docblock-short\">{docs}</td>\
                     </tr>",
            name = *myitem.name.as_ref().unwrap(),
            docs = "summary",
            class = myitem.type_(),
            href = item_path(myitem.type_(), &myitem.name.as_ref().unwrap().as_str()),
            title = [full_path(cx, myitem), myitem.type_().to_string()]
                .iter()
                .filter_map(|s| if !s.is_empty() {
                    Some(s.as_str())
                } else {
                    None
                })
                .collect::<Vec<_>>()
                .join(" "),
        )?;
    }

    Ok(())
}

fn write_page_content<W: std::fmt::Write>(
    cx: &Context,
    f: &mut W,
    item: &parsed::Item,
    index: bool,
) -> std::fmt::Result {
    write!(f, "<h1 class=\"fqn\"><span class=\"out-of-band\">")?;

    write!(
        f,
        "<span id=\"render-detail\">\
                <a id=\"toggle-all-docs\" href=\"javascript:void(0)\" \
                    title=\"collapse all docs\">\
                    [<span class=\"inner\">&#x2212;</span>]\
                </a>\
            </span>"
    )?;

    // TODO: srclink

    write!(f, "</span>")?; // out-of-band
    write!(f, "<span class=\"in-band\">")?;

    let name = match item.kind {
        parsed::EnumKind(..) => "Enum ",
        parsed::FunctionKind(..) => "Function ",
        parsed::StructKind(..) => "Struct ",
        parsed::TypedefKind(..) => "Type Definition ",
        parsed::UnionKind(..) => "Union ",
        parsed::VariableKind(..) => "Variable ",
        parsed::NamespaceKind(..) => "Namespace ",
    };
    f.write_str(name)?;

    write!(
        f,
        "<a class=\"{}\" href=\"\">{}</a>",
        item.type_(),
        escape::Escape(item.name.as_ref().unwrap())
    )?;

    write!(f, "</span></h1>")?; // in-band

    if index {
        write_index(cx, f, item)?;
    } else {
        match &item.kind {
            parsed::EnumKind(func) => write_enum(cx, f, item, func)?,
            parsed::FunctionKind(func) => write_function(cx, f, item, func)?,
            parsed::StructKind(parsed::Struct { fields, .. })
            | parsed::UnionKind(parsed::Union { fields, .. }) => {
                write_struct_or_union(cx, f, item, &fields)?
            }
            parsed::TypedefKind(typedef) => write_typedef(cx, f, item, typedef)?,
            parsed::VariableKind(variable) => write_variable(cx, f, item, variable)?,
            parsed::NamespaceKind(_) => write_index(cx, f, item)?,
        }
    }

    Ok(())
}

fn write_sidebar<W: std::fmt::Write>(
    _ctx: &Context,
    f: &mut W,
    item: &parsed::Item,
) -> std::fmt::Result {
    write!(
        f,
        "<p class=\"location\">{}{}</p>",
        "Struct ",
        escape::Escape(item.name.as_ref().unwrap())
    )?;

    write!(f, "<div class=\"sidebar-elems\">")?;

    // TODO: item-specific things

    write!(
        f,
        "<p class=\"location\">{}</p>",
        escape::Escape("<<global>>")
    )?;

    // Sidebar refers to the enclosing module, not this module.
    let relpath = if false { "../" } else { "" };
    write!(
        f,
        "<script>window.sidebarCurrent = {{\
                name: \"{name}\", \
                ty: \"{ty}\", \
                relpath: \"{path}\"\
            }};</script>",
        name = item.name.as_ref().unwrap_or(&"".to_string()),
        ty = item.type_(),
        path = relpath
    )?;
    write!(
        f,
        "<script defer src=\"{path}sidebar-items.js\"></script>",
        path = relpath
    )?;
    // Closes sidebar-elems div.
    write!(f, "</div>")?;

    Ok(())
}

fn write_item(cx: &Context, item: &parsed::Item, index: bool) -> Result<(), Error> {
    let ty = item.type_();
    let path = if index {
        cx.dst.join("index.html")
    } else {
        cx.dst
            .join(format!("{}.{}.html", ty, item.name.as_ref().unwrap()))
    };
    let f = open_file(path)?;

    let tyname = item.type_();
    let layout = layout::Layout {
        logo: String::new(),
        favicon: String::new(),
        default_settings: std::collections::HashMap::new(),
        krate: "".to_string(),
        css_file_extension: None,
        generate_search_filter: false,
    };
    let page = layout::Page {
        css_class: tyname.as_str(),
        root_path: &cx.root_path(),
        static_root_path: None,
        title: item.name.as_ref().unwrap(),
        description: "",
        keywords: "",
        resource_suffix: "",
        extra_scripts: &[],
        static_extra_scripts: &[],
    };
    layout::render(
        format::IoWriteFormatter::new(f),
        &layout,
        &page,
        format::display_fn(|f| write_sidebar(cx, f, item)),
        format::display_fn(|f| write_page_content(cx, f, item, index)),
        &[layout::StylePath {
            path: cx.dst.join("light.css"),
            disabled: false,
        }],
    )?;

    Ok(())
}

fn write_items(cx: &mut Context, container: &parsed::Item) -> Result<(), Error> {
    let items = container.items().unwrap();

    write_item(&cx, container, true)?;

    for item in items {
        if !matches!(item.kind, parsed::ItemKind::NamespaceKind(..)) {
            write_item(&cx, item, false)?;
        }

        if item.items().map(|items| !items.is_empty()).unwrap_or(false) {
            let name = item.name.clone().unwrap();
            cx.current.push(name.clone());
            cx.dst.push(name);

            write_items(cx, item)?;

            cx.dst.pop();
            cx.current.pop();
        }
    }

    Ok(())
}

/// A pair of name and its optional document.
type NameDoc = (String, Option<String>);

/// Construct a map of items shown in the sidebar to a plain-text summary of their docs.
fn build_sidebar_items(root: &parsed::Item) -> std::collections::BTreeMap<String, Vec<NameDoc>> {
    // BTreeMap instead of HashMap to get a sorted output
    let mut map: std::collections::BTreeMap<_, Vec<_>> = std::collections::BTreeMap::new();
    for item in root.items().unwrap() {
        let short = item.type_();
        let myname = match item.name {
            None => continue,
            Some(ref s) => s.to_string(),
        };
        let short = short.to_string();
        map.entry(short).or_default().push((
            myname, // TODO: description
            None,
        ));
    }

    for items in map.values_mut() {
        items.sort();
    }
    map
}

fn main() -> Result<(), Error> {
    let mut args = std::env::args();
    args.next().unwrap();
    let cdb_file = args.next().unwrap();
    let dst = std::path::PathBuf::from(args.next().unwrap());

    let cdb = load_cdb(cdb_file)?;

    let clang = clang::Clang::new().unwrap();
    let mut root = parsed::Item::root(&std::sync::Arc::new(std::path::PathBuf::new()));

    let cwd = std::env::current_dir()?;
    let mut results: Vec<_> = cdb
        .par_iter()
        .filter_map(|command| {
            // skip unsupported files
            match command.file.extension().map(|o| o.to_str()).unwrap_or(None) {
                Some("c") | Some("h") => Some(process_file(command, &clang).unwrap()),
                _ => None,
            }
        })
        .collect();
    for result in results.drain(..) {
        root.extend(result);
    }
    std::env::set_current_dir(&cwd)?;
    //println!("{:#?}", root);

    copy_statics(&dst)?;

    let mut v = String::from("var searchIndex = JSON.parse('{\\\n");
    //v.push_str(&all_indexes.join(",\\\n"));
    // "addSearchOptions" has to be called first so the crate filtering can be set before the
    // search might start (if it's set into the URL for example).
    v.push_str("\\\n}');\naddSearchOptions(searchIndex);initSearch(searchIndex);");
    write(dst.join("search-index.js"), v.as_bytes())?;

    let sidebar_items = build_sidebar_items(&root);
    let v = format!(
        "initSidebarItems({});",
        serde_json::to_string(&sidebar_items).unwrap()
    );
    write(dst.join("sidebar-items.js"), v.as_bytes())?;

    let mut cx = Context::default();
    cx.dst = dst;
    write_items(&mut cx, &root)?;

    Ok(())
}

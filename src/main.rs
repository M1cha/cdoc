mod escape;
mod format;
mod item_type;
mod layout;
mod parsed;
mod static_files;

use item_type::ItemType;
use std::io::Write;

#[derive(Debug, thiserror::Error)]
enum Error {
    #[error(transparent)]
    Io(#[from] std::io::Error),
    #[error(transparent)]
    Fmt(#[from] std::fmt::Error),
    #[error(transparent)]
    Json(#[from] serde_json::Error),

    #[error("exit status: {0}")]
    ExitStatus(std::process::ExitStatus),
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

fn process_file(
    command: &CompileCommand,
    clang: &clang::Clang,
    files: &mut parsed::Files,
) -> Result<(), Error> {
    if command.file.extension() != Some(std::ffi::OsStr::new("c")) {
        return Ok(());
    }

    std::env::set_current_dir(&command.directory)?;
    let compilationunit = std::fs::canonicalize(&command.file)?;
    //let outpath = std::path::Path::new("/tmp/cdoc.c");
    let outpath = std::path::PathBuf::from(format!(
        "/tmp/cdoc-{}.c",
        compilationunit.file_name().unwrap().to_str().unwrap()
    ));

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
    args.push(outpath.to_str().unwrap());

    //println!("ARGS: {:?}", args);
    let status = std::process::Command::new(args[0])
        .args(&args[1..])
        .status()?;
    if !status.success() {
        return Err(Error::ExitStatus(status));
    }

    let index = clang::Index::new(&clang, false, true);
    let tu = index
        .parser(outpath)
        .arguments(&["-target", "armv7a"])
        .parse()
        .unwrap();
    for entity in tu.get_entity().get_children() {
        let loc = if let Some(loc) = entity.get_location().map(|loc| loc.get_presumed_location()) {
            loc
        } else {
            continue;
        };

        let abspath = std::fs::canonicalize(loc.0)?;
        if abspath.extension() != Some(std::ffi::OsStr::new("h")) {
            return Ok(());
        }

        let file = files.for_path(&abspath);
        /*println!(
            "{:#?}",
            entity
                .get_children()
                .iter()
                .map(|c| c.get_type())
                .collect::<Vec<_>>()
        );*/
        let kind = match entity.get_kind() {
            clang::EntityKind::EnumDecl => parsed::EnumKind(parsed::Enum::from(&entity)),
            clang::EntityKind::FunctionDecl => {
                parsed::FunctionKind(parsed::Function::from(&entity))
            }
            clang::EntityKind::StructDecl => parsed::StructKind(parsed::Struct::from(&entity)),
            clang::EntityKind::TypedefDecl => parsed::TypedefKind(parsed::Typedef::from(&entity)),
            clang::EntityKind::UnionDecl => parsed::UnionKind(parsed::Union::from(&entity)),
            clang::EntityKind::VarDecl => parsed::VariableKind(parsed::Variable::from(&entity)),
            _ => continue,
        };
        file.add_item(parsed::Item {
            compilationunit: compilationunit.clone(),
            name: entity.get_name(),
            // TODO: support comments in the same line as the code
            comment: entity.get_comment(),
            kind,
            code: entity.get_pretty_printer().print(),
        });
    }

    Ok(())
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

fn write_fields<W: std::fmt::Write>(f: &mut W, fields: &[parsed::Field]) -> std::fmt::Result {
    for field in fields {
        let id = format!("structfield.{}", field.name);
        write!(
            f,
            "<span id=\"{id}\" class=\"{item_type} small-section-header\">\
                         <a href=\"#{id}\" class=\"anchor field\"></a>\
                         <code>{name}: {ty}</code>\
                     </span>",
            item_type = ItemType::StructField,
            id = id,
            name = field.name,
            ty = format::display_fn(|f| match &field.ty {
                parsed::FieldType::String(s) => f.write_str(s),
                parsed::FieldType::Struct(..) =>
                    write!(f, "{}", escape::Escape("<<anonymous struct>>")),
                parsed::FieldType::Union(..) =>
                    write!(f, "{}", escape::Escape("<<anonymous union>>")),
            }),
        )?;
        write_documentation(f, field.comment.as_ref())?;

        match &field.ty {
            parsed::FieldType::Struct(parsed::Struct { fields, .. })
            | parsed::FieldType::Union(parsed::Union { fields, .. }) => {
                if !fields.is_empty() {
                    write!(
                        f,
                        "<div class=\"autohide sub-variant\" id=\"{id}\">",
                        id = ""
                    )?;
                    write!(
                        f,
                        "<h3>Fields of <b>{name}</b></h3><div>",
                        name = field.name
                    )?;

                    write_fields(f, fields)?;

                    write!(f, "</div></div>")?;
                }
            }
            _ => (),
        }
    }

    Ok(())
}

fn write_struct_or_union<W: std::fmt::Write>(
    f: &mut W,
    item: &parsed::Item,
    fields: &[parsed::Field],
) -> std::fmt::Result {
    wrap_into_docblock(f, |f| {
        write!(
            f,
            "<pre class=\"rust {}\">{}</pre>",
            ItemType::from(&item.kind),
            item.code
        )
    })?;
    write_documentation(f, item.comment.as_ref())?;

    if !fields.is_empty() {
        write!(
            f,
            "<h2 id=\"fields\" class=\"fields small-section-header\">
                       Fields<a href=\"#fields\" class=\"anchor\"></a></h2>",
        )?;
        write_fields(f, fields)?;
    }

    Ok(())
}

fn write_enum<W: std::fmt::Write>(
    f: &mut W,
    item: &parsed::Item,
    e: &parsed::Enum,
) -> std::fmt::Result {
    wrap_into_docblock(f, |f| {
        write!(f, "<pre class=\"rust enum\">{};</pre>", item.code)
    })?;
    write_documentation(f, item.comment.as_ref())?;

    if !e.variants.is_empty() {
        write!(
            f,
            "<h2 id=\"variants\" class=\"variants small-section-header\">
                   Variants<a href=\"#variants\" class=\"anchor\"></a></h2>\n"
        )?;

        for variant in &e.variants {
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

fn write_function<W: std::fmt::Write>(
    f: &mut W,
    item: &parsed::Item,
    _func: &parsed::Function,
) -> std::fmt::Result {
    write!(
        f,
        "<pre class=\"rust fn\">{};</pre>",
        item.code.split("{").collect::<Vec<_>>()[0].trim()
    )?;
    write_documentation(f, item.comment.as_ref())?;

    Ok(())
}

fn write_typedef<W: std::fmt::Write>(
    f: &mut W,
    item: &parsed::Item,
    _typedef: &parsed::Typedef,
) -> std::fmt::Result {
    write!(f, "<pre class=\"rust type\">{};</pre>", item.code)?;
    write_documentation(f, item.comment.as_ref())?;

    Ok(())
}

fn write_variable<W: std::fmt::Write>(
    f: &mut W,
    item: &parsed::Item,
    _variable: &parsed::Variable,
) -> std::fmt::Result {
    write!(f, "<pre class=\"rust variable\">{};</pre>", item.code)?;
    write_documentation(f, item.comment.as_ref())?;

    Ok(())
}

fn write_page_content<W: std::fmt::Write>(f: &mut W, item: &parsed::Item) -> std::fmt::Result {
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
    };
    f.write_str(name)?;

    write!(
        f,
        "<a class=\"{}\" href=\"\">{}</a>",
        item.type_(),
        item.name.as_ref().unwrap()
    )?;

    write!(f, "</span></h1>")?; // in-band

    match &item.kind {
        parsed::EnumKind(func) => write_enum(f, item, func)?,
        parsed::FunctionKind(func) => write_function(f, item, func)?,
        parsed::StructKind(s) => write_struct_or_union(f, item, &s.fields)?,
        parsed::UnionKind(s) => write_struct_or_union(f, item, &s.fields)?,
        parsed::TypedefKind(typedef) => write_typedef(f, item, typedef)?,
        parsed::VariableKind(variable) => write_variable(f, item, variable)?,
    }

    Ok(())
}

fn write_sidebar<W: std::fmt::Write>(f: &mut W, item: &parsed::Item) -> std::fmt::Result {
    write!(
        f,
        "<p class=\"location\">{}{}</p>",
        "Struct ",
        item.name.as_ref().unwrap()
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

fn write_item<P: AsRef<std::path::Path>>(dst: P, item: &parsed::Item) -> Result<(), Error> {
    let dst = dst.as_ref();
    let f = open_file(dst.join(format!(
        "{}.{}.html",
        item.type_(),
        item.name.as_ref().unwrap()
    )))?;

    let tyname = item.type_();
    let layout = layout::Layout {
        logo: String::new(),
        favicon: String::new(),
        default_settings: std::collections::HashMap::new(),
        krate: "crate name".to_string(),
        css_file_extension: None,
        generate_search_filter: false,
    };
    let page = layout::Page {
        css_class: tyname.as_str(),
        root_path: "",
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
        format::display_fn(|f| write_sidebar(f, item)),
        format::display_fn(|f| write_page_content(f, item)),
        &[layout::StylePath {
            path: dst.join("light.css"),
            disabled: false,
        }],
    )?;

    Ok(())
}

/// A pair of name and its optional document.
type NameDoc = (String, Option<String>);

/// Construct a map of items shown in the sidebar to a plain-text summary of their docs.
fn build_sidebar_items(files: &parsed::Files) -> std::collections::BTreeMap<String, Vec<NameDoc>> {
    // BTreeMap instead of HashMap to get a sorted output
    let mut map: std::collections::BTreeMap<_, Vec<_>> = std::collections::BTreeMap::new();
    for file in files.list.values() {
        for item in &file.items {
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
    let mut files = parsed::Files::default();

    let cwd = std::env::current_dir()?;
    for command in &cdb {
        process_file(command, &clang, &mut files)?;
    }
    std::env::set_current_dir(&cwd)?;
    //println!("{:#?}", files);

    copy_statics(&dst)?;

    let mut v = String::from("var searchIndex = JSON.parse('{\\\n");
    //v.push_str(&all_indexes.join(",\\\n"));
    // "addSearchOptions" has to be called first so the crate filtering can be set before the
    // search might start (if it's set into the URL for example).
    v.push_str("\\\n}');\naddSearchOptions(searchIndex);initSearch(searchIndex);");
    write(dst.join("search-index.js"), v.as_bytes())?;

    let items = build_sidebar_items(&files);
    let v = format!(
        "initSidebarItems({});",
        serde_json::to_string(&items).unwrap()
    );
    write(dst.join("sidebar-items.js"), v.as_bytes())?;

    for file in files.list.values() {
        for item in &file.items {
            if item.name.is_some() {
                write_item(&dst, item)?;
            } else {
                println!("NAMELESS: {:?}", item.code);
            }
        }
    }

    Ok(())
}

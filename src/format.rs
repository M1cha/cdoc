pub(crate) fn display_fn(
    f: impl FnOnce(&mut std::fmt::Formatter<'_>) -> std::fmt::Result,
) -> impl std::fmt::Display {
    WithFormatter(std::cell::Cell::new(Some(f)))
}

pub(crate) struct IoWriteFormatter<W> {
    writer: W,
}

impl<W: std::io::Write> IoWriteFormatter<W> {
    pub fn new(writer: W) -> Self {
        Self { writer }
    }
}

impl<W: std::io::Write> std::fmt::Write for IoWriteFormatter<W> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        write!(self.writer, "{}", s).map_err(|_| std::fmt::Error)
    }
}

pub(crate) struct WithFormatter<F>(std::cell::Cell<Option<F>>);

impl<F> std::fmt::Display for WithFormatter<F>
where
    F: FnOnce(&mut std::fmt::Formatter<'_>) -> std::fmt::Result,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        (self.0.take()).unwrap()(f)
    }
}

pub(crate) fn ensure_trailing_slash(v: &str) -> impl std::fmt::Display + '_ {
    display_fn(move |f| {
        if !v.ends_with('/') && !v.is_empty() {
            write!(f, "{}/", v)
        } else {
            write!(f, "{}", v)
        }
    })
}

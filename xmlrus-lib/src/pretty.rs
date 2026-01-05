use crate::ContentSpec;
use crate::Context;
use crate::EntityType;
use crate::NodeKind;

use std::io::Write;

pub struct Options {
    pub tab_size: usize,
    pub dtd: bool,
    pub comments: bool,
}

impl Default for Options {
    fn default() -> Self {
        Self {
            tab_size: 2,
            dtd: true,
            comments: true,
        }
    }
}

pub fn pretty_print(options: Options, ctx: &Context) -> std::io::Result<()> {
    let tab_size = options.tab_size;
    let mut writer = std::io::BufWriter::new(std::io::stdout());

    let mut indent = 0;
    writeln!(writer, "Document")?;

    let mut nodes = ctx.doc.nodes.iter();
    let first = nodes.next().unwrap();

    if let NodeKind::Declaration {
        version,
        encoding,
        standalone,
    } = first.data
    {
        indent += tab_size;
        writeln!(writer, "{:>indent$}Version: {version}", "")?;
        if let Some(encoding) = encoding {
            writeln!(writer, "{:>indent$}Encoding: {encoding}", "")?;
        }
        if let Some(standalone) = standalone {
            writeln!(writer, "{:>indent$}Standalone: {standalone}", "")?;
        }
    }

    if options.dtd {
        pp_dtd(&mut writer, &mut indent, tab_size, ctx)?;
    }

    Ok(())
}

fn pp_dtd<W>(
    writer: &mut std::io::BufWriter<W>,
    indent: &mut usize,
    tab_size: usize,
    ctx: &Context,
) -> std::io::Result<()>
where
    W: ?Sized + std::io::Write,
{
    write!(writer, "{:>indent$}DTD", "")?;
    if let Some(doc_name) = ctx.doc.name {
        write!(writer, " ({doc_name})")?;
    }
    writeln!(writer)?;

    *indent += tab_size;
    for entity in ctx.entities.values() {
        writeln!(writer, "{:>indent$}Entity ({})", "", entity.name)?;
        *indent += tab_size;
        match entity.entity_type {
            EntityType::InternalGeneral { value } => writeln!(writer, "{:>indent$}INTERNAL_GENERAL ({value})", "")?,
            EntityType::InternalParameter { value } => writeln!(writer, "{:>indent$}INTERNAL_PARAMETER ({value})", "")?,
            EntityType::InternalPredefined { value } => {
                writeln!(writer, "{:>indent$}INTERNAL_PREDEFINED ({value})", "")?
            }
            EntityType::ExternalGeneralParsed { system_id, public_id } => {
                writeln!(writer, "{:>indent$}EXTERNAL_GENERAL_PARSED", "")?;
                *indent += tab_size;
                writeln!(writer, "{:>indent$}SystemId ({system_id})", "")?;
                if let Some(public_id) = public_id {
                    writeln!(writer, "{:>indent$}PublicId ({public_id})", "")?;
                }
                *indent -= tab_size;
            }
            EntityType::ExternalGeneralUnparsed {
                system_id,
                public_id,
                ndata,
            } => {
                writeln!(writer, "{:>indent$}EXTERNAL_GENERAL_UNPARSED", "")?;
                *indent += tab_size;
                writeln!(writer, "{:>indent$}SystemId ({system_id})", "")?;
                if let Some(public_id) = public_id {
                    writeln!(writer, "{:>indent$}PublicId ({public_id})", "")?;
                }
                writeln!(writer, "{:>indent$}NDATA: {ndata}", "")?;
                *indent -= tab_size;
            }
            EntityType::ExternalParameter { system_id, public_id } => {
                writeln!(writer, "{:>indent$}EXTERNAL_PARAMETER", "")?;
                *indent += tab_size;
                writeln!(writer, "{:>indent$}SystemId ({system_id})", "")?;
                if let Some(public_id) = public_id {
                    writeln!(writer, "{:>indent$}PublicId: ({public_id})", "")?;
                }
                *indent -= tab_size;
            }
        }

        *indent -= tab_size;
    }

    for element_decl in &ctx.element_types {
        write!(
            writer,
            "{:>indent$}Element ({}) {}",
            "",
            element_decl.name,
            content_spec(&element_decl.content_spec)
        )?;

        if let Some(raw) = element_decl.raw {
            // Remove any repetative whitespace
            let words = raw.split_whitespace().collect::<Vec<_>>();
            write!(writer, " {}", words.join(" "))?;
        }

        writeln!(writer)?;
    }
    Ok(())
}

fn content_spec(content_spec: &ContentSpec) -> &'static str {
    match content_spec {
        ContentSpec::Empty => "EMPTY",
        ContentSpec::Any => "ANY",
        ContentSpec::MixedContent(_, _) => "MIXED",
        ContentSpec::ElementContent(_) => "ELEMENT",
    }
}

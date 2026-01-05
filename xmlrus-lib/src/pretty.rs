use crate::ContentParticle;
use crate::ContentSpec;
use crate::Context;
use crate::ElementContentChildren;
use crate::EntityType;
use crate::NodeKind;
use crate::Repetition;

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
        pp_dtd(&mut writer, &mut indent, tab_size, &ctx)?;
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
            EntityType::InternalGeneral { value } => writeln!(writer, "{:>indent$}INTERNAL_GENERAL: {value}", "")?,
            EntityType::InternalParameter { value } => writeln!(writer, "{:>indent$}INTERNAL_PARAMETER: {value}", "")?,
            EntityType::InternalPredefined { value } => {
                writeln!(writer, "{:>indent$}INTERNAL_PREDEFINED: {value}", "")?
            }
            EntityType::ExternalGeneralParsed { system_id, public_id } => {
                writeln!(writer, "{:>indent$}EXTERNAL_GENERAL_PARSED", "")?;
                *indent += tab_size;
                writeln!(writer, "{:>indent$}SystemId: {system_id}", "")?;
                if let Some(public_id) = public_id {
                    writeln!(writer, "{:>indent$}PublicId: {public_id}", "")?;
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
                writeln!(writer, "{:>indent$}SystemId: {system_id}", "")?;
                if let Some(public_id) = public_id {
                    writeln!(writer, "{:>indent$}PublicId: {public_id}", "")?;
                }
                writeln!(writer, "{:>indent$}NDATA: {ndata}", "")?;
                *indent -= tab_size;
            }
            EntityType::ExternalParameter { system_id, public_id } => {
                writeln!(writer, "{:>indent$}EXTERNAL_PARAMETER", "")?;
                *indent += tab_size;
                writeln!(writer, "{:>indent$}SystemId: {system_id}", "")?;
                if let Some(public_id) = public_id {
                    writeln!(writer, "{:>indent$}PublicId: {public_id}", "")?;
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

        match &element_decl.content_spec {
            ContentSpec::Empty | ContentSpec::Any => {}
            ContentSpec::MixedContent(items, repeated) => {
                write!(writer, " (#PCDATA | ")?;
                write!(writer, "{}", items.join(" | "))?;
                write!(writer, ")")?;
                if *repeated {
                    write!(writer, "*")?;
                }
            }

            ContentSpec::ElementContent(element_content) => match &element_content.children {
                ElementContentChildren::Choice(content_particles) => {
                    pp_content_particles(content_particles, writer, &element_content.repetition, '|', '|')?
                }
                ElementContentChildren::Seq(content_particles) => {
                    pp_content_particles(content_particles, writer, &element_content.repetition, ',', ',')?
                }
            },
        }
        writeln!(writer)?;
    }
    Ok(())
}

fn pp_content_particles<W>(
    content: &Vec<ContentParticle>,
    writer: &mut std::io::BufWriter<W>,
    repetition: &Repetition,
    outer_sep: char,
    inner_sep: char,
) -> std::io::Result<()>
where
    W: ?Sized + std::io::Write,
{
    write!(writer, " (")?;
    let mut iter = content.iter();

    if let Some(first) = iter.next() {
        pp_content_particle(first, writer, outer_sep, true, false)?;
    }

    loop {
        let one = iter.next();
        let two = iter.next();

        match (one, two) {
            (Some(c), None) => {
                pp_content_particle(c, writer, inner_sep, false, true)?;
                break;
            }
            (Some(f), Some(s)) => {
                pp_content_particle(f, writer, inner_sep, false, false)?;
                pp_content_particle(s, writer, inner_sep, false, false)?;
            }
            (None, None) | (None, Some(_)) => break,
        }
    }
    write!(writer, ")")?;
    write!(writer, "{}", repetition.to_string())?;

    Ok(())
}

fn pp_content_particle<W: ?Sized + std::io::Write>(
    content_particle: &ContentParticle,
    writer: &mut std::io::BufWriter<W>,
    sep: char,
    first: bool,
    last: bool,
) -> std::io::Result<()> {
    match content_particle {
        ContentParticle::Name { name, repetition } => {
            if !first {
                write!(writer, " ")?;
            }

            write!(writer, "{}", name)?;

            if sep == '|' && !last {
                write!(writer, " ")?;
            }

            if !last {
                write!(writer, "{}", sep)?;
            }

            write!(writer, "{}", repetition.to_string())?;
        }
        ContentParticle::Choice { content, repetition } => pp_content_particles(content, writer, repetition, sep, '|')?,
        ContentParticle::Seq { content, repetition } => pp_content_particles(content, writer, repetition, sep, ',')?,
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

use crate::Attribute;
use crate::ContentSpec;
use crate::Context;
use crate::EntityType;
use crate::Namespace;
use crate::Node;
use crate::NodeKind;
use crate::Notation;

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
    let mut first = nodes.next();

    if let NodeKind::Declaration {
        version,
        encoding,
        standalone,
    } = first.unwrap().data
    {
        indent += tab_size;
        writeln!(writer, "{:>indent$}Version: {version}", "")?;
        if let Some(encoding) = encoding {
            writeln!(writer, "{:>indent$}Encoding: {encoding}", "")?;
        }
        if let Some(standalone) = standalone {
            writeln!(writer, "{:>indent$}Standalone: {standalone}", "")?;
        }

        indent -= tab_size;
        first.take();
    }

    if options.dtd {
        pp_dtd(&mut writer, &mut indent, tab_size, ctx)?;
    }

    if let Some(node) = first {
        pp_node(&options, &mut writer, &mut indent, tab_size, node)?;
    }

    for node in nodes {
        pp_node(&options, &mut writer, &mut indent, tab_size, node)?;
    }

    Ok(())
}

fn pp_node<W>(
    options: &Options,
    writer: &mut std::io::BufWriter<W>,
    indent: &mut usize,
    tab_size: usize,
    node: &Node,
) -> std::io::Result<()>
where
    W: ?Sized + std::io::Write,
{
    match &node.data {
        NodeKind::Declaration { .. } => {}
        NodeKind::Element {
            name,
            attributes,
            namespaces,
            children,
        } => pp_element(
            options, writer, indent, tab_size, name, attributes, namespaces, children,
        )?,
        NodeKind::ProcessingInstruction { target, data } => pp_pi(writer, indent, tab_size, target, data)?,
        NodeKind::Text(text) => pp_text(writer, indent, tab_size, text)?,
        NodeKind::Comment(comment) => {
            if options.comments {
                pp_comment(writer, indent, tab_size, comment)?;
            }
        }
        NodeKind::CData(data) => pp_cdata(writer, indent, tab_size, data)?,
    }
    Ok(())
}

fn pp_cdata<W>(
    writer: &mut std::io::BufWriter<W>,
    indent: &mut usize,
    tab_size: usize,
    data: &str,
) -> std::io::Result<()>
where
    W: ?Sized + std::io::Write,
{
    *indent += tab_size;
    writeln!(writer, "{:>indent$}CData {}", "", data)?;
    *indent -= tab_size;
    Ok(())
}

fn pp_pi<W>(
    writer: &mut std::io::BufWriter<W>,
    indent: &mut usize,
    tab_size: usize,
    target: &str,
    data: &Option<&str>,
) -> std::io::Result<()>
where
    W: ?Sized + std::io::Write,
{
    *indent += tab_size;
    write!(writer, "{:>indent$}PI ({})", "", target)?;
    if let Some(data) = data {
        write!(writer, " {data}")?;
    }
    writeln!(writer)?;
    *indent -= tab_size;
    Ok(())
}

fn pp_comment<W>(
    writer: &mut std::io::BufWriter<W>,
    indent: &mut usize,
    tab_size: usize,
    comment: &str,
) -> std::io::Result<()>
where
    W: ?Sized + std::io::Write,
{
    *indent += tab_size;
    writeln!(writer, "{:>indent$}Comment {}", "", comment.replace("\n", "").trim())?;
    *indent -= tab_size;
    Ok(())
}

fn pp_text<W>(
    writer: &mut std::io::BufWriter<W>,
    indent: &mut usize,
    tab_size: usize,
    text: &str,
) -> std::io::Result<()>
where
    W: ?Sized + std::io::Write,
{
    *indent += tab_size;
    writeln!(writer, "{:>indent$}Text {}", "", text.trim())?;
    *indent -= tab_size;
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn pp_element<W>(
    options: &Options,
    writer: &mut std::io::BufWriter<W>,
    indent: &mut usize,
    tab_size: usize,
    name: &str,
    attributes: &Vec<Attribute>,
    namespaces: &Vec<Namespace>,
    children: &Vec<Node>,
) -> std::io::Result<()>
where
    W: ?Sized + std::io::Write,
{
    *indent += tab_size;

    writeln!(writer, "{:>indent$}Element ({})", "", name)?;

    for namespace in namespaces {
        pp_namespace(writer, indent, tab_size, namespace)?;
    }

    for attribute in attributes {
        pp_attribute(writer, indent, tab_size, attribute)?;
    }

    for child in children {
        pp_node(options, writer, indent, tab_size, child)?;
    }

    *indent -= tab_size;

    Ok(())
}

fn pp_attribute<W>(
    writer: &mut std::io::BufWriter<W>,
    indent: &mut usize,
    tab_size: usize,
    attribute: &Attribute,
) -> std::io::Result<()>
where
    W: ?Sized + std::io::Write,
{
    *indent += tab_size;
    writeln!(
        writer,
        "{:>indent$}Attribute {}=\"{}\"",
        "", attribute.name, attribute.value
    )?;
    *indent -= tab_size;

    Ok(())
}

fn pp_namespace<W>(
    writer: &mut std::io::BufWriter<W>,
    indent: &mut usize,
    tab_size: usize,
    namespace: &Namespace,
) -> std::io::Result<()>
where
    W: ?Sized + std::io::Write,
{
    *indent += tab_size;

    write!(writer, "{:>indent$}Namespace", "")?;

    if let Some(name) = namespace.name {
        write!(writer, " {name} ")?;
    }

    write!(writer, "uri=\"{}\"", namespace.uri)?;
    writeln!(writer)?;

    *indent -= tab_size;

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
    *indent += tab_size;
    write!(writer, "{:>indent$}DTD", "")?;
    if let Some(doc_name) = ctx.doc.name {
        write!(writer, " ({doc_name})")?;
    }
    writeln!(writer)?;

    *indent += tab_size;

    for element_decl in &ctx.element_types {
        write!(
            writer,
            "{:>indent$}ElementDecl ({}) {}",
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

    for attr_decl in ctx.attr_decls.values() {
        if let Some(att_defs) = &attr_decl.att_defs {
            for att_def in att_defs {
                write!(
                    writer,
                    "{:>indent$}AttrDecl ({}) {}",
                    "", attr_decl.element_name, att_def.name
                )?;

                match &att_def.att_type {
                    crate::AttType::String => write!(writer, " CDATA")?,
                    crate::AttType::Tokenized(tokenized_type) => {
                        let tt = match tokenized_type {
                            crate::TokenizedType::Id => " ID",
                            crate::TokenizedType::IdRef => " IDREF",
                            crate::TokenizedType::IdRefs => " IDREFS",
                            crate::TokenizedType::Entity => " ENTITY",
                            crate::TokenizedType::Entities => " ENTITIES",
                            crate::TokenizedType::NmToken => " NMTOKEN",
                            crate::TokenizedType::NmTokens => " NMTOKENS",
                        };
                        write!(writer, " {tt}")?;
                    }
                    crate::AttType::Enumerated(enumerated_type) => match enumerated_type {
                        crate::EnumeratedType::NotationType(items) => {
                            write!(writer, " NOTATION ({})", items.join(" | "))?;
                        }
                        crate::EnumeratedType::Enumeration(items) => {
                            write!(writer, " ENUMERATION ({})", items.join(" | "))?;
                        }
                    },
                }

                match &att_def.default_decl {
                    crate::DefaultDecl::Required => write!(writer, " REQUIRED")?,
                    crate::DefaultDecl::Implied => write!(writer, " IMPLIED")?,
                    crate::DefaultDecl::Fixed { fixed, value } => {
                        if *fixed {
                            write!(writer, " FIXED")?;
                        }
                        write!(writer, "=\"{value}\"")?;
                    }
                }

                writeln!(writer)?;
            }
        }
    }

    for entity in ctx.entities.values() {
        writeln!(writer, "{:>indent$}EntityDecl ({})", "", entity.name)?;
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

    for notation in &ctx.notations {
        let name = notation.0;
        let Notation { system_id, public_id } = notation.1;

        write!(writer, "{:>indent$}NotationDecl ({}) ", "", name)?;

        match (public_id, system_id) {
            (None, None) => {}
            // 1: PUBLIC PubidLiteral
            (Some(public_literal), None) => {
                write!(writer, "PUBLIC \"{public_literal}\"")?;
            }
            // 2: SYSTEM SystemLiteral
            (None, Some(system_literal)) => {
                write!(writer, "SYSTEM \"{system_literal}\"")?;
            }
            // 3: PUBLIC PubidLiteral SystemLiteral
            (Some(public_literal), Some(system_literal)) => {
                write!(writer, "PUBLIC \"{public_literal}\" \"{system_literal}\"")?;
            }
        }

        writeln!(writer)?;
    }

    *indent -= tab_size;
    *indent -= tab_size;

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

use crate::Attribute;
use crate::ContentSpec;
use crate::Context;
use crate::EntityType;
use crate::Namespace;
use crate::Node;
use crate::NodeKind;
use crate::Notation;
use crate::QName;

use std::io::Write;

pub struct PrettyPrinterBuilder<W>
where
    W: std::io::Write,
{
    pub tab_size: Option<usize>,
    pub print_dtd: Option<bool>,
    pub print_comments: Option<bool>,
    pub print_doc_info: Option<bool>,
    pub writer: std::io::BufWriter<W>,
}

impl<W> PrettyPrinterBuilder<W>
where
    W: std::io::Write,
{
    pub fn new(writer: W) -> Self {
        Self {
            tab_size: None,
            print_dtd: None,
            print_comments: None,
            print_doc_info: None,
            writer: std::io::BufWriter::new(writer),
        }
    }

    #[allow(unused)]
    pub fn tab_size(mut self, tab_size: usize) -> Self {
        self.tab_size = Some(tab_size);
        self
    }

    #[allow(unused)]
    pub fn print_dtd(mut self, print_dtd: bool) -> Self {
        self.print_dtd = Some(print_dtd);
        self
    }

    #[allow(unused)]
    pub fn print_comments(mut self, print_comments: bool) -> Self {
        self.print_comments = Some(print_comments);
        self
    }

    #[allow(unused)]
    pub fn print_doc_info(mut self, print_doc_info: bool) -> Self {
        self.print_doc_info = Some(print_doc_info);
        self
    }

    pub fn pretty_print(self, ctx: &Context) -> std::io::Result<()> {
        let mut printer = PrettyPrinter {
            tab_size: self.tab_size.unwrap_or(2),
            print_dtd: self.print_dtd.unwrap_or(true),
            print_comments: self.print_comments.unwrap_or(true),
            print_doc_info: self.print_doc_info.unwrap_or(true),
            writer: self.writer,
        };

        printer.pretty_print(ctx)?;
        Ok(())
    }
}

pub struct PrettyPrinter<W>
where
    W: std::io::Write,
{
    pub tab_size: usize,
    pub print_dtd: bool,
    pub print_comments: bool,
    pub print_doc_info: bool,
    pub writer: std::io::BufWriter<W>,
}

impl<W> PrettyPrinter<W>
where
    W: std::io::Write,
{
    pub fn pretty_print(&mut self, ctx: &Context) -> std::io::Result<()>
    where
        W: std::io::Write,
    {
        let mut indent = 0;
        writeln!(self.writer, "Document")?;

        let mut has_xml_decl = false;
        let mut nodes = ctx.doc.nodes.iter();
        let Some(first) = nodes.next() else { return Ok(()) };

        if let NodeKind::Declaration {
            version,
            encoding,
            standalone,
        } = first.data
        {
            if self.print_doc_info {
                indent += self.tab_size;
                writeln!(self.writer, "{:>indent$}Version: {version}", "")?;
                if let Some(encoding) = encoding {
                    writeln!(self.writer, "{:>indent$}Encoding: {encoding}", "")?;
                }
                if let Some(standalone) = standalone {
                    writeln!(self.writer, "{:>indent$}Standalone: {standalone}", "")?;
                }

                indent -= self.tab_size;
            }
            has_xml_decl = true;
        }

        if self.print_dtd {
            self.pp_dtd(&mut indent, ctx)?;
        }

        if !has_xml_decl {
            self.pp_node(&mut indent, first)?;
        }

        for node in nodes {
            self.pp_node(&mut indent, node)?;
        }

        Ok(())
    }

    fn pp_node(&mut self, indent: &mut usize, node: &Node) -> std::io::Result<()>
    where
        W: std::io::Write,
    {
        match &node.data {
            NodeKind::Declaration { .. } => {}
            NodeKind::Element {
                name,
                attributes,
                namespaces,
                children,
            } => self.pp_element(indent, name, attributes, namespaces, children)?,
            NodeKind::ProcessingInstruction { target, data } => self.pp_pi(indent, target, data)?,
            NodeKind::Text(text) => self.pp_text(indent, text)?,
            NodeKind::Comment(comment) => {
                if self.print_comments {
                    self.pp_comment(indent, comment)?;
                }
            }
            NodeKind::CData(data) => self.pp_cdata(indent, data)?,
        }
        Ok(())
    }

    fn pp_cdata(&mut self, indent: &mut usize, data: &str) -> std::io::Result<()>
    where
        W: std::io::Write,
    {
        *indent += self.tab_size;
        writeln!(self.writer, "{:>indent$}CData {}", "", data)?;
        *indent -= self.tab_size;
        Ok(())
    }

    fn pp_pi(&mut self, indent: &mut usize, target: &str, data: &Option<&str>) -> std::io::Result<()>
    where
        W: std::io::Write,
    {
        *indent += self.tab_size;
        write!(self.writer, "{:>indent$}PI ({})", "", target)?;
        if let Some(data) = data {
            write!(self.writer, " {data}")?;
        }
        writeln!(self.writer)?;
        *indent -= self.tab_size;
        Ok(())
    }

    fn pp_comment(&mut self, indent: &mut usize, comment: &str) -> std::io::Result<()>
    where
        W: std::io::Write,
    {
        *indent += self.tab_size;
        writeln!(
            self.writer,
            "{:>indent$}Comment {}",
            "",
            comment.replace("\n", "").trim()
        )?;
        *indent -= self.tab_size;
        Ok(())
    }

    fn pp_text(&mut self, indent: &mut usize, text: &str) -> std::io::Result<()>
    where
        W: std::io::Write,
    {
        *indent += self.tab_size;
        writeln!(self.writer, "{:>indent$}Text {}", "", text.trim())?;
        *indent -= self.tab_size;
        Ok(())
    }

    fn pp_element(
        &mut self,
        indent: &mut usize,
        qname: &QName,
        attributes: &Vec<Attribute>,
        namespaces: &Vec<Namespace>,
        children: &Vec<Node>,
    ) -> std::io::Result<()>
    where
        W: std::io::Write,
    {
        *indent += self.tab_size;

        write!(self.writer, "{:>indent$}Element ", "")?;
        if let Some(prefix) = qname.prefix {
            write!(self.writer, "{prefix}:")?;
        }

        writeln!(self.writer, "{}", qname.local)?;

        for namespace in namespaces {
            self.pp_namespace(indent, namespace)?;
        }

        for attribute in attributes {
            self.pp_attribute(indent, attribute)?;
        }

        for child in children {
            self.pp_node(indent, child)?;
        }

        *indent -= self.tab_size;

        Ok(())
    }

    fn pp_attribute(&mut self, indent: &mut usize, attribute: &Attribute) -> std::io::Result<()>
    where
        W: std::io::Write,
    {
        *indent += self.tab_size;
        write!(self.writer, "{:>indent$}Attribute ", "")?;
        if let Some(prefix) = attribute.qname.prefix {
            write!(self.writer, "{prefix}:")?;
        }
        writeln!(self.writer, "{}=\"{}\"", attribute.qname.local, attribute.value)?;
        *indent -= self.tab_size;

        Ok(())
    }

    fn pp_namespace(&mut self, indent: &mut usize, namespace: &Namespace) -> std::io::Result<()>
    where
        W: std::io::Write,
    {
        *indent += self.tab_size;

        let name = namespace.name.map_or("default", |n| n);
        writeln!(
            self.writer,
            "{:>indent$}Namespace ({}) uri=\"{}\"",
            "", name, namespace.uri
        )?;

        *indent -= self.tab_size;

        Ok(())
    }

    fn pp_dtd(&mut self, indent: &mut usize, ctx: &Context) -> std::io::Result<()>
    where
        W: std::io::Write,
    {
        *indent += self.tab_size;
        write!(self.writer, "{:>indent$}DTD", "")?;
        if let Some(doc_name) = ctx.doc.name {
            write!(self.writer, " ({doc_name})")?;
        }
        writeln!(self.writer)?;

        *indent += self.tab_size;

        for element_decl in &ctx.element_types {
            write!(
                self.writer,
                "{:>indent$}ElementDecl ({}) {}",
                "",
                element_decl.name,
                content_spec(&element_decl.content_spec)
            )?;

            if let Some(raw) = element_decl.raw {
                // Remove any repetative whitespace
                let words = raw.split_whitespace().collect::<Vec<_>>();
                write!(self.writer, " {}", words.join(" "))?;
            }

            writeln!(self.writer)?;
        }

        for attr_decl in ctx.attr_decls.values() {
            if let Some(att_defs) = &attr_decl.att_defs {
                for att_def in att_defs {
                    write!(
                        self.writer,
                        "{:>indent$}AttrDecl ({}) {}",
                        "", attr_decl.element_name, att_def.name
                    )?;

                    match &att_def.att_type {
                        crate::AttType::String => write!(self.writer, " CDATA")?,
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
                            write!(self.writer, " {tt}")?;
                        }
                        crate::AttType::Enumerated(enumerated_type) => match enumerated_type {
                            crate::EnumeratedType::NotationType(items) => {
                                write!(self.writer, " NOTATION ({})", items.join(" | "))?;
                            }
                            crate::EnumeratedType::Enumeration(items) => {
                                write!(self.writer, " ENUMERATION ({})", items.join(" | "))?;
                            }
                        },
                    }

                    match &att_def.default_decl {
                        crate::DefaultDecl::Required => write!(self.writer, " REQUIRED")?,
                        crate::DefaultDecl::Implied => write!(self.writer, " IMPLIED")?,
                        crate::DefaultDecl::Fixed { fixed, value } => {
                            if *fixed {
                                write!(self.writer, " FIXED")?;
                            }
                            write!(self.writer, "=\"{value}\"")?;
                        }
                    }

                    writeln!(self.writer)?;
                }
            }
        }

        for entity in ctx.entities.values() {
            writeln!(self.writer, "{:>indent$}EntityDecl ({})", "", entity.name)?;
            *indent += self.tab_size;
            match entity.entity_type {
                EntityType::InternalGeneral { value } => {
                    writeln!(self.writer, "{:>indent$}INTERNAL_GENERAL ({value})", "")?
                }
                EntityType::InternalParameter { value } => {
                    writeln!(self.writer, "{:>indent$}INTERNAL_PARAMETER ({value})", "")?
                }
                EntityType::InternalPredefined { value } => {
                    writeln!(self.writer, "{:>indent$}INTERNAL_PREDEFINED ({value})", "")?
                }
                EntityType::ExternalGeneralParsed { system_id, public_id } => {
                    writeln!(self.writer, "{:>indent$}EXTERNAL_GENERAL_PARSED", "")?;
                    *indent += self.tab_size;
                    writeln!(self.writer, "{:>indent$}SystemId ({system_id})", "")?;
                    if let Some(public_id) = public_id {
                        writeln!(self.writer, "{:>indent$}PublicId ({public_id})", "")?;
                    }
                    *indent -= self.tab_size;
                }
                EntityType::ExternalGeneralUnparsed {
                    system_id,
                    public_id,
                    ndata,
                } => {
                    writeln!(self.writer, "{:>indent$}EXTERNAL_GENERAL_UNPARSED", "")?;
                    *indent += self.tab_size;
                    writeln!(self.writer, "{:>indent$}SystemId ({system_id})", "")?;
                    if let Some(public_id) = public_id {
                        writeln!(self.writer, "{:>indent$}PublicId ({public_id})", "")?;
                    }
                    writeln!(self.writer, "{:>indent$}NDATA: {ndata}", "")?;
                    *indent -= self.tab_size;
                }
                EntityType::ExternalParameter { system_id, public_id } => {
                    writeln!(self.writer, "{:>indent$}EXTERNAL_PARAMETER", "")?;
                    *indent += self.tab_size;
                    writeln!(self.writer, "{:>indent$}SystemId ({system_id})", "")?;
                    if let Some(public_id) = public_id {
                        writeln!(self.writer, "{:>indent$}PublicId: ({public_id})", "")?;
                    }
                    *indent -= self.tab_size;
                }
            }

            *indent -= self.tab_size;
        }

        for notation in &ctx.notations {
            let name = notation.0;
            let Notation { system_id, public_id } = notation.1;

            write!(self.writer, "{:>indent$}NotationDecl ({}) ", "", name)?;

            match (public_id, system_id) {
                (None, None) => {}
                // 1: PUBLIC PubidLiteral
                (Some(public_literal), None) => {
                    write!(self.writer, "PUBLIC \"{public_literal}\"")?;
                }
                // 2: SYSTEM SystemLiteral
                (None, Some(system_literal)) => {
                    write!(self.writer, "SYSTEM \"{system_literal}\"")?;
                }
                // 3: PUBLIC PubidLiteral SystemLiteral
                (Some(public_literal), Some(system_literal)) => {
                    write!(self.writer, "PUBLIC \"{public_literal}\" \"{system_literal}\"")?;
                }
            }

            writeln!(self.writer)?;
        }

        *indent -= self.tab_size;
        *indent -= self.tab_size;

        Ok(())
    }
}

fn content_spec(content_spec: &ContentSpec) -> &'static str {
    match content_spec {
        ContentSpec::Empty => "EMPTY",
        ContentSpec::Any => "ANY",
        ContentSpec::MixedContent(_, _) => "MIXED",
        ContentSpec::ElementContent(_) => "ELEMENT",
    }
}

use crate::document::{DTD, Document, Encoding, Notation, XmlVersion};

#[derive(Default, Debug)]
pub struct Context<'a> {
    // path: Option<PathBuf>,
    pub doc: Document<'a>,

    /// Peform validation checks alongside the standard well-formedness checks
    pub validate: bool,

    /// XML version
    ///
    /// While any 1.x version is accepted, documents will be parsed as if they are
    /// Version 1.0
    pub version: XmlVersion,

    /// Document encoding
    pub encoding: Encoding<'a>,

    /// Standalone Document
    pub standalone: bool,
}

impl<'a> Context<'a> {
    pub(crate) fn initialize_dtd(&mut self, name: &'a str) {
        self.doc.dtd = Some(DTD::new(name));
    }

    pub(crate) fn set_version(&mut self, version: &'a str) {
        if !version.starts_with("1.") {
            panic!("invalid version {version}");
        }

        if version == "1.0)" {
            // no-op, default
        } else if version == "1.1" {
            // emit warning for cli?
            self.version = XmlVersion::V1_1;
        } else {
            // emit warning for cli?
        }
    }

    pub(crate) fn set_encoding(&mut self, encoding: &'a str) {
        if encoding.is_empty() {
            panic!("empty encoding");
        }

        // default
        if encoding == "UTF-8" {
            return;
        } else if encoding == "UTF-16" {
            self.encoding = Encoding::Utf16
        } else {
            // Encoding name may contain only Latin characters
            // [A-Za-z] ([A-Za-z0-9._] | '-')*
            let mut bytes = encoding.bytes();
            if let Some(first) = bytes.next()
                && !matches!(first, b'A'..=b'Z' | b'a'..=b'z')
            {
                panic!("invalid encoding")
            }

            for b in bytes {
                if !matches!(b, b'A'..=b'Z' | b'a'..=b'z' | b'.' | b'_' | b'-') {
                    panic!("invalid encoding")
                }
            }

            self.encoding = Encoding::Other(encoding);
        }
    }

    pub(crate) fn set_standalone(&mut self, standalone: &'a str) {
        if standalone == "yes" {
            self.standalone = true;
        } else if standalone == "no" {
            // no-op, default
        } else {
            panic!("invalid standalone {standalone}");
        }
    }

    pub(crate) fn emit_notation_decl(&mut self, name: &'a str, public_id: Option<&'a str>, system_id: Option<&'a str>) {
        let Some(dtd) = self.doc.dtd.as_mut() else {
            return;
        };

        // No entity names, processing instruction targets, or notation names contain any colons.
        if name.contains(":") {
            panic!("colons are not allowed in notation names!")
        }

        if self.validate && dtd.notations.contains_key(name) {
            panic!("duplicate notation declared!");
        }

        dtd.notations.insert(name, Notation::new(public_id, system_id));
    }
}

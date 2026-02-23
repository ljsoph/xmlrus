use std::collections::HashMap;

#[derive(Clone, Debug, Default)]
pub struct Document<'a> {
    pub name: Option<&'a str>,
    pub dtd: Option<DTD<'a>>,
}

impl<'a> Document<'a> {
    pub fn new() -> Self {
        Self { name: None, dtd: None }
    }
}

#[derive(Copy, Clone, Debug, Default)]
pub enum XmlVersion {
    #[default]
    V1_0,
    V1_1,
}

#[derive(Copy, Clone, Debug, Default)]
pub enum Encoding<'a> {
    #[default]
    Utf8,
    Utf16,
    Other(&'a str),
}

#[derive(Clone, Debug, Default)]
pub struct DTD<'a> {
    pub name: &'a str,
    pub entities: HashMap<&'a str, Entity<'a>>,
    pub notations: HashMap<&'a str, Notation<'a>>,
}

impl<'a> DTD<'a> {
    pub(crate) fn new(name: &'a str) -> Self {
        Self {
            name,
            entities: HashMap::new(),
            notations: HashMap::new(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Notation<'a> {
    pub public_id: Option<&'a str>,
    pub system_id: Option<&'a str>,
}

impl<'a> Notation<'a> {
    pub(crate) fn new(public_id: Option<&'a str>, system_id: Option<&'a str>) -> Self {
        Self { public_id, system_id }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Entity<'a> {
    name: &'a str,
    entity_type: EntityType<'a>,
    expanding: bool,
}

impl<'a> Entity<'a> {
    fn new(name: &'a str, entity_type: EntityType<'a>) -> Self {
        Self {
            name,
            entity_type,
            expanding: false,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum EntityType<'a> {
    InternalGeneral {
        value: &'a str,
    },
    InternalParameter {
        value: &'a str,
    },
    // TODO
    InternalPredefined {
        value: char,
    },
    ExternalGeneralParsed {
        system_id: Option<&'a str>,
        public_id: Option<&'a str>,
    },
    ExternalGeneralUnparsed {
        system_id: Option<&'a str>,
        public_id: Option<&'a str>,
        ndata: &'a str,
    },
    ExternalParameter {
        system_id: Option<&'a str>,
        public_id: Option<&'a str>,
    },
}

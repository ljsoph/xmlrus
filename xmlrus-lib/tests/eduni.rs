mod eduni_namespaces {
    fn test(test_num: &str) -> Result<(), xmlrus::error::Error> {
        xmlrus::Parser::parse(format!("tests/eduni/namespaces/1.0/{test_num}.xml"))
    }

    #[test]
    fn ns10_001_valid() {
        // Namespace name test: a perfectly good http URI
        assert!(test("001").is_ok());
    }

    #[test]
    fn ns10_002_valid() {
        // Namespace name test: a syntactically plausible URI with a fictitious scheme
        assert!(test("002").is_ok());
    }

    #[test]
    fn ns10_003_valid() {
        // Namespace name test: a perfectly good http URI with a fragment
        assert!(test("003").is_ok());
    }

    #[test]
    fn ns10_004_error() {
        // Namespace name test: a relative URI (deprecated)
        assert!(test("004").is_err());
    }

    #[test]
    fn ns10_005_error() {
        // Namespace name test: a same-document relative URI (deprecated)
        assert!(test("005").is_err());
    }

    #[test]
    fn ns10_006_error() {
        // Namespace name test: an http IRI that is not a URI
        assert!(test("006").is_err());
    }

    #[test]
    fn ns10_007_valid() {
        // Namespace inequality test: different capitalization
        assert!(test("007").is_ok());
    }

    #[test]
    fn ns10_008_valid() {
        // Namespace inequality test: different escaping
        assert!(test("008").is_ok());
    }

    #[test]
    fn ns10_009_not_wf() {
        // Namespace equality test: plain repetition
        assert!(test("009").is_err());
    }

    #[test]
    fn ns10_010_not_wf() {
        // Namespace equality test: use of character reference
        assert!(test("010").is_err());
    }

    #[test]
    fn ns10_011_not_wf() {
        // Namespace equality test: use of entity reference
        assert!(test("011").is_err());
    }

    #[test]
    fn ns10_012_not_wf() {
        // Namespace inequality test: equal after attribute value normalization
        assert!(test("012").is_err());
    }

    #[test]
    fn ns10_013_not_wf() {
        // Bad QName syntax: multiple colons
        assert!(test("013").is_err());
    }

    #[test]
    fn ns10_014_not_wf() {
        // Bad QName syntax: colon at end
        assert!(test("014").is_ok());
    }

    #[test]
    fn ns10_015_not_wf() {
        // Bad QName syntax: colon at start
        assert!(test("015").is_err());
    }

    #[test]
    fn ns10_016_not_wf() {
        // Bad QName syntax: xmlns:
        assert!(test("016").is_err());
    }

    #[test]
    fn ns10_017_valid() {
        // Simple legal case: no namespaces
        assert!(test("017").is_ok());
    }

    #[test]
    fn ns10_018_valid() {
        // Simple legal case: default namespace
        assert!(test("018").is_ok());
    }

    #[test]
    fn ns10_019_valid() {
        // Simple legal case: prefixed element
        assert!(test("019").is_ok());
    }

    #[test]
    fn ns10_020_valid() {
        // Simple legal case: prefixed attribute
        assert!(test("020").is_ok());
    }

    #[test]
    fn ns10_021_valid() {
        // Simple legal case: default namespace and unbinding
        assert!(test("021").is_ok());
    }

    #[test]
    fn ns10_022_valid() {
        // Simple legal case: default namespace and rebinding
        assert!(test("022").is_ok());
    }

    #[test]
    fn ns10_023_not_wf() {
        // Illegal use of 1.1-style prefix unbinding in 1.0 document
        assert!(test("023").is_err());
    }

    #[test]
    fn ns10_024_valid() {
        // Simple legal case: prefix rebinding
        assert!(test("024").is_ok());
    }

    #[test]
    fn ns10_025_not_wf() {
        // Unbound element prefix
        assert!(test("025").is_err());
    }

    #[test]
    fn ns10_026_not_wf() {
        // Unbound attribute prefix
        assert!(test("026").is_err());
    }

    #[test]
    fn ns10_027_valid() {
        // Reserved prefixes and namespaces: using the xml prefix undeclared
        assert!(test("027").is_err());
    }

    #[test]
    fn ns10_028_valid() {
        // Reserved prefixes and namespaces: declaring the xml prefix correctly
        assert!(test("028").is_ok());
    }

    #[test]
    fn ns10_029_not_wf() {
        // Reserved prefixes and namespaces: declaring the xml prefix incorrectly
        assert!(test("029").is_err());
    }

    #[test]
    fn ns10_030_not_wf() {
        // Reserved prefixes and namespaces: binding another prefix to the xml namespace
        assert!(test("030").is_err());
    }

    #[test]
    fn ns10_031_not_wf() {
        // Reserved prefixes and namespaces: declaring the xmlns prefix with its correct URI (illegal)
        assert!(test("031").is_err());
    }

    #[test]
    fn ns10_032_not_wf() {
        // Reserved prefixes and namespaces: declaring the xmlns prefix with an incorrect URI
        assert!(test("032").is_err());
    }

    #[test]
    fn ns10_033_not_wf() {
        // Reserved prefixes and namespaces: binding another prefix to the xmlns namespace
        assert!(test("033").is_err());
    }

    #[test]
    fn ns10_034_valid() {
        // Reserved prefixes and namespaces: binding a reserved prefix
        assert!(test("034").is_ok());
    }

    #[test]
    fn ns10_035_not_wf() {
        // Attribute uniqueness: repeated identical attribute
        assert!(test("035").is_err());
    }

    #[test]
    fn ns10_036_not_wf() {
        // Attribute uniqueness: repeated attribute with different prefixes
        assert!(test("036").is_err());
    }

    #[test]
    fn ns10_037_valid() {
        // Attribute uniqueness: different attributes with same local name
        assert!(test("037").is_ok());
    }

    #[test]
    fn ns10_038_valid() {
        // Attribute uniqueness: prefixed and unprefixed attributes with same local name
        assert!(test("038").is_ok());
    }

    #[test]
    fn ns10_039_valid() {
        // Attribute uniqueness: prefixed and unprefixed attributes with same local name, with default namespace
        assert!(test("039").is_ok());
    }

    #[test]
    fn ns10_040_valid() {
        // Attribute uniqueness: prefixed and unprefixed attributes with same local name,
        // with default namespace and element in default namespace
        assert!(test("040").is_ok());
    }

    #[test]
    fn ns10_041_valid() {
        // Attribute uniqueness: prefixed and unprefixed attributes with same local name,
        // element in same namespace as prefixed attribute
        assert!(test("041").is_ok());
    }

    #[test]
    fn ns10_042_not_wf() {
        // Colon in PI name
        assert!(test("042").is_err());
    }

    #[test]
    fn ns10_043_not_wf() {
        // Colon in entity name
        assert!(test("043").is_err());
    }

    #[test]
    fn ns10_044_not_wf() {
        // Colon in entity name
        assert!(test("044").is_err());
    }

    #[test]
    fn ns10_045_valid() {
        // Colon in ID attribute name
        assert!(test("045").is_ok());
    }

    #[test]
    fn ns10_046_valid() {
        // Colon in ID attribute name
        assert!(test("046").is_ok());
    }

    #[test]
    fn ns10_047_valid() {
        // Reserved name: _not_ an error
        assert!(test("047").is_ok());
    }

    #[test]
    fn ns10_048_valid() {
        // Reserved name: _not_ an error
        assert!(test("048").is_ok());
    }
}

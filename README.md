# XML'R'Us

A minimal XML Parser with partial namespace support (don't expect it to be perfect).

## Basic Usage

```rust
fn main() {
    let source = &std::fs::read_to_string("tests/test.xml").unwrap_or_default();
    let document = match xmlrus::Parser::parse(source) {
        Ok(document) => document,
        Err(err) => {
            eprintln!("{err}");
            std::process::exit(1);
        }
    };

    println!("{document:#?}");
}
```

## Examples

The following XML:
```xml
<?xml version="1.0" encoding="UTF-8"?>
<?target pi="instruction"?>
<manifest>
    <p xmlns:foo="foo">
        <q bar="baz"/>
    </p>
</manifest>
```

outputs

```shell
Document {
    nodes: [
        [0]Declaration {
            version: "1.0",
            encoding: Some(
                "UTF-8",
            ),
            standalone: None,
        },
        [1]ProcessingInstruction {
            target: "target",
            data: Some(
                "pi=\"instruction\"",
            ),
        },
        [2]Element {
            name: "manifest",
            attributes: [],
            namespaces: [],
            children: [
                [3]Element {
                    name: "p",
                    attributes: [],
                    namespaces: [
                        Namespace { name: Some("foo"), uri: foo },
                    ],
                    children: [
                        [4]Element {
                            name: "q",
                            attributes: [
                                Attribute { name: bar, value: baz },
                            ],
                            namespaces: [
                                Namespace { name: Some("foo"), uri: foo },
                            ],
                            children: [],
                        },
                    ],
                },
            ],
        },
    ],
    namespaces: [
        Namespace { name: Some("foo"), uri: foo },
    ],
}
```

---

The parser also aims to provide useful error messages for malformed XML documents (not 100% implemented)

```xml
<?xml version="1.0" encoding="UTF-8"?>
<manifest>
    <p xmlns:xml="foo"/>
</manifest>
```

outputs

```shell
error: Invalid namespace bound to 'xml' prefix at 3:19
   |
 3 |     <p xmlns:xml="foo"/>
   |                   ^^^
   |
```



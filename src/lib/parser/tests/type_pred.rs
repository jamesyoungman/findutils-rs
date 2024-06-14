use super::*;

#[test]
fn test_parse_type_valid() {
    let test_inputs: Vec<(&str, TypePredicateFileType)> = vec![
        ("b", TypePredicateFileType::BlockDevice),
        ("c", TypePredicateFileType::CharacterDevice),
        ("d", TypePredicateFileType::Directory),
        ("p", TypePredicateFileType::NamedPipe),
        ("f", TypePredicateFileType::RegularFile),
        ("l", TypePredicateFileType::SymbolicLink),
        ("s", TypePredicateFileType::Socket),
        ("D", TypePredicateFileType::Door),
    ];
    for (letter, _expected_indicator) in test_inputs {
        let input = &["-type", letter];
        match tokenize_single_predicate(input) {
            Err(e) => {
                panic!("failed to parse {input:?}: {e}");
            }
            Ok(Some((_, _boxed_predicate))) => {
                // TODO: verify the value of boxed_predicate.
            }
            other => {
                panic!("parsed valid input {input:?} got unexpected result {other:?}");
            }
        }
    }
}

#[cfg(test)]
mod tests {

    use crate::utils::MockDatabaseImpl;
    use crate::ParseDatabase;
    use errors::{pos::Span, FileDatabase};
    use insta::assert_debug_snapshot;
    use std::io::Write;
    use syntax::Token;
    use tempfile::NamedTempFile;

    fn get_tokens(input: &str) -> Vec<Span<Token>> {
        let mut file = NamedTempFile::new().unwrap();
        write!(file, "{}", input).unwrap();

        let db = MockDatabaseImpl::default();
        let handle = db.intern_file(file.path().to_path_buf());

        db.lex(handle).0
    }

    #[test]
    fn it_works() {
        let input = "fn main() {print(\"hello\")}";

        assert_debug_snapshot!(get_tokens(input))
    }
    #[test]
    fn lex_int() {
        let input = "12,34,,45,67,89,0";

        assert_debug_snapshot!(get_tokens(input))
    }

    #[test]
    fn lex_floats() {
        let input = "12.34,45.67,89.10";

        assert_debug_snapshot!(get_tokens(input))
    }

    #[test]
    fn lex_block_comments() {
        let input = "12 /*this is a comment */ 34";

        assert_debug_snapshot!(get_tokens(input))
    }

    #[test]
    fn lex_nested_block_comments() {
        let input = "/*this /*is a nested */  comment */";

        assert_debug_snapshot!(get_tokens(input))
    }

    #[test]
    fn lex_line_comment() {
        let input = "12 // this is a line comment";

        assert_debug_snapshot!(get_tokens(input))
    }
}

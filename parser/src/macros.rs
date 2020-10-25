#[macro_export]
macro_rules! test_parser {
    ($f_name:ident,$test:expr) => {
        #[test]
        fn $f_name() {
            let parser_output = $crate::utils::parse($test).0;

            insta::assert_snapshot!($crate::utils::dump_debug(&parser_output));
        }
    };
}

#[cfg(test)]

mod test {

    use semant::TyChecker;
    use util::env::TypeEnv;
    use util::pos::Spanned;
    use util::symbol::SymbolFactory;
    use syntax::ast::statement::Statement;
    use std::rc::Rc;
    use util::emmiter::Reporter;

    fn get_ast(
        input: &str,
        strings: Rc<SymbolFactory>,
        reporter: Reporter,
    ) -> Vec<Spanned<Statement>> {
        use syntax::lexer::Lexer;
        use syntax::parser::Parser;
        use util::symbol::Table;

        let tokens = Lexer::new(input, reporter.clone()).lex().unwrap();

        let mut symbols = Table::new(strings);
        Parser::new(tokens, reporter.clone(), &mut symbols)
            .parse()
            .unwrap()
    }

    #[test]
    #[should_panic]
    fn float_int() {
        let input = "123.0+456;";

        let strings = Rc::new(SymbolFactory::new());
        let mut env = TypeEnv::new(&strings);
        let reporter = Reporter::new();

        TyChecker::new(reporter.clone())
            .analyse(&get_ast(input, strings, reporter), &mut env)
            .unwrap();
    }

    #[test]
    #[should_panic]
    fn int_str() {
        let input = "10+\"h\";";
        let strings = Rc::new(SymbolFactory::new());
        let mut env = TypeEnv::new(&strings);
        let reporter = Reporter::new();
        TyChecker::new(reporter.clone())
            .analyse(&get_ast(input, strings, reporter), &mut env)
            .unwrap();
    }

    #[test]
    #[should_panic]
    fn float_str() {
        let input = "10.0+\"h\";";
        let strings = Rc::new(SymbolFactory::new());
        let mut env = TypeEnv::new(&strings);
        let reporter = Reporter::new();
        TyChecker::new(reporter.clone())
            .analyse(&get_ast(input, strings, reporter), &mut env)
            .unwrap();
    }

    #[test]
    #[should_panic]
    fn too_little_instance() {
        let input = "class Person {name:str,surname:str,age:int;fun hello(a:int,b:int){nil;}}
        var lenard = Person{name:\"Lenard\"};";
        let strings = Rc::new(SymbolFactory::new());
        let mut env = TypeEnv::new(&strings);
        let reporter = Reporter::new();
        TyChecker::new(reporter.clone())
            .analyse(&get_ast(input, strings, reporter), &mut env)
            .unwrap();
    }

    #[test]
    #[should_panic]
    fn wrong_set_instance_type() {
        let input = "class Person {name:str,surname:str,age:int;fun hello(a:int,b:int){nil;}}
        var lenard = Person{name:\"Lenard\"};lenard.name = 10";
        let strings = Rc::new(SymbolFactory::new());
        let mut env = TypeEnv::new(&strings);
        let reporter = Reporter::new();
        TyChecker::new(reporter.clone())
            .analyse(&get_ast(input, strings, reporter), &mut env)
            .unwrap();
    }

    #[test]

    fn set_instance_type() {
        let input = "class Person {name:str;}
        var lenard = Person{name:\"Lenard\"};lenard.name = \"h\";";
        let strings = Rc::new(SymbolFactory::new());
        let mut env = TypeEnv::new(&strings);
        let reporter = Reporter::new();

        TyChecker::new(reporter.clone())
            .analyse(&get_ast(input, strings, reporter), &mut env)
            .unwrap();
    }

    #[test]
    #[should_panic]
    fn too_many_instance() {
        let input = "class Person {name:str,surname:str,age:int;fun hello(a:int,b:int){nil;}}
        var lenard = Person{name:\"Lenard\",name:\"Lenard\",name:\"Lenard\",name:\"Lenard\"};";
        let strings = Rc::new(SymbolFactory::new());
        let mut env = TypeEnv::new(&strings);
        let reporter = Reporter::new();
        TyChecker::new(reporter.clone())
            .analyse(&get_ast(input, strings, reporter), &mut env)
            .unwrap();
    }

    #[test]
    fn func_closure_returning() {
        let input = "
        fun makePoint(x:int, y:int) -> fun(str) -> int {
            fun closure(method:str) -> int {
                if (method == \"x\") return x;
                if (method == \"y\") return y;
                print \"unknown method \" + method;
                return 0;
                }
            return closure;
        }
    var point = makePoint(2, 3);
    print point(\"x\"); 
    print point(\"y\"); ";
        let strings = Rc::new(SymbolFactory::new());
        let mut env = TypeEnv::new(&strings);
        let reporter = Reporter::new();

        TyChecker::new(reporter.clone())
            .analyse(&get_ast(input, strings, reporter), &mut env)
            .unwrap();
    }
    #[test]
    #[should_panic]
    fn wrong_body_type() {
        let input = "fun add(a:int,b:int) {return a+b;}";
        let strings = Rc::new(SymbolFactory::new());
        let mut env = TypeEnv::new(&strings);
        let reporter = Reporter::new();
        TyChecker::new(reporter.clone())
            .analyse(&get_ast(input, strings, reporter), &mut env)
            .unwrap();
    }

    #[test]
    #[should_panic]
    fn unary() {
        let input = "!true;!false!";
        let strings = Rc::new(SymbolFactory::new());
        let mut env = TypeEnv::new(&strings);
        let reporter = Reporter::new();

        TyChecker::new(reporter.clone())
            .analyse(&get_ast(input, strings, reporter), &mut env)
            .unwrap()
    }

    #[test]
    #[should_panic]
    fn missing_field() {
        let input = "class Person{name:str;} var john = Person {john:\"a\"};";
        let strings = Rc::new(SymbolFactory::new());
        let mut env = TypeEnv::new(&strings);
        let reporter = Reporter::new();
        TyChecker::new(reporter.clone())
            .analyse(&get_ast(input, strings, reporter), &mut env)
            .unwrap()
    }

    #[test]
    #[should_panic]
    fn wrong_unary_str() {
        let input = "!\"h\";";
        let strings = Rc::new(SymbolFactory::new());
        let mut env = TypeEnv::new(&strings);
        let reporter = Reporter::new();
        TyChecker::new(reporter.clone())
            .analyse(&get_ast(input, strings, reporter), &mut env)
            .unwrap();
    }

    #[test]
    fn recusive_fib() {
        let input = "fun fib(n:int) -> int {if (n < 2) return n;return fib(n - 2) + fib(n - 1);}";
        let strings = Rc::new(SymbolFactory::new());
        let mut env = TypeEnv::new(&strings);
        let reporter = Reporter::new();
        TyChecker::new(reporter.clone())
            .analyse(&get_ast(input, strings, reporter), &mut env)
            .unwrap();
    }

   

    #[test]
    fn str_index() {
        let input = "var a = \"h\"; a[0];";
        let strings = Rc::new(SymbolFactory::new());
        let mut env = TypeEnv::new(&strings);
        let reporter = Reporter::new();
        TyChecker::new(reporter.clone())
            .analyse(&get_ast(input, strings, reporter), &mut env)
            .unwrap();
    }

    #[test]
    #[should_panic]
    fn invalid_index_expr_int() {
        let input = "var a = 10; a[0];";
        let strings = Rc::new(SymbolFactory::new());
        let mut env = TypeEnv::new(&strings);
        let reporter = Reporter::new();
        TyChecker::new(reporter.clone())
            .analyse(&get_ast(input, strings, reporter), &mut env)
            .unwrap();
    }

    #[test]
    #[should_panic]
    fn invalid_index_expr_float() {
        let input = "var a = 10.0; a[0];";
        let strings = Rc::new(SymbolFactory::new());
        let mut env = TypeEnv::new(&strings);
        let reporter = Reporter::new();
        TyChecker::new(reporter.clone())
            .analyse(&get_ast(input, strings, reporter), &mut env)
            .unwrap();
    }

    #[test]
    #[should_panic]
    fn invalid_index_expr_true() {
        let input = "var a = true; a[0];";
        let strings = Rc::new(SymbolFactory::new());
        let mut env = TypeEnv::new(&strings);
        let reporter = Reporter::new();
        TyChecker::new(reporter.clone())
            .analyse(&get_ast(input, strings, reporter), &mut env)
            .unwrap();
    }

    #[test]
    #[should_panic]
    fn invalid_index_expr_false() {
        let input = "var a = false; a[0];";
        let strings = Rc::new(SymbolFactory::new());
        let mut env = TypeEnv::new(&strings);
        let reporter = Reporter::new();
        TyChecker::new(reporter.clone())
            .analyse(&get_ast(input, strings, reporter), &mut env)
            .unwrap();
    }

    #[test]
    #[should_panic]
    fn func_expr_fail() {
        let input = "var add = fun(a:int,b:int) -> int {a+b;};";
        let strings = Rc::new(SymbolFactory::new());
        let mut env = TypeEnv::new(&strings);
        let reporter = Reporter::new();
        TyChecker::new(reporter.clone())
            .analyse(&get_ast(input, strings, reporter), &mut env)
            .unwrap();
    }

    #[test]
    #[should_panic]
    fn invalid_assingment() {
        let input = "var a:[[int]] = [10];";
        let strings = Rc::new(SymbolFactory::new());
        let mut env = TypeEnv::new(&strings);
        let reporter = Reporter::new();
        TyChecker::new(reporter.clone())
            .analyse(&get_ast(input, strings, reporter), &mut env)
            .unwrap();
    }

    #[test]
    fn func_type_alias() {
        let input = "
        type Int = int;
        fun add(a:Int,b:Int) -> Int {
            return a+b;
        }
        add(10,10);";
        let strings = Rc::new(SymbolFactory::new());
        let mut env = TypeEnv::new(&strings);
        let reporter = Reporter::new();

        TyChecker::new(reporter.clone())
            .analyse(&get_ast(input, strings, reporter), &mut env)
            .unwrap();
    }

    #[test]
    fn func_type_alias_actual() {
        let input = "
        type Int = int;
        fun add(a:Int,b:Int) -> int {
            return a+b;
        }
        add(10,10);";
        let strings = Rc::new(SymbolFactory::new());
        let mut env = TypeEnv::new(&strings);
        let reporter = Reporter::new();

        TyChecker::new(reporter.clone())
            .analyse(&get_ast(input, strings, reporter), &mut env)
            .unwrap();
    }

    #[test]
    fn func_call() {
        let input = "fun add(a:int,b:int) -> int {return a+b;} add(10,10);";
        let strings = Rc::new(SymbolFactory::new());
        let mut env = TypeEnv::new(&strings);
        let reporter = Reporter::new();

        TyChecker::new(reporter.clone())
            .analyse(&get_ast(input, strings, reporter), &mut env)
            .unwrap();
    }

    #[test]
    fn assign_types() {
        let input =
            "var a = 10; var b = 10.0; var c = nil; var d = \"h\"; var e = true; var f = false; ";
        let strings = Rc::new(SymbolFactory::new());
        let mut env = TypeEnv::new(&strings);
        let reporter = Reporter::new();

        TyChecker::new(reporter.clone())
            .analyse(&get_ast(input, strings, reporter), &mut env)
            .unwrap();
    }

   /* #[test]
    fn array_types() {
        let input =
            "var a = [10]; var b = [10.0]; var c = [nil]; var d = [\"h\"]; var e = [true]; var f = [false]; ";
        let strings = Rc::new(SymbolFactory::new());
        let mut env = TypeEnv::new(&strings);
        let reporter = Reporter::new();

        TyChecker::new(reporter.clone())
            .analyse(&get_ast(input, strings, reporter), &mut env)
            .unwrap();
    }

     #[test]
    fn array_index() {
        let input = "var a = [10]; a[0];";
        let strings = Rc::new(SymbolFactory::new());
        let mut env = TypeEnv::new(&strings);
        let reporter = Reporter::new();
        TyChecker::new(reporter.clone())
            .analyse(&get_ast(input, strings, reporter), &mut env)
            .unwrap();
    } */

}

#[cfg(test)]
mod parser_tests {
    use crate::*;
    use scanner::Scanner;

    fn initialize_parser(source: &str) -> Parser {
        let tokens = Scanner::new(source, String::from("mega_test_file.lyc"), false).scan();
        Parser::new(tokens)
    }

    fn parse(source: &str) -> Parser {
        let parser = initialize_parser(source);
        parser.parse().unwrap()
    }

    fn number(number: f64) -> rug::Float {
        rug::Float::with_val(literal::PRECISION, number)
    }

    #[test]
    fn parse_simple_class() {
        let source = "
Program:
    Main = 42
";
        let result = parse(source);

        // Program class checks
        let program_class = result.classes.get(&String::from("Program")).unwrap();
        assert_eq!(program_class.name.as_ref(), "Program");

        // Main method checks
        let main_method = program_class
            .methods
            .get(&String::from("Main"))
            .unwrap()
            .first()
            .unwrap();
        assert_eq!(main_method.params, vec![]);
        assert_eq!(main_method.return_type, None);
        assert_eq!(
            main_method.body.kind,
            ExpressionKind::Literal(Literal::Integer(number(42.0)).into()).into()
        );
        assert!(main_method.decorator.is_empty());
    }

    #[test]
    fn parse_simple_class_with_eliminatable_expr() {
        let source = "
Program:
    Main = 420 + 69
";
        let result = parse(source);

        // Program class checks
        let program_class = result.classes.get(&String::from("Program")).unwrap();
        assert_eq!(program_class.name.as_ref(), "Program");

        // Main method checks
        let main_method = program_class
            .methods
            .get(&String::from("Main"))
            .unwrap()
            .first()
            .unwrap();
        assert_eq!(main_method.params, vec![]);
        assert_eq!(main_method.return_type, None);
        assert_eq!(
            main_method.body.kind,
            ExpressionKind::Literal(Literal::Integer(number(420.0 + 69.0)).into()).into()
        );
        assert!(main_method.decorator.is_empty());
    }

    #[test]
    fn parse_add() {
        let mut parser = initialize_parser("420 + 69");
        let result = parser.term().unwrap();

        assert!(matches!(
            *result.kind.clone(),
            ExpressionKind::Binary(
                Expression { kind: rhs, .. },
                Operator::Add,
                Expression { kind: lhs, .. }
            ) if rhs.as_ref() == &ExpressionKind::Literal(Literal::Integer(number(420.0)).into()) &&
                lhs.as_ref() == &ExpressionKind::Literal(Literal::Integer(number(69.0)).into())
        ));

        assert!(matches!(
            *Parser::eliminate_expr(result).kind,
            ExpressionKind::Literal(num) if num.as_ref() == &Literal::Integer(number(420.0 + 60.0)).into(),
        ));
    }

    #[test]
    fn parse_simply_call() {
        let mut parser = initialize_parser("call");
        let result = parser.call().unwrap();

        assert_eq!(
            *result.kind,
            ExpressionKind::Call {
                callee: None,
                caller: Rc::new(String::from("call")),
                args: vec![],
                block: None,
                call_type: CallType::default()
            }
        )
    }

    #[test]
    fn parse_simply_call_with_redundant_paranthesis() {
        let mut parser = initialize_parser("call()");
        let result = parser.call().unwrap();

        assert_eq!(
            *result.kind,
            ExpressionKind::Call {
                callee: None,
                caller: Rc::new(String::from("call")),
                args: vec![],
                block: None,
                call_type: CallType::default()
            }
        )
    }

    #[test]
    fn parse_call_with_args() {
        let mut parser = initialize_parser("call(5)");
        let result = parser.call().unwrap();

        assert!(matches!(
            *result.kind,
            ExpressionKind::Call {
                caller,
                args,
                ..
            } if caller.as_str() == "call" && matches!(args[0].clone(), Expression {
                kind: caller_kind,
                ..
            } if *caller_kind == ExpressionKind::Literal(Rc::new(Literal::Integer(number(5.0)))))
        ))
    }

    #[test]
    fn parse_method_call() {
        let mut parser = initialize_parser("function.method");
        let result = parser.call().unwrap();

        assert!(matches!(
            *result.kind,
            ExpressionKind::Call {
                callee,
                caller: method_call_name,
                ..
            } if method_call_name.as_str() == "method" && matches!(callee.clone().map(|expr| *expr.kind), Some(ExpressionKind::Call {
                caller: function_call_name,
                callee: None,
                ..
            }) if function_call_name.as_str() == "function")
        ))
    }

    #[test]
    fn parse_method_call_with_same_name() {
        let mut parser = initialize_parser("call.call");
        let result = parser.call().unwrap();

        assert!(matches!(
            *result.kind,
            ExpressionKind::Call {
                callee,
                caller: method_call_name,
                ..
            } if method_call_name.as_str() == "call" && matches!(callee.clone().map(|expr| *expr.kind), Some(ExpressionKind::Call {
                caller: function_call_name,
                callee: None,
                ..
            }) if function_call_name.as_str() == "call")
        ))
    }

    #[test]
    fn parse_match_expr() {
        let source = "
match x:
    42 => 42
    Integer => 69
";
        let mut parser = initialize_parser(source);

        let result = parser.expression().unwrap();

        let ExpressionKind::Match {
            scrutinee:
                Expression {
                    kind: scrutinee_caller,
                    ..
                },
            arms,
            ..
        } = *result.kind
        else {
            panic!("result.kind is not a match expr!");
        };

        let ExpressionKind::Call {
            caller: scrutinee_name,
            ..
        } = *scrutinee_caller
        else {
            panic!("scrutinee expected as a call!");
        };

        assert_eq!(scrutinee_name.as_str(), "x");

        assert!(matches!(&arms[0], (Pattern {
            name: PatternName::NoName,
            value: Expression { kind: pattern_value, .. },
            condition: None,
        }, Expression { kind: first_arm_kind, ..}) if (**first_arm_kind == ExpressionKind::Literal(Literal::Integer(number(42.0)).into())) &&
                         (**pattern_value == ExpressionKind::Literal(Literal::Integer(number(42.0)).into()))
        ));
    }

    #[test]
    fn test_state_declarations() {
        let source = "
Connection:
    Connected
    Disconnected
    Error(String)
    DetailedError(code: Integer, message: String)
";
        let result = initialize_parser(source).parse().unwrap();

        let connection_class = result.classes.get(&String::from("Connection")).unwrap();

        assert_eq!(connection_class.name.as_str(), "Connection");
        assert_eq!(connection_class.ancestors, vec![]);

        // Check constructors
        let constructors = &connection_class.constructors;

        // Connected constructor
        assert_eq!(
            constructors[0],
            (Rc::new(String::from("Connected")), vec![])
        );

        // Disconnected constructor
        assert_eq!(
            constructors[1],
            (Rc::new(String::from("Disconnected")), vec![])
        );

        assert_eq!(constructors[2].0, Rc::new(String::from("Error")));

        // Error constructor
        assert!(matches!(
            &constructors[2].1[0],
                    Pattern {
                        name: PatternName::NoName,
                        value: Expression {
                            kind,
                            ..
                        },
                        condition: None
                    } if matches!(*kind.clone(), ExpressionKind::Call { caller: name, .. } if name.as_str() == "String")
        ));

        // DetailedError Constructor
        assert_eq!(constructors[3].0, Rc::new(String::from("DetaliedError")));
        assert!(matches!(
            &constructors[3].1[0],
                    Pattern {
                        name: PatternName::Name(name),
                        value: Expression {
                            kind,
                            ..
                        },
                        condition: None
                    } if name.as_ref() == "code" && matches!(*kind.clone(), ExpressionKind::Call { caller: type_name, .. } if type_name.as_str() == "Integer")
        ));

        assert!(matches!(
            &constructors[3].1[1],
                    Pattern {
                        name: PatternName::Name(name),
                        value: Expression {
                            kind,
                            ..
                        },
                        condition: None
                    } if name.as_ref() == "message" && matches!(*kind.clone(), ExpressionKind::Call { caller: type_name, .. } if type_name.as_str() == "String")
        ));
    }

    #[test]
    fn test_empty_file() {
        let source = "";
        let result = initialize_parser(source).parse();

        assert!(result.is_ok());
        assert!(result.unwrap().classes.is_empty());
    }

    #[test]
    fn test_pattern_matching() {
        let source = "
match value:
    number: Integer when number > 0 => 42
    _: Integer => -1
";
        let mut parser = initialize_parser(source);

        let result = parser.expression().unwrap();

        let ExpressionKind::Match {
            scrutinee:
                Expression {
                    kind: scrutinee_caller,
                    ..
                },
            arms,
        } = *result.kind
        else {
            panic!("result.kind is not a match expr!");
        };

        let ExpressionKind::Call {
            caller: scrutinee_name,
            ..
        } = *scrutinee_caller
        else {
            panic!("scrutinee expected as a call!");
        };

        assert_eq!(scrutinee_name.as_str(), "value");

        assert!(matches!(&arms[0].0, Pattern {
            name: PatternName::Name(pattern_name),
            value: Expression { kind: pattern_value, .. },
            condition: Some(Expression { kind: condition_kind, ..  })
        } if pattern_name.as_str() == "number" &&
            matches!(
                *pattern_value.clone(), ExpressionKind::Call { caller: value_name, .. } if value_name.as_str() == "Integer"
            ) && matches!(
                *condition_kind.clone(), ExpressionKind::Binary(Expression { kind: left, .. }, Operator::Greater, Expression { kind: right, .. })
                    if (matches!(*left.clone(), ExpressionKind::Call { caller: condition_call_name, .. } if condition_call_name.as_str() == "number")) &&
                       *right.clone() == ExpressionKind::Literal(Rc::new(Literal::Integer(number(0.0)))
                    )
                )))
    }

    #[test]
    fn test_class_self_in_patterns() {
        let source = "
match self:
    self: Connected => Disconnected
    self: Disconnected => Connected
";
        let mut parser = initialize_parser(source);

        let result = parser.expression().unwrap();

        let ExpressionKind::Match {
            scrutinee:
                Expression {
                    kind: scrutinee_self,
                    ..
                },
            arms,
        } = *result.kind
        else {
            panic!("result.kind is not match!");
        };

        let ExpressionKind::ClassSelf = *scrutinee_self else {
            panic!("scrutinee expected as a class self");
        };

        assert_eq!(ExpressionKind::ClassSelf, *scrutinee_self);

        assert!(matches!(&arms[0].0, Pattern {
           name: PatternName::ClassSelf,
           value: Expression {
               kind: first_pattern_kind,
               ..
           },
           condition: None
        } if matches!(
            *first_pattern_kind.clone(),
            ExpressionKind::Call { caller, ..  } if caller.as_str() == "Connected"
        )));

        assert!(matches!(&arms[1].0, Pattern {
           name: PatternName::ClassSelf,
           value: Expression {
               kind: second_pattern_kind,
               ..
           },
           condition: None
        } if matches!(
            *second_pattern_kind.clone(),
            ExpressionKind::Call { caller, ..  } if caller.as_str() == "Disconnected"
        )));

        assert!(matches!(
            &arms[0].1,
            Expression { kind: call, .. } if matches!(
            *call.clone(),
            ExpressionKind::Call { caller, .. } if caller.as_str() == "Disconnected"
        )));

        assert!(matches!(
            &arms[1].1,
            Expression { kind: call, .. } if matches!(
            *call.clone(),
            ExpressionKind::Call { caller, .. } if caller.as_str() == "Connected"
        )));
    }
}

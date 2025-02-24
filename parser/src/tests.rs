#[cfg(test)]
mod parser_tests {
    use crate::*;
    use scanner::Scanner;
    use unicode_segmentation::UnicodeSegmentation;

    fn initialize_parser(source: &str) -> Parser {
        let graphemes = source.graphemes(true).collect::<Vec<&str>>();
        let tokens = Scanner::new(source, false).scan();
        Parser::new(graphemes, tokens)
    }

    fn parse(source: &str) -> Parser {
        let mut parser = initialize_parser(source);
        parser.parse().unwrap();
        parser
    }

    fn create_number(number: f64) -> rug::Float {
        rug::Float::with_val(literal::PRECISION, number)
    }

    fn prepare_class_scope_entity(parser: &mut Parser) {
        let entity = Entity {
            index: EntityIndex(1),
            name: "YellowCanary".to_string(),
            visibility: Visibility::Public,
            kind: EntityKind::Class,
            sub_entities: vec![],
        };

        parser.current_class_index = EntityIndex(1);
        parser.entities.insert(EntityIndex(1), entity);
        parser.global_entity.sub_entities.push(EntityIndex(1));
    }

    fn prepare_method_scope_entity(parser: &mut Parser) {
        prepare_class_scope_entity(parser);
        let entity = Entity {
            index: EntityIndex(2),
            name: "attack_canary".to_string(),
            visibility: Visibility::Protected,
            kind: EntityKind::Declaration,
            sub_entities: vec![],
        };

        parser.current_declaration_index = EntityIndex(2);
        parser.entities.insert(EntityIndex(2), entity);
        parser
            .entities
            .get_mut(&EntityIndex(1))
            .unwrap()
            .sub_entities
            .push(EntityIndex(2));
    }

    fn prepare_local_scope_entity(parser: &mut Parser) {
        prepare_method_scope_entity(parser);
        let entity = Entity {
            index: EntityIndex(3),
            name: "_canaries_can_not_attack".to_string(),
            visibility: Visibility::Private,
            kind: EntityKind::Local,
            sub_entities: vec![],
        };

        parser.current_local_index = EntityIndex(3);
        parser.entities.insert(EntityIndex(3), entity);
        parser
            .entities
            .get_mut(&EntityIndex(2))
            .unwrap()
            .sub_entities
            .push(EntityIndex(3));
    }

    #[test]
    fn parse_simple_class() {
        let source = "
Program:
    Main = 42
";
        let result = parse(source);

        // Program class checks
        let program_class = result.classes.get(&EntityIndex(1)).unwrap();
        assert_eq!(program_class.name, EntityIndex(1));

        // Main method checks
        let main_method = program_class
            .methods
            .get(&EntityIndex(2))
            .unwrap()
            .first()
            .unwrap();
        assert_eq!(main_method.params, vec![]);
        assert_eq!(main_method.return_type, None);
        assert_eq!(main_method.environment.as_ref().map(|e| e.len()), Some(0));
        assert_eq!(main_method.body, Expression::Literal(LiteralIndex(0)));
        assert!(main_method.decorator.is_empty());

        // Check literal value
        assert_eq!(
            result.get_literal(LiteralIndex(0)),
            &Literal::Integer(create_number(42.0))
        );
    }

    #[test]
    fn parse_simple_class_with_constexpr() {
        let source = "
Program:
    Main = 420 + 69
";
        let result = parse(source);

        // Program class checks
        let program_class = result.classes.get(&EntityIndex(1)).unwrap();
        assert_eq!(program_class.name, EntityIndex(1));

        // Main method checks
        let main_method = program_class
            .methods
            .get(&EntityIndex(2))
            .unwrap()
            .first()
            .unwrap();
        assert_eq!(main_method.params, vec![]);
        assert_eq!(main_method.return_type, None);
        assert_eq!(main_method.environment.as_ref().map(|e| e.len()), Some(0));
        assert_eq!(main_method.body, Expression::Literal(LiteralIndex(2)));
        assert!(main_method.decorator.is_empty());

        // Check constant expression result
        assert_eq!(
            result.get_literal(LiteralIndex(2)),
            &Literal::Integer(create_number(420.0 + 69.0))
        );
    }

    #[test]
    fn parse_add() {
        let mut parser = initialize_parser("420 + 69");
        let result = parser.term().unwrap();

        assert_eq!(
            parser.get_literal(LiteralIndex(0)),
            &Literal::Integer(create_number(420.0))
        );
        assert_eq!(
            parser.get_literal(LiteralIndex(1)),
            &Literal::Integer(create_number(69.0))
        );

        assert_eq!(
            result,
            Expression::Binary(
                Box::new(Expression::Literal(LiteralIndex(0))),
                Operator::Add,
                Box::new(Expression::Literal(LiteralIndex(1)))
            )
        );

        assert_eq!(
            parser.eval_constexpr(&result).unwrap(),
            Some(Expression::Literal(LiteralIndex(2)))
        );

        assert_eq!(
            parser.get_literal(LiteralIndex(2)),
            &Literal::Integer(create_number(420.0 + 69.0))
        );
    }

    #[test]
    fn parse_simply_call() {
        let mut parser = initialize_parser("call");
        prepare_local_scope_entity(&mut parser);
        let result = parser.call().unwrap();

        assert_eq!(
            result,
            Expression::Call {
                name_id: EntityIndex(4),
                caller: None,
                args: vec![],
                block: None
            }
        )
    }

    #[test]
    fn parse_simply_call_with_redundant_paranthesis() {
        let mut parser = initialize_parser("call()");
        prepare_local_scope_entity(&mut parser);
        let result = parser.call().unwrap();

        assert_eq!(
            result,
            Expression::Call {
                name_id: EntityIndex(4),
                caller: None,
                args: vec![],
                block: None
            }
        )
    }

    #[test]
    fn parse_call_with_args() {
        let mut parser = initialize_parser("call(5)");
        prepare_local_scope_entity(&mut parser);
        let result = parser.call().unwrap();

        assert_eq!(
            result,
            Expression::Call {
                name_id: EntityIndex(4),
                block: None,
                args: vec![Pattern {
                    name: PatternName::NoName,
                    value: Some(Expression::Literal(LiteralIndex(0))),
                    condition: None
                }],
                caller: None
            }
        )
    }

    #[test]
    fn parse_method_call() {
        let mut parser = initialize_parser("function.method");
        prepare_local_scope_entity(&mut parser);
        let result = parser.call().unwrap();

        assert_eq!(
            result,
            Expression::Call {
                caller: Some(Box::new(Expression::Call {
                    name_id: EntityIndex(4),
                    block: None,
                    args: vec![],
                    caller: None
                })),
                name_id: EntityIndex(5),
                args: vec![],
                block: None
            }
        )
    }

    #[test]
    fn parse_method_call_with_same_name() {
        let mut parser = initialize_parser("call.call");
        prepare_local_scope_entity(&mut parser);
        let result = parser.call().unwrap();

        assert_eq!(
            result,
            Expression::Call {
                caller: Some(Box::new(Expression::Call {
                    name_id: EntityIndex(4),
                    block: None,
                    args: vec![],
                    caller: None
                })),
                name_id: EntityIndex(5),
                args: vec![],
                block: None
            }
        )
    }

    #[test]
    fn parse_match_expr() {
        let source = "
match x:
    42 -> 42
    Integer -> 69
";
        let mut parser = initialize_parser(source);
        prepare_local_scope_entity(&mut parser);
        let result = parser.expression().unwrap();

        assert_eq!(
            result,
            Expression::Match {
                scrutinee: Box::new(Expression::Call {
                    caller: None,
                    name_id: EntityIndex(4),
                    args: vec![],
                    block: None
                }),
                arms: vec![
                    (
                        Pattern {
                            name: PatternName::NoName,
                            value: Some(Expression::Literal(LiteralIndex(0))),
                            condition: None
                        },
                        Expression::Literal(LiteralIndex(1))
                    ),
                    (
                        Pattern {
                            name: PatternName::NoName,
                            value: Some(Expression::Call {
                                caller: None,
                                name_id: EntityIndex(5),
                                args: vec![],
                                block: None
                            }),
                            condition: None
                        },
                        Expression::Literal(LiteralIndex(2))
                    )
                ]
            }
        );
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
        let mut parser = initialize_parser(source);
        parser.parse().unwrap();

        let connection_class = parser.classes.get(&EntityIndex(1)).unwrap();

        assert_eq!(connection_class.name, EntityIndex(1));
        assert_eq!(connection_class.ancestors, vec![]);

        // Check states
        let states = &connection_class.states;

        // Connected state
        assert_eq!(
            states[0],
            Statement::ClassState {
                name: EntityIndex(2),
                patterns: vec![]
            }
        );

        // Disconnected state
        assert_eq!(
            states[1],
            Statement::ClassState {
                name: EntityIndex(3),
                patterns: vec![]
            }
        );

        // Error state
        assert_eq!(
            states[2],
            Statement::ClassState {
                name: EntityIndex(4),
                patterns: vec![Pattern {
                    name: PatternName::NoName,
                    value: Some(Expression::Call {
                        caller: None,
                        name_id: EntityIndex(5),
                        args: vec![],
                        block: None
                    }),
                    condition: None
                }]
            }
        );

        // DetailedError state
        assert_eq!(
            states[3],
            Statement::ClassState {
                name: EntityIndex(6),
                patterns: vec![
                    Pattern {
                        name: PatternName::Name(EntityIndex(7)),
                        value: Some(Expression::Call {
                            caller: None,
                            name_id: EntityIndex(8),
                            args: vec![],
                            block: None
                        }),
                        condition: None
                    },
                    Pattern {
                        name: PatternName::Name(EntityIndex(9)),
                        value: Some(Expression::Call {
                            caller: None,
                            name_id: EntityIndex(10),
                            args: vec![],
                            block: None
                        }),
                        condition: None
                    }
                ]
            }
        );
    }

    #[test]
    fn test_empty_file() {
        let source = "";
        let mut parser = initialize_parser(source);
        let result = parser.parse();

        assert!(result.is_ok());
        assert!(parser.classes.is_empty());
    }

    #[test]
    fn test_pattern_matching() {
        let source = "
match value:
    number: Integer when number > 0 -> 42
    _: Integer -> -1
";
        let mut parser = initialize_parser(source);
        prepare_local_scope_entity(&mut parser);
        let result = parser.expression().unwrap();

        assert_eq!(
            result,
            Expression::Match {
                scrutinee: Box::new(Expression::Call {
                    caller: None,
                    name_id: EntityIndex(4),
                    args: vec![],
                    block: None
                }),
                arms: vec![
                    (
                        Pattern {
                            name: PatternName::Name(EntityIndex(5)),
                            value: Some(Expression::Call {
                                caller: None,
                                name_id: EntityIndex(6),
                                args: vec![],
                                block: None
                            }),
                            condition: Some(Expression::Binary(
                                Box::new(Expression::Call {
                                    caller: None,
                                    name_id: EntityIndex(5),
                                    args: vec![],
                                    block: None
                                }),
                                Operator::Greater,
                                Box::new(Expression::Literal(LiteralIndex(0)))
                            ))
                        },
                        Expression::Literal(LiteralIndex(1))
                    ),
                    (
                        Pattern {
                            name: PatternName::Name(EntityIndex(7)),
                            value: Some(Expression::Call {
                                caller: None,
                                name_id: EntityIndex(6),
                                args: vec![],
                                block: None
                            }),
                            condition: None
                        },
                        Expression::Literal(LiteralIndex(3))
                    )
                ]
            }
        );
    }

    #[test]
    fn test_class_self_in_patterns() {
        let source = "
match self:
    self: Connected -> Disconnected
    self: Disconnected -> Connected
";
        let mut parser = initialize_parser(source);
        prepare_local_scope_entity(&mut parser);
        let result = parser.expression().unwrap();

        assert_eq!(
            result,
            Expression::Match {
                scrutinee: Box::new(Expression::ClassSelf),
                arms: vec![
                    (
                        Pattern {
                            name: PatternName::ClassSelf,
                            value: Some(Expression::Call {
                                caller: None,
                                name_id: EntityIndex(4),
                                args: vec![],
                                block: None
                            }),
                            condition: None
                        },
                        Expression::Call {
                            caller: None,
                            name_id: EntityIndex(5),
                            args: vec![],
                            block: None
                        }
                    ),
                    (
                        Pattern {
                            name: PatternName::ClassSelf,
                            value: Some(Expression::Call {
                                caller: None,
                                name_id: EntityIndex(5),
                                args: vec![],
                                block: None
                            }),
                            condition: None
                        },
                        Expression::Call {
                            caller: None,
                            name_id: EntityIndex(4),
                            args: vec![],
                            block: None
                        }
                    )
                ]
            }
        );
    }
}

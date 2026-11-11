use syntra_lang::{
    Declaration, DiagnosticEngine, Expression, GoalMember, IntentValue, Lexer, Parser,
    UncertaintyMember, GovernanceMember,
};

fn parse(source: &str) -> syntra_lang::Program<'_> {
    let tokens = Lexer::new(source).tokenize().expect("lexing succeeds");
    let mut parser = Parser::new(&tokens);
    parser.parse_program().expect("parsing succeeds")
}

#[test]
fn parses_fully_qualified_cognitive_intent() {
    let source = r#"intent mission {
  motive "deliver safely"
  goal deploy {
    description "production release"
    priority 10
    condition ready && score >= 0.8
    target deploy()
  }
  effects { Pure, Network, IO }
  uncertainty {
    confidence 0.95
    assumption "staging matches production"
    evidence assess(threshold(0.8))
    source "telemetry"
  }
  governance {
    requires operator
    approval reviewer
    policy "production-change"
    constraint risk <= threshold(0.8)
  }
}
"#;

    let program = parse(source);
    let Declaration::Intent(intent) = &program.declarations[0] else {
        panic!("expected intent declaration");
    };

    assert_eq!(intent.name, "mission");
    assert_eq!(intent.members.len(), 5);

    assert!(matches!(&intent.members[0].value, IntentValue::String("deliver safely")));
    assert!(matches!(&intent.members[1].value, IntentValue::Goal(_)));
    assert!(matches!(&intent.members[2].value, IntentValue::Effects(_)));
    assert!(matches!(&intent.members[3].value, IntentValue::Uncertainty(_)));
    assert!(matches!(&intent.members[4].value, IntentValue::Governance(_)));

    let IntentValue::Goal(goal) = &intent.members[1].value else {
        panic!("expected goal");
    };
    assert_eq!(goal.name, "deploy");
    assert_eq!(goal.members.len(), 4);
    assert!(matches!(&goal.members[0], GoalMember::Description { .. }));
    assert!(matches!(&goal.members[1], GoalMember::Priority { .. }));
    assert!(matches!(&goal.members[2], GoalMember::Condition { .. }));
    assert!(matches!(&goal.members[3], GoalMember::Target { .. }));

    let IntentValue::Uncertainty(uncertainty) = &intent.members[3].value else {
        panic!("expected uncertainty");
    };
    assert_eq!(uncertainty.members.len(), 4);
    assert!(matches!(&uncertainty.members[0], UncertaintyMember::Confidence { .. }));
    assert!(matches!(&uncertainty.members[1], UncertaintyMember::Assumption { .. }));
    assert!(matches!(&uncertainty.members[2], UncertaintyMember::Evidence { .. }));
    assert!(matches!(&uncertainty.members[3], UncertaintyMember::Source { .. }));

    let UncertaintyMember::Evidence { value, .. } = &uncertainty.members[2] else {
        panic!("expected evidence expression");
    };
    let Expression::Call { arguments, .. } = value else {
        panic!("expected outer evidence call");
    };
    assert_eq!(arguments.len(), 1);
    assert!(matches!(arguments[0], Expression::Call { .. }));

    let IntentValue::Governance(governance) = &intent.members[4].value else {
        panic!("expected governance");
    };
    assert_eq!(governance.members.len(), 4);
    assert!(matches!(&governance.members[0], GovernanceMember::Requires { value: "operator", .. }));
    assert!(matches!(&governance.members[1], GovernanceMember::Approval { value: "reviewer", .. }));
    assert!(matches!(&governance.members[2], GovernanceMember::Policy { value: "production-change", .. }));
    assert!(matches!(&governance.members[3], GovernanceMember::Constraint { .. }));

    assert_eq!(intent.span.start, 0);
    assert_eq!(intent.span.end, source.len() - 1);
}

#[test]
fn cognitive_nodes_preserve_source_spans() {
    let source = "intent x { goal y { description \"ok\" } effects { Network } }";
    let program = parse(source);
    let Declaration::Intent(intent) = &program.declarations[0] else {
        panic!("expected intent");
    };

    let IntentValue::Goal(goal) = &intent.members[0].value else {
        panic!("expected goal");
    };
    assert_eq!(&source[goal.span.start..goal.span.end], "goal y { description \"ok\" }");

    let IntentValue::Effects(effects) = &intent.members[1].value else {
        panic!("expected effects");
    };
    assert_eq!(&source[effects.span.start..effects.span.end], "effects { Network }");
}

#[test]
fn malformed_cognitive_syntax_recovers_without_stalling() {
    let source = r#"intent broken {
  goal deploy { priority ; }
}
intent good {
  effects { Network }
  governance { requires operator }
}
"#;

    let tokens = Lexer::new(source).tokenize().expect("lexing succeeds");
    let mut parser = Parser::new(&tokens);
    let report = parser.parse_program_recovering();

    assert!(!report.errors.is_empty());
    assert_eq!(report.program.declarations.len(), 1);

    let Declaration::Intent(intent) = &report.program.declarations[0] else {
        panic!("expected recovered good intent");
    };
    assert_eq!(intent.name, "good");

    let diagnostics = DiagnosticEngine::parse_errors(source, report.errors);
    assert!(!diagnostics.is_empty());
    assert!(diagnostics.iter().all(|d| d.span.start <= d.span.end && d.span.end <= source.len()));
    assert!(diagnostics.iter().any(|d| d.render().contains("priority")));
}

#[test]
fn missing_cognitive_closing_brace_reports_eof() {
    let source = "intent broken { uncertainty { confidence 0.8";
    let tokens = Lexer::new(source).tokenize().expect("lexing succeeds");
    let mut parser = Parser::new(&tokens);

    let error = parser.parse_program().expect_err("missing brace must fail");
    let diagnostic = DiagnosticEngine::parse_error(source, error);

    assert_eq!(diagnostic.code, "SYN1001");
    assert!(diagnostic.render().contains("bytes"));
    assert!(diagnostic.render().contains("expected }"));
}

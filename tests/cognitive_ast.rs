use syntra_lang::ast::{
    EffectItem, Effects, Goal, GoalMember, Governance, GovernanceMember, Intent, IntentMember,
    IntentValue, Uncertainty, UncertaintyMember,
};
use syntra_lang::tokens::Span;

fn span(start: usize, end: usize) -> Span {
    Span::with_pos(start, end, 1, start + 1)
}

#[test]
fn cognitive_ast_preserves_borrowed_source_fields() {
    let source = "intent plan { goal deliver { description \"ship\" } }";
    let goal_name = &source[19..26];
    let description = &source[41..47];

    let goal = Goal {
        name: goal_name,
        members: vec![GoalMember::Description {
            value: description,
            span: span(29, 47),
        }],
        span: span(14, 49),
    };

    let intent = Intent {
        name: &source[7..11],
        members: vec![IntentMember {
            name: &source[14..18],
            value: IntentValue::Goal(goal),
            span: span(14, 49),
        }],
        span: span(0, source.len()),
    };

    let IntentMember { name, value, .. } = &intent.members[0];
    assert_eq!(*name, "goal");

    let IntentValue::Goal(goal) = value else {
        panic!("expected goal node");
    };
    assert_eq!(goal.name, "deliver");
    assert_eq!(goal.members.len(), 1);

    let GoalMember::Description { value, .. } = &goal.members[0] else {
        panic!("expected description");
    };
    assert_eq!(*value, "\"ship\"");
}

#[test]
fn cognitive_ast_has_distinct_effects_uncertainty_and_governance_nodes() {
    let effects = Effects {
        items: vec![
            EffectItem { name: "Pure", span: span(0, 4) },
            EffectItem { name: "Network", span: span(5, 12) },
        ],
        span: span(0, 12),
    };

    let uncertainty = Uncertainty {
        members: vec![
            UncertaintyMember::Confidence {
                value: syntra_lang::ast::Expression::Literal(
                    syntra_lang::ast::Literal::Float(0.75),
                    span(0, 4),
                ),
                span: span(0, 4),
            },
            UncertaintyMember::Assumption {
                value: "network is available",
                span: span(5, 26),
            },
        ],
        span: span(0, 26),
    };

    let governance = Governance {
        members: vec![
            GovernanceMember::Requires {
                value: "human",
                span: span(0, 5),
            },
            GovernanceMember::Approval {
                value: "reviewer",
                span: span(6, 14),
            },
        ],
        span: span(0, 14),
    };

    assert_eq!(effects.items[0].name, "Pure");
    assert_eq!(uncertainty.members.len(), 2);
    assert_eq!(governance.members.len(), 2);
}

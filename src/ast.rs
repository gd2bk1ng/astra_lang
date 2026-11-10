use crate::tokens::Span;

/// Borrowed syntax tree. Source-bearing fields borrow from the original input.
#[derive(Debug, PartialEq)]
pub struct Program<'a> { pub declarations: Vec<Declaration<'a>>, pub span: Span }

#[derive(Debug, PartialEq)]
pub enum Declaration<'a> { Function(Function<'a>), Intent(Intent<'a>), Rule(Rule<'a>) }

#[derive(Debug, PartialEq)]
pub struct Function<'a> { pub name: &'a str, pub params: Vec<Parameter<'a>>, pub return_type: Option<TypeRef<'a>>, pub body: Block<'a>, pub span: Span, pub grad: bool }

#[derive(Debug, PartialEq)]
pub struct Intent<'a> { pub name: &'a str, pub members: Vec<IntentMember<'a>>, pub span: Span }

#[derive(Debug, PartialEq)]
pub struct IntentMember<'a> { pub name: &'a str, pub value: IntentValue<'a>, pub span: Span }

#[derive(Debug, PartialEq)]
pub enum IntentValue<'a> {
    String(&'a str),
    Block(Block<'a>),
    Goal(Goal<'a>),
    Effects(Effects<'a>),
    Uncertainty(Uncertainty<'a>),
    Governance(Governance<'a>),
}

/// A named cognitive goal nested inside an intent.
#[derive(Debug, PartialEq)]
pub struct Goal<'a> {
    pub name: &'a str,
    pub members: Vec<GoalMember<'a>>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub enum GoalMember<'a> {
    Description { value: &'a str, span: Span },
    Priority { value: Expression<'a>, span: Span },
    Condition { value: Expression<'a>, span: Span },
    Target { value: Expression<'a>, span: Span },
}

/// Declared effect capabilities for a cognitive construct.
#[derive(Debug, PartialEq)]
pub struct Effects<'a> {
    pub items: Vec<EffectItem<'a>>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct EffectItem<'a> {
    pub name: &'a str,
    pub span: Span,
}

/// Structured uncertainty metadata. Semantics are intentionally deferred to verification.
#[derive(Debug, PartialEq)]
pub struct Uncertainty<'a> {
    pub members: Vec<UncertaintyMember<'a>>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub enum UncertaintyMember<'a> {
    Confidence { value: Expression<'a>, span: Span },
    Assumption { value: &'a str, span: Span },
    Evidence { value: Expression<'a>, span: Span },
    Source { value: &'a str, span: Span },
}

/// Authority and policy metadata. An approval field is not proof of actual approval.
#[derive(Debug, PartialEq)]
pub struct Governance<'a> {
    pub members: Vec<GovernanceMember<'a>>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub enum GovernanceMember<'a> {
    Requires { value: &'a str, span: Span },
    Approval { value: &'a str, span: Span },
    Policy { value: &'a str, span: Span },
    Constraint { value: Expression<'a>, span: Span },
}

#[derive(Debug, PartialEq)]
pub struct Rule<'a> { pub name: &'a str, pub params: Vec<Parameter<'a>>, pub return_type: Option<TypeRef<'a>>, pub body: Block<'a>, pub span: Span }

#[derive(Debug, PartialEq)]
pub struct Parameter<'a> { pub name: &'a str, pub ty: TypeRef<'a>, pub span: Span }

#[derive(Debug, PartialEq)]
pub struct TypeRef<'a> { pub name: &'a str, pub arguments: Vec<TypeRef<'a>>, pub span: Span }

#[derive(Debug, PartialEq)]
pub struct Block<'a> { pub statements: Vec<Statement<'a>>, pub span: Span }

#[derive(Debug, PartialEq)]
pub enum Statement<'a> { Let { name: &'a str, value: Option<Expression<'a>>, span: Span }, Return { value: Expression<'a>, span: Span }, Expression { expression: Expression<'a>, span: Span }, Backtrack { body: Block<'a>, span: Span }, Annotation(Annotation<'a>) }

#[derive(Debug, PartialEq)]
pub struct Annotation<'a> { pub name: &'a str, pub arguments: Vec<Expression<'a>>, pub span: Span }

#[derive(Debug, PartialEq)]
pub enum Expression<'a> { Identifier(&'a str, Span), Literal(Literal<'a>, Span), Call { callee: Box<Expression<'a>>, arguments: Vec<Expression<'a>>, span: Span }, Unary { operator: UnaryOp, operand: Box<Expression<'a>>, span: Span }, Binary { left: Box<Expression<'a>>, operator: BinaryOp, right: Box<Expression<'a>>, span: Span }, Block(Block<'a>), Symbolic { identifier: &'a str, span: Span }, SelfModify { target: &'a str, patch: Box<Expression<'a>>, span: Span } }

#[derive(Debug, PartialEq)]
pub enum Literal<'a> { Integer(i64), Float(f64), String(&'a str), Boolean(bool) }

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp { Negate, Not }

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp { Add, Subtract, Multiply, Divide, Equal, NotEqual, Less, LessEqual, Greater, GreaterEqual, And, Or }

#[derive(Debug)]
pub(super) enum SyntaxNodeKind {
    Name(String),
    Pattern(String),
    List(Vec<SyntaxNodeKind>),
    Optional(Box<SyntaxNodeKind>),
    Choice(Vec<SyntaxNodeKind>),
}

#[derive(Debug)]
pub(super) struct SyntaxNode {
    pub(super) name: String,
    pub(super) kind: SyntaxNodeKind,
}

#[derive(Debug)]
pub enum GrammarNodeKind {
    Rule(String),
    AssumedToken(String),
    List(Vec<GrammarNode>),
    Optional(Box<GrammarNode>),
    Choice(Vec<GrammarNode>, usize),
}

#[derive(Debug)]
pub struct GrammarNode {
    pub index: usize,
    pub kind: GrammarNodeKind,
}

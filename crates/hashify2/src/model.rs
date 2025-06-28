use unsynn::{
    Assign, Cached, FatArrow, Ident, Nothing, ParenthesisGroupContaining, ToTokens, TokenStream,
};

use crate::{If, Let};

pub(crate) struct LetExpr<E> {
    pub(crate) kw_let: Let,
    // yeah, let takes irrefutable patterns, don't care.
    pub(crate) id: Ident,
    pub(crate) kw_assign: Assign,
    pub(crate) expr: E,
}

impl<E> LetExpr<E> {
    pub(crate) fn new(id: Ident, expr: E) -> Self {
        Self { kw_let: Let(Cached::new("let")), id, kw_assign: Assign::new(), expr }
    }
}

impl<E> ToTokens for LetExpr<E>
where
    E: ToTokens,
{
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { kw_let, id, kw_assign, expr } = self;
        kw_let.to_tokens(tokens);
        id.to_tokens(tokens);
        kw_assign.to_tokens(tokens);
        expr.to_tokens(tokens);
    }
}

pub(crate) struct MethodCall<E = Nothing> {
    pub(crate) name: Ident,
    pub(crate) args: ParenthesisGroupContaining<E>,
}

impl<Args> MethodCall<Args> {
    pub(crate) const fn new(name: Ident, args: Args) -> Self {
        Self { name, args: ParenthesisGroupContaining::new(args) }
    }
}

impl<E> ToTokens for MethodCall<E>
where
    E: ToTokens,
{
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.name.to_tokens(tokens);
        self.args.to_tokens(tokens);
    }
}

pub(crate) struct IfGuard<E> {
    pub(crate) kw_if: If,
    pub(crate) expr: E,
}

impl<E> IfGuard<E> {
    pub fn new(expr: E) -> Self {
        Self { kw_if: If(Cached::new("if")), expr }
    }
}

impl<E> ToTokens for IfGuard<E>
where
    E: ToTokens,
{
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.kw_if.to_tokens(tokens);
        self.expr.to_tokens(tokens);
    }
}

pub(crate) struct MatchArm<P, E, G = Nothing> {
    pub(crate) pattern: P,
    pub(crate) if_guard: G,
    pub(crate) fat_arrow: FatArrow,
    pub(crate) body: E,
}

impl<P, E> MatchArm<P, E> {
    pub const fn new(pattern: P, fat_arrow: FatArrow, body: E) -> Self {
        Self { pattern, if_guard: Nothing, fat_arrow, body }
    }
}

impl<P, G, E> MatchArm<P, E, G> {
    pub const fn new_guarded(pattern: P, if_guard: G, fat_arrow: FatArrow, body: E) -> Self {
        Self { pattern, if_guard, fat_arrow, body }
    }
}

impl<P, G, E> ToTokens for MatchArm<P, G, E>
where
    P: ToTokens,
    G: ToTokens,
    E: ToTokens,
{
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.pattern.to_tokens(tokens);
        self.if_guard.to_tokens(tokens);
        self.fat_arrow.to_tokens(tokens);
        self.body.to_tokens(tokens);
    }
}

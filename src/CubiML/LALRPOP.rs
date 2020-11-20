use super::ast; // super instead of self because lalrpop wraps this in an internal module


grammar;

Ident: String = <r"[a-z_]\w*"> => String::from(<>);
Tag: String = <r"`[A-Z]\w*"> => String::from(<>);

// make sure __proto__ is not considered a valid identifier
Illegal = "__proto__";

SepList<T, Sep>: Vec<T> = {
    <v:(<T> Sep)*> <e:T> => {
        let mut v = v;
        v.push(e);
        v
    }
};
SepListOpt<T, Sep>: Vec<T> = {
    SepList<T, Sep>,
    => Vec::new(),
};

VarOrLiteral: Box<ast::Expr> = {
    Ident => Box::new(
        match <>.as_str() {
            "true" => ast::Expr::Literal(ast::Literal::Bool(true)),
            "false" => ast::Expr::Literal(ast::Literal::Bool(false)),
            _ => ast::Expr::Variable(<>)
        }
    ),
}

If: Box<ast::Expr> = {
    "if" <Expr> "then" <Expr> "else" <Expr> => Box::new(ast::Expr::If(<>)),
}

FuncDef: Box<ast::Expr> = {
    "fun" <Ident> "->" <Expr> => Box::new(ast::Expr::FuncDef(<>)),
}
Call: Box<ast::Expr> = {
    CallExpr CaseExpr => Box::new(ast::Expr::Call(<>)),
}


KeyPairExpr = {
    <Ident> "=" <Expr>,
}
Record: Box<ast::Expr> = {
    "{" <SepListOpt<KeyPairExpr, ";">> "}" => Box::new(ast::Expr::Record(<>)),
}
FieldAccess: Box<ast::Expr> = {
    <SimpleExpr> "." <Ident> => Box::new(ast::Expr::FieldAccess(<>)),
}

Case: Box<ast::Expr> = {
    <Tag> <CaseExpr> => Box::new(ast::Expr::Case(<>)),
}

CaseMatchPattern = {
    Tag Ident,
}
MatchArm = {
    <CaseMatchPattern> "->" <CallExpr>,
}
Match: Box<ast::Expr> = {
    "match" <Expr> "with" <SepList<MatchArm, "|">> => Box::new(ast::Expr::Match(<>)),
}

LetLHS = {
    "let" <Ident> "=" <Expr>,
}
LetRHS = {
    "in" <Expr>,
}
Let: Box<ast::Expr> = {
    <LetLHS> <LetRHS> => Box::new(ast::Expr::Let(<>)),
}


LetRecDef = {
    <Ident> "=" <FuncDef>,
}
LetRecLHS = {
    "let" "rec" <SepList<LetRecDef, "and">>,
}
LetRec: Box<ast::Expr> = {
     <LetRecLHS> <LetRHS> => Box::new(ast::Expr::LetRec(<>)),
}


SimpleExpr = {
    FieldAccess,
    Record,
    VarOrLiteral,
    "(" <Expr> ")",
}
CaseExpr = {
    SimpleExpr,
    Case,
}
CallExpr = {
    CaseExpr,
    Call,
}
Expr = {
    CallExpr,
    FuncDef,
    If,
    Let,
    LetRec,
    Match,
}

TopLevelItem: ast::TopLevel = {
    <LetLHS> => ast::TopLevel::LetDef(<>),
    <LetRecLHS> => ast::TopLevel::LetRecDef(<>),
    <Expr> => ast::TopLevel::Expr(*<>),
}

pub Script = {
   <SepList<TopLevelItem, ";">>
}
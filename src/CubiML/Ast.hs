module CubiML.Ast where

import Import

data Literal = LitBool Bool 
  deriving Show

type VarDefinition = (Text, Expr)
type CaseMatchPattern = (Text, Text)

data Expr
  = ExprCall Expr Expr
  | ExprCase Text Expr
  | ExprFieldAccess Expr Text
  | ExprFuncDef Text Expr
  | ExprIf Expr Expr Expr
  | ExprLet VarDefinition Expr
  | ExprLetRec [VarDefinition] Expr
  | ExprLiteral Literal
  | ExprMatch Expr [(CaseMatchPattern, Expr)]
  | ExprRecord [(Text, Expr)]
  | ExprVariable Text
  deriving Show

data TopLevel
  = TLExpr Expr
  | TLLetDef VarDefinition
  | TLLetRecDef [VarDefinition]
  deriving Show
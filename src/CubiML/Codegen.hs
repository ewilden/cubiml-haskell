{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module CubiML.Codegen where

import Import
import CubiML.Ast

import Lens.Micro.Platform
import qualified RIO.HashMap as HashMap
import qualified RIO.HashSet as HashSet
import RIO.State

toJS :: Expr -> Text
toJS (ExprCall fn arg) = 
  " /* call */(" <> toJS fn <> ")(" <> toJS arg <> ") "
toJS (ExprCase name exp) =
  " /* case */({ tag: \"" <> name <> "\", contents: " <> toJS exp <> "}) "
toJS (ExprFieldAccess exp name) =
  " /* fieldAccess */((" <> toJS exp <> ")." <> name <> ") "
toJS (ExprFuncDef argName body) =
  " /* funcDef */(" <> argName <> ") => (" <> toJS body <> ") "
toJS (ExprIf cond texp fexp) =
  " /* if */((" <> toJS cond <> ") ? (" <> toJS texp <> ") : (" <> toJS fexp <> ")) "
toJS (ExprLet (name, boundexp) body) =
  " /* let */((" <> name <> ") => (" <> toJS body <> "))(" <> toJS boundexp <> ") "
toJS (ExprLetRec defns body) = 
  " /* letrec */" <> "(() => {" <> (mconcat $ map renderDef defns) <> "return " <> toJS body <> "})()"
  where renderDef (name, expr) = "let " <> name <> " = " <> toJS expr <> "; "
toJS (ExprLiteral (LitBool True)) = " true "
toJS (ExprLiteral (LitBool False)) = " false "
toJS (ExprMatch matchedOn cases) =
  let renderCase ((tag, boundName), rhsExp) =
        " \"" <> tag <> "\": (" <> boundName <> ") => (" <> toJS rhsExp <> "), "
  in  "/* match */(() => {const __tmp__ = " <> toJS matchedOn <> "; return ({" <> mconcat (map renderCase cases) <> "})[__tmp__.tag](__tmp__.contents);})() "
toJS (ExprRecord fields) =
  let renderField (name, rhsExp) =
        " " <> name <> ": " <> toJS rhsExp <> ", "
  in "/* record */ ({ " <> mconcat (map renderField fields) <> "}) "
toJS (ExprVariable name) = "/* var */" <> name <> " "

toJSToplevel :: TopLevel -> Text
toJSToplevel (TLExpr expr) = toJS expr <> ";\n"
toJSToplevel (TLLetDef (name, rhs)) = " var " <> name <> " = " <> toJS rhs <> ";\n"
toJSToplevel (TLLetRecDef defs) = mconcat $ map renderDef defs
  where renderDef (name, rhs) = " var " <> name <> " = " <> toJS rhs <> ";\n"

{-

let rec foo = bar and bar = if true then false else foo in foo bar

=>
let foo = (bar) => (bar);
let bar = (foo) => (true ? false : foo);
function foo() {
  return bar(1);
}
function bar() {
  return foo(1);
}

-}
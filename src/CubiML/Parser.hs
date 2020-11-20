{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}

module CubiML.Parser where

import Import hiding (many, some, try)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.RawString.QQ

import CubiML.Ast

type Parser = Parsec Void Text

trying :: IO ()
trying = parseTest (satisfy (== 'a') :: Parser Char) ""

wordchar :: Parser Char
wordchar = lowerChar <|> upperChar <|> numberChar <|> char '_'

-- Ident: String = <r"[a-z_]\w*"> => String::from(<>);
pIdent :: Parser Text
pIdent = lexeme $ do
  initial <- lowerChar <|> char '_'
  rest <- many wordchar
  let ident = fromString $ initial : rest
  if ident == "__proto__" then
    fail "\"__proto__\" is an illegal identifier."
    else return ident

-- Tag: String = <r"`[A-Z]\w*"> => String::from(<>);
pTag :: Parser Text
pTag = lexeme $ do
  backtick <- char '`'
  initial <- upperChar
  rest <- many wordchar
  return $ fromString $ backtick : initial : rest

pSepList :: Parser a -> Text -> Parser [a]
pSepList item separator = do
  hd <- item
  tl <- many $ symbol separator *> item
  return (hd:tl)

pVarOrLiteral :: Parser Expr
pVarOrLiteral = do
  ident <- pIdent
  return $ case ident of
    "true" -> ExprLiteral $ LitBool True
    "false" -> ExprLiteral $ LitBool False
    _ -> ExprVariable ident

pIf :: Parser Expr
pIf = do
  _ <- symbol "if"
  cond <- pExpr
  _ <- symbol "then"
  exprTrue <- pExpr
  _ <- symbol "else"
  exprFalse <- pExpr
  return $ ExprIf cond exprTrue exprFalse

pFuncDef :: Parser Expr
pFuncDef = do
  _ <- symbol "fun"
  boundIdent <- pIdent
  _ <- symbol "->"
  body <- pExpr
  return $ ExprFuncDef boundIdent body

pCall :: Parser Expr
pCall = ExprCall <$> pCallExpr <*> pCaseExpr

pKeyPair :: Parser (Text, Expr)
pKeyPair = do
  ident <- pIdent
  _ <- symbol "="
  expr <- pExpr
  return (ident, expr)

pRecord :: Parser Expr
pRecord = do
  _ <- symbol "{"
  keyPairs <- optional $ pSepList pKeyPair ";"
  _ <- symbol "}"
  return $ ExprRecord $ fromMaybe [] keyPairs

pFieldAccess :: Parser Expr
pFieldAccess = do
  simpleExpr <- pSimpleExpr
  _ <- symbol "."
  ident <- pIdent
  return $ ExprFieldAccess simpleExpr ident

pCase :: Parser Expr
pCase = ExprCase <$> pTag <*> pCaseExpr

pCaseMatchPattern :: Parser CaseMatchPattern
pCaseMatchPattern = (,) <$> pTag <*> pIdent

pMatchArm :: Parser (CaseMatchPattern, Expr)
pMatchArm = (,) <$> pCaseMatchPattern <*> pCallExpr

pMatch :: Parser Expr
pMatch = do
  _ <- symbol "match"
  exprMatchedOn <- pExpr
  _ <- symbol "with"
  cases <- pSepList pMatchArm "|"
  return $ ExprMatch exprMatchedOn cases

pLetLHS :: Parser VarDefinition
pLetLHS = do
  _ <- symbol "let"
  name <- pIdent
  _ <- symbol "="
  (name,) <$> pExpr

pLetRHS :: Parser Expr
pLetRHS = symbol "in" *> pExpr

pLet :: Parser Expr
pLet = ExprLet <$> pLetLHS <*> pLetRHS

pLetRecDef :: Parser VarDefinition
pLetRecDef = (,) <$> pIdent <*> pFuncDef

pLetRecLHS :: Parser [VarDefinition]
pLetRecLHS = symbol "let" 
  *> symbol "rec"
  *> pSepList pLetRecDef "and"

pLetRec :: Parser Expr
pLetRec = ExprLetRec <$> pLetRecLHS <*> pLetRHS

pSimpleExpr :: Parser Expr
pSimpleExpr = choice
  [ symbol "(" *> pExpr <* ")"
  , pRecord
  , pVarOrLiteral
  , pFieldAccess
  ]

pCaseExpr :: Parser Expr
pCaseExpr = pSimpleExpr <|> pCase

pCallExpr :: Parser Expr
pCallExpr = pCaseExpr <|> pCall

pExpr :: Parser Expr
pExpr = choice
  [ pIf
  , try pLet
  , pLetRec
  , pFuncDef
  , pMatch
  , pCallExpr
  ]

pTopLevelItem :: Parser TopLevel
pTopLevelItem = choice
  [ TLLetDef <$> pLetLHS
  , TLLetRecDef <$> pLetRecLHS
  , TLExpr <$> pExpr
  ]

pScript :: Parser [TopLevel]
pScript = pSepList pTopLevelItem ";" <* eof


spaceConsumer :: Parser ()
spaceConsumer = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

-- picks up all trailing white space using spaceConsumer.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- matches given text using string internally and then similarly picks up all trailing white space.
symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

sampleCubiMLPgm :: Text
sampleCubiMLPgm = fromString [r|let calculate_area = fun shape ->
    match shape with
          `Circle circle_val -> true
        | `Rectangle rect_val -> false;

calculate_area `Circle {rad=true}
calculate_area `Rectangle {height=false; length=true}|]
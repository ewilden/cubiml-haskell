{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}

module CubiML.Parser where

import Import hiding (many, some, try)
import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug (
  -- dbg
  )
import qualified Text.Megaparsec.Char.Lexer as L
import Text.RawString.QQ

import CubiML.Ast

type Parser = Parsec Void Text

enableDbg :: Bool
enableDbg = False
dbg _ = id

trying :: IO ()
trying = parseTest (satisfy (== 'a') :: Parser Char) ""

wordchar :: Parser Char
wordchar = lowerChar <|> upperChar <|> numberChar <|> char '_'

reservedWords :: [Text]
reservedWords =
  [ "if"
  , "then"
  , "else"
  , "let"
  , "rec"
  , "in"
  , "and"
  , "fun"
  , "match"
  , "with"]

pReservedWord :: Parser Text
pReservedWord = choice $ map fullSymbol reservedWords
  where fullSymbol str = do
                s <- symbol str
                notFollowedBy wordchar
                return s

illegalWords :: [Text]
illegalWords = ["__proto__"]

-- Ident: String = <r"[a-z_]\w*"> => String::from(<>);
pIdent :: Parser Text
pIdent = do
  notFollowedBy pReservedWord
  lexeme $ do
    initial <- lowerChar <|> char '_'
    rest <- many wordchar
    let ident = fromString $ initial : rest
    if ident `elem` illegalWords then
      fail $ show ident ++ " is an illegal identifier."
      else return ident

-- Tag: String = <r"`[A-Z]\w*"> => String::from(<>);
pTag :: Parser Text
pTag = lexeme $ do
  backtick <- char '`'
  initial <- upperChar
  rest <- many wordchar
  return $ fromString $ backtick : initial : rest

pSepList :: (Show a) => Parser a -> Text -> Parser [a]
pSepList item separator = dbg "pSepList" $ do
  hd <- item
  tl <- many $ symbol separator *> item
  return (hd:tl)

pVarOrLiteral :: Parser Expr
pVarOrLiteral = dbg "VarOrLiteral" $ do
  ident <- pIdent
  return $ case ident of
    "true" -> ExprLiteral $ LitBool True
    "false" -> ExprLiteral $ LitBool False
    _ -> ExprVariable ident

pIf :: Parser Expr
pIf = dbg "If" $ do
  _ <- symbol "if"
  cond <- pExpr
  _ <- symbol "then"
  exprTrue <- pExpr
  _ <- symbol "else"
  exprFalse <- pExpr
  return $ ExprIf cond exprTrue exprFalse

pFuncDef :: Parser Expr
pFuncDef = dbg "FuncDef" $ do
  _ <- symbol "fun"
  boundIdent <- pIdent
  _ <- symbol "->"
  body <- pExpr
  return $ ExprFuncDef boundIdent body

-- pCall :: Parser (Expr -> Expr)
-- pCall = do
--   rhs <- pCaseExpr
--   return $ \lhs -> ExprCall lhs rhs

pKeyPair :: Parser (Text, Expr)
pKeyPair = dbg "keyPair" $ do
  ident <- pIdent
  _ <- symbol "="
  expr <- pExpr
  return (ident, expr)

pRecord :: Parser Expr
pRecord = dbg "record" $ do
  _ <- symbol "{"
  keyPairs <- optional $ pSepList pKeyPair ";"
  _ <- symbol "}"
  return $ ExprRecord $ fromMaybe [] keyPairs


pCase :: Parser Expr
pCase = dbg "case" $ ExprCase <$> pTag <*> pCaseExpr

pCaseMatchPattern :: Parser CaseMatchPattern
pCaseMatchPattern = dbg "CaseMatchPattern" $ (,) <$> pTag <*> pIdent

pMatchArm :: Parser (CaseMatchPattern, Expr)
pMatchArm = dbg "matchArm" $ do
  lhs <- pCaseMatchPattern
  _ <- symbol "->"
  rhs <- pCallExpr
  return (lhs, rhs)

pMatch :: Parser Expr
pMatch = do
  _ <- symbol "match"
  exprMatchedOn <- pExpr
  _ <- symbol "with"
  cases <- pSepList pMatchArm "|"
  return $ ExprMatch exprMatchedOn cases

pLetLHS :: Parser VarDefinition
pLetLHS = dbg "LetLHS" $ do
  _ <- symbol "let"
  name <- pIdent
  _ <- symbol "="
  (name,) <$> pExpr

pLetRHS :: Parser Expr
pLetRHS = dbg "LetRHS" $ symbol "in" *> pExpr

pLet :: Parser Expr
pLet = dbg "Let" $ ExprLet <$> pLetLHS <*> pLetRHS

pLetRecDef :: Parser VarDefinition
pLetRecDef = dbg "LetRec" $ (,) <$> pIdent <*> pFuncDef

pLetRecLHS :: Parser [VarDefinition]
pLetRecLHS = dbg "LetRecLHS" $ symbol "let" 
  *> symbol "rec"
  *> pSepList pLetRecDef "and"

pLetRec :: Parser Expr
pLetRec = dbg "LetRec" $ ExprLetRec <$> pLetRecLHS <*> pLetRHS

pFieldAccess :: Parser (Expr -> Expr)
pFieldAccess = do
  _ <- symbol "."
  ident <- pIdent
  return $ \e -> ExprFieldAccess e ident

pSimpleExprNonleft :: Parser Expr
pSimpleExprNonleft = dbg "simpleExprNonLeft" $ choice
  [ symbol "(" *> pExpr <* ")"
  , pRecord
  , pVarOrLiteral
  ]
  
pSimpleExpr :: Parser Expr
pSimpleExpr = dbg "SimpleExpr" $ postfixChain 
  pSimpleExprNonleft 
  pFieldAccess

pCaseExpr :: Parser Expr
pCaseExpr = dbg "CaseExpr" $ pCase <|> pSimpleExpr

pCallExpr :: Parser Expr
pCallExpr = dbg "CallExpr" $ do
  caseExprs <- some pCaseExpr
  return $ case caseExprs of
    [] -> error "impossible"
    [caseExpr] -> caseExpr
    ls -> foldr1 ExprCall ls

-- pCallExpr :: Parser Expr
-- pCallExpr = try pCaseExpr 
--   <|> postfixChain pCaseExpr pCall

pExpr :: Parser Expr
pExpr = choice
  [ pIf
  , try pLetRec
  , pLet
  , pFuncDef
  , pMatch
  , pCallExpr
  ]

pTopLevelItem :: Parser TopLevel
pTopLevelItem = choice
  [ dbg "tl let rec" $ try $ TLLetRecDef <$> pLetRecLHS
  , dbg "tllet" $ TLLetDef <$> pLetLHS
  , dbg "tl expr" $ TLExpr <$> pExpr
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

-- from https://www.reddit.com/r/haskelltil/comments/3ocukk/a_function_postfixchain_to_parse_a_left_recursive/
postfixChain :: Parser a -> Parser (a -> a) -> Parser a
postfixChain p op = do
  x <- p
  rest x
  where
    rest x = (do f <- op
                 rest $ f x) <|> return x



sampleCubiMLPgm :: Text
sampleCubiMLPgm = fromString [r|let calculate_area = fun shape ->
    match shape with
          `Circle circle_val -> true
        | `Rectangle rect_val -> false ;

calculate_area (`Circle {rad=true});
calculate_area `Rectangle {height=false; length=true}|]
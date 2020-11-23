{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module CubiML.Typechecker.Frontend where

import Control.Monad.Except as Except
import Lens.Micro.Platform  
import qualified RIO.HashMap as HashMap
import qualified RIO.HashSet as HashSet
import RIO.State

import Import
import CubiML.Ast
import qualified CubiML.Typechecker.Core as Core

newtype Bindings = Bindings 
  { _bindingsMap :: HashMap.HashMap Text Core.Value
  } deriving Show
makeClassy ''Bindings

initBindings :: Bindings
initBindings = Bindings mempty

type Frontend s m = 
  (Core.TypecheckerCore m, MonadError Core.TypeError m, MonadState s m, HasBindings s) 

lookupBinding :: (Frontend s m) => Text -> m (Maybe Core.Value)
lookupBinding name = use $ bindingsMap . at name

insertBinding :: (Frontend s m) => Text -> Core.Value -> m ()
insertBinding name value = bindingsMap . at name .= Just value

inChildBindingScope :: (Frontend s m) => m a -> m a
inChildBindingScope action = do
  currBindings <- use bindings
  result <- action
  bindings .= currBindings
  return result

flow :: (Frontend s m) => Core.Value -> Core.Use -> m ()
flow = Core.flow

checkUnique :: (Frontend s m) => (Hashable a, Eq a) => Core.TypeError -> [a] -> m ()
checkUnique errMsg items =
  when (length (HashSet.fromList items) /= length items) $
    throwError errMsg

checkExpr :: (Frontend s m) => Expr -> m Core.Value
checkExpr (ExprLiteral _) = Core.bool
checkExpr (ExprVariable name) = do
  mayVal <- lookupBinding name
  case mayVal of
      Nothing -> throwError (Core.SyntaxError $ "Unbound variable " ++ show name)
      Just value -> return value
checkExpr (ExprRecord fields) = do
  checkUnique 
    (Core.SyntaxError $ "Repeated field name " ++ show (map fst fields))
    $ map fst fields
  fieldVals <- mapM (checkExpr . snd) fields
  Core.obj $ zip (map fst fields) fieldVals
checkExpr (ExprCase tag valExpr) = do
  valType <- checkExpr valExpr
  Core.case_ (tag, valType)
checkExpr (ExprIf condExpr thenExpr elseExpr) = do
  condType <- checkExpr condExpr
  bound <- Core.bool_use
  flow condType bound

  thenType <- checkExpr thenExpr
  elseType <- checkExpr elseExpr

  {- In biunfication, union value types are not represented explicitly. 
    Instead, they are implicit in the flow graph. To do this we create 
    an intermediate type variable node using engine.var(), add flow edges
    from the type of each subexpression to the variable, and then return 
    the variable as the type of the entire if expression. 
  -}
  (merged, mergedBound) <- Core.var
  flow thenType mergedBound
  flow elseType mergedBound
  return merged
checkExpr (ExprFieldAccess lhsExpr name) = do
  lhsType <- checkExpr lhsExpr
  (fieldType, fieldBound) <- Core.var
  bound <- Core.obj_use (name, fieldBound)
  flow lhsType bound
  return fieldType
checkExpr (ExprMatch matchExpr cases) = do
  matchType <- checkExpr matchExpr
  (resultType, resultBound) <- Core.var
  let caseNames = map (fst . fst) cases
  checkUnique
    (Core.SyntaxError $ "Repeated match case: " ++ show caseNames)
    caseNames
  caseTypePairs <- forM cases $ \((tag, name), rhsExpr) -> do
    (wrappedType, wrappedBound) <- Core.var
    rhsType <- inChildBindingScope $ do
      insertBinding name wrappedType
      checkExpr rhsExpr
    flow rhsType resultBound
    return (tag, wrappedBound)
  bound <- Core.case_use caseTypePairs
  flow matchType bound
  return resultType
checkExpr (ExprFuncDef argName bodyExpr) = do
  (argType, argBound) <- Core.var
  bodyType <- inChildBindingScope $ do
    insertBinding argName argType
    checkExpr bodyExpr
  Core.func argBound bodyType
checkExpr (ExprCall funcExpr argExpr) = do
  funcType <- checkExpr funcExpr
  argType <- checkExpr argExpr
  (retType, retBound) <- Core.var
  bound <- Core.func_use argType retBound
  flow funcType bound
  return retType
checkExpr (ExprLet (name, varExpr) restExpr) = do
  varType <- checkExpr varExpr
  inChildBindingScope $ do
    insertBinding name varType
    checkExpr restExpr
checkExpr (ExprLetRec defs restExpr) = inChildBindingScope $ do
  tempBounds <- forM (map fst defs) $ \name -> do
    (tempType, tempBound) <- Core.var
    insertBinding name tempType
    return tempBound
  forM_ (zip defs tempBounds) $ \((_, expr), bound) -> do
    varType <- checkExpr expr
    flow varType bound
  checkExpr restExpr

checkToplevel :: (Frontend s m) => TopLevel -> m ()
checkToplevel (TLExpr expr) = void $ checkExpr expr
checkToplevel (TLLetDef (name, varExpr)) = do
  varType <- checkExpr varExpr
  insertBinding name varType
checkToplevel (TLLetRecDef defs) = do
  tempBounds <- forM defs $ \(name, _) -> do
    (tempType, tempBound) <- Core.var
    insertBinding name tempType
    return tempBound
  forM_ (zip defs tempBounds) $ \((_, expr), bound) -> do
    varType <- checkExpr expr
    flow varType bound
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
  }
makeLenses ''Bindings

newBindings :: Bindings
newBindings = Bindings mempty

getBinding :: Bindings -> Text -> Maybe Core.Value
getBinding b t = HashMap.lookup t $ _bindingsMap b

putBinding :: Bindings -> Text -> Core.Value -> Bindings
putBinding b t v = over bindingsMap (HashMap.insert t v) b

type Frontend m a = (Core.TypecheckerCore m) => Except.ExceptT Core.TypeError (StateT Bindings m) a

liftCore :: m a -> Frontend m a
liftCore = lift . lift

lookupBinding :: Text -> Frontend m (Maybe Core.Value)
lookupBinding name = gets (`getBinding` name)

insertBinding :: Text -> Core.Value -> Frontend m ()
insertBinding name value = modify (\b -> putBinding b name value)

inChildBindingScope :: Frontend m a -> Frontend m a
inChildBindingScope action = do
  currBindings <- get
  result <- action
  put currBindings
  return result

flow :: Core.Value -> Core.Use -> Frontend m ()
flow val use = do
  eith <- liftCore $ Core.flow val use
  either throwError return eith

checkUnique :: (Hashable a, Eq a) => Core.TypeError -> [a] -> Frontend m ()
checkUnique errMsg items =
  when (length (HashSet.fromList items) /= length items) $
    throwError errMsg

checkExpr :: Expr -> Frontend m Core.Value
checkExpr (ExprLiteral _) = liftCore Core.bool
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
  liftCore $ Core.obj $ zip (map fst fields) fieldVals
checkExpr (ExprCase tag valExpr) = do
  valType <- checkExpr valExpr
  liftCore $ Core.case_ (tag, valType)
checkExpr (ExprIf condExpr thenExpr elseExpr) = do
  condType <- checkExpr condExpr
  bound <- liftCore Core.bool_use
  flow condType bound

  thenType <- checkExpr thenExpr
  elseType <- checkExpr elseExpr

  {- In biunfication, union value types are not represented explicitly. 
     Instead, they are implicit in the flow graph. To do this we create 
     an intermediate type variable node using engine.var(), add flow edges
     from the type of each subexpression to the variable, and then return 
     the variable as the type of the entire if expression. 
  -}
  (merged, mergedBound) <- liftCore Core.var
  flow thenType mergedBound
  flow elseType mergedBound
  return merged
checkExpr (ExprFieldAccess lhsExpr name) = do
  lhsType <- checkExpr lhsExpr
  (fieldType, fieldBound) <- liftCore Core.var
  bound <- liftCore $ Core.obj_use (name, fieldBound)
  flow lhsType bound
  return fieldType
checkExpr (ExprMatch matchExpr cases) = do
  matchType <- checkExpr matchExpr
  (resultType, resultBound) <- liftCore Core.var
  let caseNames = map (fst . fst) cases
  checkUnique
    (Core.SyntaxError $ "Repeated match case: " ++ show caseNames)
    caseNames
  caseTypePairs <- forM cases $ \((tag, name), rhsExpr) -> do
    (wrappedType, wrappedBound) <- liftCore Core.var
    rhsType <- inChildBindingScope $ do
      insertBinding name wrappedType
      checkExpr rhsExpr
    flow rhsType resultBound
    return (tag, wrappedBound)
  bound <- liftCore $ Core.case_use caseTypePairs
  flow matchType bound
  return resultType

checkExpr _ = undefined
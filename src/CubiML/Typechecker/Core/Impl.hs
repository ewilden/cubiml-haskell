{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module CubiML.Typechecker.Core.Impl where
  
import Control.Monad.Except as Except
import Import
import Lens.Micro.Platform
import qualified RIO.HashMap as HashMap
import RIO.State

import CubiML.Typechecker.Core as Core hiding (addNode', newVal, newUse, var, bool, bool_use, func, func_use, obj, obj_use, case_, case_use, flow)
import CubiML.Typechecker.Reachability

type CoreM m = (Reachability m, MonadError TypeError m, MonadState CoreState m)

newtype CoreState = CoreState
  { _coreTypes :: HashMap ID TypeNode
  } deriving Show

makeLenses ''CoreState

addNode' :: (CoreM m) => m ID
addNode' = do
  i <- add_node
  types <- gets _coreTypes
  when (i /= HashMap.size types) $ error "node list mismatch"
  return i

newVal :: (CoreM m) => VTypeHead -> m Value
newVal valType = do
  i <- addNode'
  modify (over coreTypes (HashMap.insert i (TNValue valType)))
  return $ Value i

newUse :: (CoreM m) => UTypeHead -> m Use
newUse constraint = do
  i <- addNode'
  modify (over coreTypes (HashMap.insert i (TNUse constraint)))
  return $ Use i

-- pub fn var(&mut self) -> (Value, Use);
var :: (CoreM m) => m (Value, Use)
var = do
  i <- addNode'
  modify (over coreTypes (HashMap.insert i TNVar))
  return (Value i, Use i)

-- pub fn bool(&mut self) -> Value;
bool :: (CoreM m) => m Value
bool = newVal VBool
-- pub fn bool_use(&mut self) -> Use;
bool_use :: (CoreM m) => m Use
bool_use = newUse UBool

-- pub fn func(&mut self, arg: Use, ret: Value) -> Value;
func :: (CoreM m) => Use -> Value -> m Value
func arg ret = newVal $ VFunc arg ret
-- pub fn func_use(&mut self, arg: Value, ret: Use) -> Use;
func_use :: (CoreM m) => Value -> Use -> m Use
func_use arg ret = newUse $ UFunc arg ret

-- pub fn obj(&mut self, fields: Vec<(String, Value)>) -> Value;
obj :: (CoreM m) => [(Text, Value)] -> m Value
obj fields = newVal $ VObj (HashMap.fromList fields)
-- pub fn obj_use(&mut self, field: (String, Use)) -> Use;
obj_use :: (CoreM m) => (Text, Use) -> m Use
obj_use field = newUse $ uncurry UObj field

-- pub fn case(&mut self, case: (String, Value)) -> Value;
case_ :: (CoreM m) => (Text, Value) -> m Value
case_ c = newVal $ uncurry VCase c
-- pub fn case_use(&mut self, cases: Vec<(String, Use)>) -> Use;
case_use :: (CoreM m) => [(Text, Use)] -> m Use
case_use cases = newUse $ UCase (HashMap.fromList cases)

-- pub fn flow(&mut self, lhs: Value, rhs: Use) -> Result<(), TypeError>;
-- view flow v u as v <= u, aka a variable "flowing" to its usage.
flow :: (CoreM m) => Value -> Use -> m ()
flow (Value l) (Use r) = do
  newPairsToCheck <- add_edge l r
  newEdges <- fmap join $ forM newPairsToCheck $ \(i, j) -> do
    typeMap <- gets _coreTypes
    case (HashMap.lookup i typeMap, HashMap.lookup j typeMap) of
      (Just (TNValue lhsHead), Just (TNUse rhsHead)) -> 
        either throwError return (checkHeads lhsHead rhsHead)
      _ -> return []
  forM_ newEdges (uncurry flow)
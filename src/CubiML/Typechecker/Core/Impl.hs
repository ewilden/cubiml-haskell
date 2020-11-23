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


newtype CoreState = CoreState
  { _coreTypes :: HashMap ID TypeNode
  } deriving Show

makeClassy ''CoreState
type CoreM s m = (Reachability m, MonadError TypeError m, MonadState s m, HasCoreState s)

initCoreState :: CoreState
initCoreState = CoreState mempty

addNode' :: (CoreM s m) => m ID
addNode' = do
  i <- add_node
  types <- use coreTypes
  when (i /= HashMap.size types) $ error "node list mismatch"
  return i

newVal :: (CoreM s m) => VTypeHead -> m Value
newVal valType = do
  i <- addNode'
  coreTypes %= HashMap.insert i (TNValue valType)
  return $ Value i

newUse :: (CoreM s m) => UTypeHead -> m Use
newUse constraint = do
  i <- addNode'
  coreTypes %= HashMap.insert i (TNUse constraint)
  return $ Use i

-- pub fn var(&mut self) -> (Value, Use);
var :: (CoreM s m) => m (Value, Use)
var = do
  i <- addNode'
  coreTypes %= HashMap.insert i TNVar
  return (Value i, Use i)

-- pub fn bool(&mut self) -> Value;
bool :: (CoreM s m) => m Value
bool = newVal VBool
-- pub fn bool_use(&mut self) -> Use;
bool_use :: (CoreM s m) => m Use
bool_use = newUse UBool

-- pub fn func(&mut self, arg: Use, ret: Value) -> Value;
func :: (CoreM s m) => Use -> Value -> m Value
func arg ret = newVal $ VFunc arg ret
-- pub fn func_use(&mut self, arg: Value, ret: Use) -> Use;
func_use :: (CoreM s m) => Value -> Use -> m Use
func_use arg ret = newUse $ UFunc arg ret

-- pub fn obj(&mut self, fields: Vec<(String, Value)>) -> Value;
obj :: (CoreM s m) => [(Text, Value)] -> m Value
obj fields = newVal $ VObj (HashMap.fromList fields)
-- pub fn obj_use(&mut self, field: (String, Use)) -> Use;
obj_use :: (CoreM s m) => (Text, Use) -> m Use
obj_use field = newUse $ uncurry UObj field

-- pub fn case(&mut self, case: (String, Value)) -> Value;
case_ :: (CoreM s m) => (Text, Value) -> m Value
case_ c = newVal $ uncurry VCase c
-- pub fn case_use(&mut self, cases: Vec<(String, Use)>) -> Use;
case_use :: (CoreM s m) => [(Text, Use)] -> m Use
case_use cases = newUse $ UCase (HashMap.fromList cases)

-- pub fn flow(&mut self, lhs: Value, rhs: Use) -> Result<(), TypeError>;
-- view flow v u as v <= u, aka a variable "flowing" to its usage.
flow :: (CoreM s m) => Value -> Use -> m ()
flow (Value l) (Use r) = do
  newPairsToCheck <- add_edge l r
  newEdges <- fmap join $ forM newPairsToCheck $ \(i, j) -> do
    typeMap <- use coreTypes
    case (HashMap.lookup i typeMap, HashMap.lookup j typeMap) of
      (Just (TNValue lhsHead), Just (TNUse rhsHead)) -> 
        either throwError return (checkHeads lhsHead rhsHead)
      _ -> return []
  forM_ newEdges (uncurry flow)
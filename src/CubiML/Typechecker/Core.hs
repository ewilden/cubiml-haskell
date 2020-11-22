{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module CubiML.Typechecker.Core where
  
import Control.Monad.Except as Except
import Import
import Lens.Micro.Platform
import RIO.HashMap (HashMap)
import qualified RIO.HashMap as HashMap
import RIO.State

type ID = Int

-- Value = "producing" side of variable
newtype Value = Value { _valueID :: ID } deriving Show
makeLenses ''Value

-- Use = "consuming" side of variable
newtype Use = Use { _useID :: ID } deriving Show
makeLenses ''Use

data VTypeHead = VBool
               | VFunc Use Value
               | VObj (HashMap Text Value)
               | VCase Text Value
               deriving Show

data UTypeHead = UBool
               | UFunc Value Use
               | UObj Text Use
               | UCase (HashMap Text Use)
               deriving Show

data TypeError = TypeError String
               | SyntaxError String
               deriving Show

class (Monad m) => Reachability m where
  add_node :: m ID
  add_edge :: ID -> ID -> m [(ID, ID)]

data TypeNode = TNVar
              | TNValue VTypeHead
              | TNUse UTypeHead
              deriving Show

newtype CoreState = CoreState
  { _coreTypes :: HashMap ID TypeNode
  } deriving Show

makeLenses ''CoreState

class (Reachability m, MonadError TypeError m, MonadState CoreState m) => TypecheckerCore m where
  addNode' :: m ID
  addNode' = do
    i <- add_node
    types <- gets _coreTypes
    when (i /= HashMap.size types) $ error "node list mismatch"
    return i

  newVal :: VTypeHead -> m Value
  newVal valType = do
    i <- addNode'
    modify (over coreTypes (HashMap.insert i (TNValue valType)))
    return $ Value i

  newUse :: UTypeHead -> m Use
  newUse constraint = do
    i <- addNode'
    modify (over coreTypes (HashMap.insert i (TNUse constraint)))
    return $ Use i

  -- pub fn var(&mut self) -> (Value, Use);
  var :: m (Value, Use)
  var = do
    i <- addNode'
    modify (over coreTypes (HashMap.insert i TNVar))
    return (Value i, Use i)
  
  -- pub fn bool(&mut self) -> Value;
  bool :: m Value
  bool = newVal VBool
  -- pub fn bool_use(&mut self) -> Use;
  bool_use :: m Use
  bool_use = newUse UBool

  -- pub fn func(&mut self, arg: Use, ret: Value) -> Value;
  func :: Use -> Value -> m Value
  func arg ret = newVal $ VFunc arg ret
  -- pub fn func_use(&mut self, arg: Value, ret: Use) -> Use;
  func_use :: Value -> Use -> m Use
  func_use arg ret = newUse $ UFunc arg ret

  -- pub fn obj(&mut self, fields: Vec<(String, Value)>) -> Value;
  obj :: [(Text, Value)] -> m Value
  obj fields = newVal $ VObj (HashMap.fromList fields)
  -- pub fn obj_use(&mut self, field: (String, Use)) -> Use;
  obj_use :: (Text, Use) -> m Use
  obj_use field = newUse $ uncurry UObj field

  -- pub fn case(&mut self, case: (String, Value)) -> Value;
  case_ :: (Text, Value) -> m Value
  case_ c = newVal $ uncurry VCase c
  -- pub fn case_use(&mut self, cases: Vec<(String, Use)>) -> Use;
  case_use :: [(Text, Use)] -> m Use
  case_use cases = newUse $ UCase (HashMap.fromList cases)

  -- pub fn flow(&mut self, lhs: Value, rhs: Use) -> Result<(), TypeError>;
  -- view flow v u as v <= u, aka a variable "flowing" to its usage.
  flow :: Value -> Use -> m ()
  flow (Value l) (Use r) = do
    newPairsToCheck <- add_edge l r
    newEdges <- fmap join $ forM newPairsToCheck $ \(i, j) -> do
      typeMap <- gets _coreTypes
      case (HashMap.lookup i typeMap, HashMap.lookup j typeMap) of
        (Just (TNValue lhsHead), Just (TNUse rhsHead)) -> 
          either throwError return (checkHeads lhsHead rhsHead)
        _ -> return []
    forM_ newEdges (uncurry flow)

{- In order to avoid the infinite loops that make semi-unification undecidable, 
   biunification restricts all subtype constraints to be of the form v <= u where 
   v is a value type and u is a use type. These constraints can be naturally 
   interpreted as requiring that a program value be compatible with the way it is 
   used.
   Variables have both a value v and a bound u, and we actually need u <= v
   (every read from a variable fits within the span of types written to it)
   but this can express u = v which is undecidable.
   Instead we ensure transitivity of flows: for variable (v1, u1), for each v <= u1
   and each v1 <= u, we add constraint v <= u. (then we get u1 and v1 in the interval v <= _ <= u.
   Could v <= v1 < u1 <= u ? Need to think about that.)
   (AKA for each pair of assignment from the variable (v), and assignment to the variable (u), 
   we add a flows constraint.)
-}
  runCore :: m a -> Either TypeError a

checkHeads :: VTypeHead -> UTypeHead -> Either TypeError [(Value, Use)]
checkHeads VBool UBool = return []
checkHeads (VFunc arg1 ret1) (UFunc arg2 ret2) = return [(ret1, ret2), (arg2, arg1)]
checkHeads (VObj fields1) (UObj name rhs2) =
  case HashMap.lookup name fields1 of
    Just lhs2 -> return [(lhs2, rhs2)]
    Nothing -> Left $ TypeError $ "missing field: " ++ show name
checkHeads (VCase name lhs2) (UCase cases2) = 
  case HashMap.lookup name cases2 of
    Just rhs2 -> return [(lhs2, rhs2)]
    Nothing -> Left $ TypeError $ "unhandled case: " ++ show name
checkHeads v u = Left $ TypeError $ "Unexpected types: " ++ show v ++ "; " ++ show u



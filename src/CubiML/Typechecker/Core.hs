{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}

module CubiML.Typechecker.Core where
  
import Import

data Value

data Use

data TypeError = TypeError String
               | SyntaxError String
               deriving Show

class (Monad m) => TypecheckerCore m where
  -- pub fn var(&mut self) -> (Value, Use);
  var :: m (Value, Use)

  -- pub fn bool(&mut self) -> Value;
  bool :: m Value
  -- pub fn bool_use(&mut self) -> Use;
  bool_use :: m Use

  -- pub fn func(&mut self, arg: Use, ret: Value) -> Value;
  func :: Use -> Value -> m Value
  -- pub fn func_use(&mut self, arg: Value, ret: Use) -> Use;
  func_use :: Value -> Use -> m Value

  -- pub fn obj(&mut self, fields: Vec<(String, Value)>) -> Value;
  obj :: [(Text, Value)] -> m Value
  -- pub fn obj_use(&mut self, field: (String, Use)) -> Use;
  obj_use :: (Text, Use) -> m Use

  -- pub fn case(&mut self, case: (String, Value)) -> Value;
  case_ :: (Text, Value) -> m Value
  -- pub fn case_use(&mut self, cases: Vec<(String, Use)>) -> Use;
  case_use :: [(Text, Use)] -> m Use

  -- pub fn flow(&mut self, lhs: Value, rhs: Use) -> Result<(), TypeError>;
  -- view flow v u as v <= u, aka a variable "flowing" to its usage.
  flow :: Value -> Use -> m (Either TypeError ())

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
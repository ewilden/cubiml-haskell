{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}

module CubiML.Typechecker where
  
import CubiML.Parser
import Import
import Control.Monad.Except
import Lens.Micro.Platform
import RIO.State
import Text.Megaparsec
import CubiML.Codegen

import CubiML.Ast
import qualified CubiML.Typechecker.Core as Core
import qualified CubiML.Typechecker.Core.Impl as CoreImpl
import qualified CubiML.Typechecker.Reachability as Reach
import qualified CubiML.Typechecker.Frontend as Frontend

data UnifiedState = UnifiedState 
 { _reach :: Reach.RState
 , _core :: CoreImpl.CoreState
 , _frontend :: Frontend.Bindings
 } deriving Show
makeClassy ''UnifiedState

instance Reach.HasRState UnifiedState where rState = reach
instance CoreImpl.HasCoreState UnifiedState where coreState = core
instance Frontend.HasBindings UnifiedState where bindings = frontend

newtype Typechecker a = Typechecker {
  unTypechecker :: StateT UnifiedState (Except Core.TypeError) a
} deriving (Functor, Applicative, Monad, MonadError Core.TypeError, MonadState UnifiedState)

instance Reach.Reachability Typechecker where
  add_node = Reach.addNode
  add_edge = Reach.addEdge

instance Core.TypecheckerCore Typechecker where
  addNode' = CoreImpl.addNode'
  newVal = CoreImpl.newVal
  newUse = CoreImpl.newUse
  var = CoreImpl.var
  bool = CoreImpl.bool
  bool_use = CoreImpl.bool_use
  func = CoreImpl.func
  func_use = CoreImpl.func_use
  obj = CoreImpl.obj
  obj_use = CoreImpl.obj_use
  case_ = CoreImpl.case_
  case_use = CoreImpl.case_use
  flow = CoreImpl.flow

checkScript :: [TopLevel] -> Typechecker ()
checkScript = mapM_ Frontend.checkToplevel

initState :: UnifiedState
initState = UnifiedState Reach.initRState CoreImpl.initCoreState Frontend.initBindings

runTypechecker :: UnifiedState -> Typechecker a -> Either Core.TypeError (a, UnifiedState)
runTypechecker s action = unTypechecker action
  & flip runStateT s
  & runExcept

sampleParsedAst :: [TopLevel]
sampleParsedAst = let (Right p) = runParser pScript "sampleCubiMLPgm" sampleCubiMLPgm in p

sampleTypechecker :: Either Core.TypeError ((), UnifiedState)
sampleTypechecker = runTypechecker initState $
  checkScript sampleParsedAst

sampleJS :: Text
sampleJS = mconcat $ map toJSToplevel sampleParsedAst



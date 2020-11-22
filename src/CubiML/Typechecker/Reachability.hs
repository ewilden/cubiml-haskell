{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module CubiML.Typechecker.Reachability where

import Import
import Lens.Micro.Platform
import qualified RIO.HashMap as HashMap
import qualified RIO.HashSet as HashSet
import RIO.State

type ID = Int

data RState = RState 
  { _upsets :: HashMap ID (HashSet ID)
  , _downsets :: HashMap ID (HashSet ID)
  } deriving Show

makeLenses ''RState

contains :: ID -> Maybe (HashSet ID) -> Bool
contains a (Just s) = HashSet.member a s
contains _ Nothing = error "unexpected nonexistent ID"

asList :: Maybe (HashSet a) -> [a]
asList Nothing = []
asList (Just s) = HashSet.toList s

type ReachM m = (MonadState RState m)

class Reachability m where
  add_node :: m ID
  add_edge :: ID -> ID -> m [(ID, ID)]

addNode :: (ReachM m) => m ID
addNode = do
  i <- HashMap.size <$> gets _upsets
  modify $ over upsets $ HashMap.insert i mempty
  modify $ over downsets $ HashMap.insert i mempty
  return i

addEdge :: (ReachM m) => ID -> ID -> m [(ID, ID)]
addEdge a b = do
  edgeAlreadyExists <- get <&> (^. (downsets . at a)) <&> contains b
  if edgeAlreadyExists then return [] else do
    modify $ over (downsets . ix a) $ HashSet.insert b
    modify $ over (upsets . ix b) $ HashSet.insert a
    intoAs <- get <&> (^. (upsets . at a)) <&> asList
    newLhsEdges <- join <$> (forM intoAs $ \a' -> addEdge a' b)
    outofBs <- get <&> (^. (downsets . at b)) <&> asList
    newRhsEdges <- join <$> (forM outofBs $ \b' -> addEdge a b')
    return $ (a,b) : newLhsEdges ++ newRhsEdges
{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
module Database.PipesGremlin.Internal where

import Control.Proxy (
    Proxy,Producer,respond,liftP,(>->),
    ProduceT,RespondT(RespondT),runRespondT,
    eachS,toListD,unitU,
    ProxyFast,)
import Control.Proxy.Trans.Writer (execWriterK)

import Web.Neo (
    NeoT,
    Node,Edge,Label,Properties)
import qualified Web.Neo as Neo (
    nodeById,nodesByLabel,
    allEdges,incomingEdges,outgoingEdges,
    nodeLabels,nodeProperties,
    edgeLabel,edgeProperties,
    source,target)

import Control.Monad ((>=>),mzero,guard)
import Control.Monad.Trans (lift)

import Data.Text (Text)
import Data.Aeson (Value)
import qualified Data.HashMap.Strict as HashMap (lookup)

type PG m = ProduceT ProxyFast (NeoT m)

runPG :: PG m a -> Producer ProxyFast a (NeoT m) ()
runPG = runRespondT

-- | The node with the given Id.
nodeById :: (Monad m) => Integer -> PG m Node
nodeById = lift . Neo.nodeById

-- | All nodes with the given label.
nodesByLabel :: (Monad m) => Label -> PG m Node
nodesByLabel = lift . Neo.nodesByLabel >=> eachS

outEdge :: (Monad m) => Node -> PG m Edge
outEdge = lift . Neo.outgoingEdges >=> eachS

inEdge :: (Monad m) => Node -> PG m Edge
inEdge = lift . Neo.incomingEdges >=> eachS

anyEdge :: (Monad m) => Node -> PG m Edge
anyEdge = lift . Neo.allEdges >=> eachS

outEdgeLabeled :: (Monad m) => Label -> Node -> PG m Edge
outEdgeLabeled wantedLabel = outEdge >=> hasEdgeLabel wantedLabel

inEdgeLabeled :: (Monad m) => Text -> Node -> PG m Edge
inEdgeLabeled wantedLabel = inEdge >=> hasEdgeLabel wantedLabel

anyEdgeLabeled :: (Monad m) => Text -> Node -> PG m Edge
anyEdgeLabeled wantedLabel = anyEdge >=> hasEdgeLabel wantedLabel

next :: (Monad m) => Node -> PG m Node
next = outEdge >=> target

previous :: (Monad m) => Node -> PG m Node
previous = inEdge >=> source

neighbour :: (Monad m) => Node -> PG m Node
neighbour node = RespondT (do
    runRespondT (previous node)
    runRespondT (next node))

nextLabeled :: (Monad m) => Label -> Node -> PG m Node
nextLabeled wantedLabel = outEdgeLabeled wantedLabel >=> target

previousLabeled :: (Monad m) => Label -> Node -> PG m Node
previousLabeled wantedLabel = inEdgeLabeled wantedLabel >=> source

neighbourLabeled :: (Monad m) => Label -> Node -> PG m Node
neighbourLabeled wantedLabel node = RespondT (do
    runRespondT (previousLabeled wantedLabel node)
    runRespondT (nextLabeled wantedLabel node))

nodeLabel :: (Monad m) => Node -> PG m Label
nodeLabel = lift . Neo.nodeLabels >=> eachS

hasNodeLabel :: (Monad m) => Text -> Node -> PG m Node
hasNodeLabel wantedLabel = has (nodeLabel >=> strain (== wantedLabel))

nodeProperty :: (Monad m) => Text -> Node -> PG m Value
nodeProperty key = lift . Neo.nodeProperties >=> lookupProperty key

lookupProperty :: (Monad m) => Text -> Properties -> PG m Value
lookupProperty key properties = case HashMap.lookup key properties of
    Nothing -> mzero
    Just value -> return value

target :: (Monad m) => Edge -> PG m Node
target = lift . Neo.target

source :: (Monad m) => Edge -> PG m Node
source = lift . Neo.source

edgeLabel :: (Monad m) => Edge -> PG m Label
edgeLabel = lift . Neo.edgeLabel

hasEdgeLabel :: (Monad m) => Text -> Edge -> PG m Edge
hasEdgeLabel wantedLabel = has (edgeLabel >=> strain (== wantedLabel))

edgeProperty :: (Monad m) => Text -> Edge -> PG m Value
edgeProperty key = lift . Neo.edgeProperties >=> lookupProperty key

-- | Gather all results in a list.
gather :: (Monad m) => PG m a -> PG m [a]
gather = RespondT . gatherPipe . runRespondT

-- | Produce each element of the given list.
scatter :: (Monad m) => [a] -> PG m a
scatter = eachS

has :: (Monad m) => (a -> PG m b) -> a -> PG m a
has p a = do
    ensure (p a)
    return a

-- | Only let an element through if the given pipe produces nothing.
hasnot :: (Monad m) => (a -> PG m b) -> a -> PG m a
hasnot p a = do
    ensurenot (p a)
    return a

-- | Filter out only elements satisfying the given predicate.
strain :: (Monad m) => (a -> Bool) -> a -> PG m a
strain predicate a
    | predicate a = return a
    | otherwise   = mzero

-- | Only continue if the given pipe produces anything.
ensure :: (Monad m) => PG m a -> PG m ()
ensure p = do
    as <- gather p
    guard (not (null as))

-- | Only continue if the given pipe produces nothing.
ensurenot :: (Monad m) => PG m a -> PG m ()
ensurenot p = do
    as <- gather p
    guard (null as)

-- | Gather all responds of a pipe into a list.
gatherPipe :: (Monad m) =>
              (ProxyFast x' x ()  b  m r ) ->
              (ProxyFast x' x () [b] m ())
gatherPipe p = (execWriterK (const (liftP p) >-> toListD >-> unitU) ()) >>= respond


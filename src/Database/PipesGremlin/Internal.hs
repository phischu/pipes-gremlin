{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
module Database.PipesGremlin.Internal where

import Control.Proxy (
    Proxy,Producer,request,respond,liftP,(>->),
    ProduceT,RespondT(RespondT),runRespondT,
    eachS,toListD,unitU,
    C)
import Control.Proxy.Trans.Writer (execWriterK)

import Web.Neo (
    NeoT,Node,Edge,Label,Properties)
import qualified Web.Neo as Neo (
    nodeById,nodesByLabel,
    allEdges,incomingEdges,outgoingEdges,
    edgeLabel,
    source,target)

import Control.Monad (forever,(>=>),mzero,guard)
import Control.Monad.Trans (lift)
import Control.Exception (Exception)
import Data.Typeable (Typeable)

import Data.Text (Text,pack)
import Data.Aeson (Value)

-- | The node with the given Id.
nodeById :: (Proxy p,Monad m) => Integer -> ProduceT p (NeoT m) Node
nodeById = lift . Neo.nodeById

-- | All nodes with the given label.
nodesByLabel :: (Proxy p,Monad m) => Label -> ProduceT p (NeoT m) Node
nodesByLabel = lift . Neo.nodesByLabel >=> eachS

outEdge :: (Proxy p,Monad m) => Node -> ProduceT p (NeoT m) Edge
outEdge = lift . Neo.outgoingEdges >=> eachS

outEdgeLabeled :: (Proxy p,Monad m) => Label -> Node -> ProduceT p (NeoT m) Edge
outEdgeLabeled wantedLabel node = do
    edge  <- outEdge node
    label <- lift (Neo.edgeLabel edge)
    guard (wantedLabel == label)
    return edge

next :: (Proxy p,Monad m) => Node -> ProduceT p (NeoT m) Node
next = outEdge >=> target

nextLabeled :: (Proxy p,Monad m) => Label -> Node -> ProduceT p (NeoT m) Node
nextLabeled wantedLabel = outEdgeLabeled wantedLabel >=> target

inEdge :: (Proxy p,Monad m) => Node -> ProduceT p (NeoT m) Edge
inEdge = lift . Neo.incomingEdges >=> eachS

previous :: (Proxy p,Monad m) => Node -> ProduceT p (NeoT m) Node
previous = inEdge >=> source

anyEdge :: (Proxy p,Monad m) => Node -> ProduceT p (NeoT m) Edge
anyEdge = lift . Neo.allEdges >=> eachS

neighbour :: (Proxy p,Monad m,Monad (p C () () Node (NeoT m))) => Node -> ProduceT p (NeoT m) Node
neighbour node = RespondT (do
    runRespondT (previous node)
    runRespondT (next node))

target :: (Proxy p,Monad m) => Edge -> ProduceT p (NeoT m) Node
target = lift . Neo.target

source :: (Proxy p,Monad m) => Edge -> ProduceT p (NeoT m) Node
source = lift . Neo.source



-- | The direction of an edge.
data Direction = In | Out | Both
{-
-- | Jump a node following edges as specified.
jump :: (Proxy p) => Direction -> Node -> ProduceT (ExceptionP p) SafeIO Node
jump direction = follow direction >=> target

-- | A next node following only edges with the given label
jumpLabeled :: (Proxy p) => Direction -> Text -> Node -> ProduceT (ExceptionP p) SafeIO Node
jumpLabeled direction edgelabel = followLabeled direction edgelabel >=> target

-- | Any outgoing Edge.
follow :: (Proxy p) => Direction -> Node -> ProduceT (ExceptionP p) SafeIO Relationship
follow direction node = RespondT (do
    result <- tryIO (getRelationships defaultClient (toRetreivalType direction) node)
    either (throw . PipesGremlinError) respond result) >>= eachS

-- | An outgoing Edge with the given label
followLabeled :: (Proxy p) => Direction -> Text -> Node -> ProduceT (ExceptionP p) SafeIO Relationship
followLabeled direction edgelabel node = do
    edge <- follow direction node
    guard (pack (relationshipType edge) == edgelabel)
    return edge

-- | A next node following any edge.
next :: (Proxy p) => Node -> ProduceT (ExceptionP p) SafeIO Node
next = jump Out

-- | A next node following only edges with the given label
nextLabeled :: (Proxy p) => Text -> Node -> ProduceT (ExceptionP p) SafeIO Node
nextLabeled = jumpLabeled Out

-- | Any outgoing Edge.
outEdge :: (Proxy p) => Node -> ProduceT (ExceptionP p) SafeIO Relationship
outEdge = follow Out

-- | An outgoing Edge with the given label
outEdgeLabeled :: (Proxy p) => Text -> Node -> ProduceT (ExceptionP p) SafeIO Relationship
outEdgeLabeled = followLabeled Out

-- | A previous node following any edge backwards.
previous :: (Proxy p) => Node -> ProduceT (ExceptionP p) SafeIO Node
previous = jump In

-- | A previous node following backwards only edges with the given label.
previousLabeled :: (Proxy p) => Text -> Node -> ProduceT (ExceptionP p) SafeIO Node
previousLabeled = jumpLabeled In

-- | Any incoming edge.
inEdge :: (Proxy p) => Node -> ProduceT (ExceptionP p) SafeIO Relationship
inEdge = follow In

-- | An incoming edge with the given label.
inEdgeLabeled :: (Proxy p) => Text -> Node -> ProduceT (ExceptionP p) SafeIO Relationship
inEdgeLabeled = followLabeled In

-- | A neighbouring node following any edge in any direction.
neighbour :: (Proxy p) => Node -> ProduceT (ExceptionP p) SafeIO Node
neighbour = jump Both

-- | A neighbouring node following edgews with the given label in any direction.
neighbourLabeled :: (Proxy p) => Text -> Node -> ProduceT (ExceptionP p) SafeIO Node
neighbourLabeled = jumpLabeled Both

-- | Any incoming or outgoing edge.
anyEdge :: (Proxy p) => Node -> ProduceT (ExceptionP p) SafeIO Relationship
anyEdge = follow Both

-- | Any incoming or outgoing edge with the given label.
anyEdgeLabeled :: (Proxy p) => Text -> Node -> ProduceT (ExceptionP p) SafeIO Relationship
anyEdgeLabeled = followLabeled Both

-- | The source node of an edge.
source :: (Proxy p,Monad m) => Relationship -> ProduceT p m Node
source = return . relationshipFrom

-- | The target node of an edge.
target :: (Proxy p,Monad m) => Relationship -> ProduceT p m Node
target = return . relationshipTo

-- | Gather all results in a list.
gather :: (Monad (Producer p [b] m),Proxy p,Monad m) =>
          ProduceT p m  b ->
          ProduceT p m [b]
gather = RespondT . gatherPipe . runRespondT

-- | Produce each element of the given list.
scatter :: (Proxy p,Monad m) =>
           [b] -> ProduceT p m b
scatter = eachS

-- | Only let an element through if the given pipe produces anything.
has :: (Proxy p,Monad m,Monad (Producer p [a] m)) => ProduceT p m a -> b -> ProduceT p m b
has p b = do
    ensure p
    return b

-- | Only let an element through if the given pipe produces nothing.
hasnot :: (Proxy p,Monad m,Monad (Producer p [a] m)) => ProduceT p m a -> b -> ProduceT p m b
hasnot p b = do
    ensurenot p
    return b

-- | Only continue if the given pipe produces anything.
ensure :: (Proxy p,Monad m,Monad (Producer p [a] m)) =>  ProduceT p m a -> ProduceT p m ()
ensure p = do
    as <- gather p
    guard (not (null as))

-- | Only continue if the given pipe produces nothing.
ensurenot :: (Proxy p,Monad m,Monad (Producer p [a] m)) =>  ProduceT p m a -> ProduceT p m ()
ensurenot p = do
    as <- gather p
    guard (null as)

-- | Filter out only elements satisfying the given predicate.
strain :: (Proxy p,Monad m) => (a -> Bool) -> ProduceT p m a -> ProduceT p m a
strain predicate pipe = do
    a <- pipe
    guard (predicate a)
    return a

-- | A 'Map' of all properties of a node.
properties :: (Proxy p,Monad m) => Node -> ProduceT p m Properties
properties = return . nodeProperties

-- | The property with the given name of the given node.
--   produces nothing if no such property exists.
property :: (Proxy p,Monad m) => Text -> Node -> ProduceT p m Value
property propertyname node = maybe mzero return (lookup propertyname (nodeProperties node))

-- | A 'Map' of all properties of an edge.
edgeProperties :: (Proxy p,Monad m) => Relationship -> ProduceT p m Properties
edgeProperties = return . relationshipProperties

-- | The property with the given name of the given edge.
--   produces nothing if no such property exists.
edgeProperty :: (Proxy p,Monad m) => Text -> Relationship -> ProduceT p m Value
edgeProperty propertyname relationship = maybe
    mzero
    return
    (lookup propertyname (relationshipProperties relationship))

-- | The id of the given node.
nodeId :: (Proxy p,Monad m) => Node -> ProduceT p m NodeID
nodeId = return . getNodeID

-- | The label of the given edge.
label :: (Proxy p,Monad m) => Relationship -> ProduceT p m RelationshipType
label = return . relationshipType

-- | Gather all responds of a pipe into a list.
gatherPipe :: (Monad (p x' x () [b] m),Monad m,Proxy p) =>
              (p x' x ()  b  m r ) ->
              (p x' x () [b] m ())
gatherPipe p = (execWriterK (const (liftP p) >-> toListD >-> unitU) ()) >>= respond

-- | Any Error that might occur during the actual querying of the database.
data PipesGremlinError = PipesGremlinError String deriving (Show,Typeable)

instance Exception PipesGremlinError
-}
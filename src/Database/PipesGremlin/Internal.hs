{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
module Database.PipesGremlin.Internal where

import Pipes (
    ListT(Select),enumerate,each,
    Producer,runEffect,(>->))
import Pipes.Prelude (toListM)
import qualified Pipes.Prelude as Pipes (print)

import Web.Neo (
    defaultRunNeoT,NeoT,
    Node,Edge,Label,Properties,
    CypherQuery,CypherParameters,CypherResult(..))
import qualified Web.Neo as Neo (
    nodeById,nodesByLabel,
    allEdges,incomingEdges,outgoingEdges,
    nodeLabels,nodeProperties,
    edgeLabel,edgeProperties,
    source,target,
    cypher)

import Control.Monad ((>=>),mzero,guard)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Monad.Trans (lift)

import Data.Text (Text)
import Data.Aeson (Value,FromJSON,fromJSON,Result(Error,Success))
import qualified Data.HashMap.Strict as HashMap (fromList,lookup)

type PG m = ListT (NeoT m)

-- | Run the Query as a producer of its results.
runPG :: PG m a -> Producer a (NeoT m) ()
runPG = enumerate

-- | Run the Query and print its results.
printPG :: (Show a,MonadIO m) => PG m a -> m ()
printPG pg = defaultRunNeoT (runEffect (enumerate pg >-> Pipes.print)) >>= liftIO . print

-- | The node with the given Id.
nodeById :: (Monad m) => Integer -> PG m Node
nodeById = lift . Neo.nodeById

-- | All nodes with the given label.
nodesByLabel :: (Monad m) => Label -> PG m Node
nodesByLabel = lift . Neo.nodesByLabel >=> Select . each

outEdge :: (Monad m) => Node -> PG m Edge
outEdge = lift . Neo.outgoingEdges >=> Select . each

inEdge :: (Monad m) => Node -> PG m Edge
inEdge = lift . Neo.incomingEdges >=> Select . each

anyEdge :: (Monad m) => Node -> PG m Edge
anyEdge = lift . Neo.allEdges >=> Select . each

outEdgeLabeled :: (Monad m) => Label -> Node -> PG m Edge
outEdgeLabeled wantedLabel = outEdge >=> hasEdgeLabel wantedLabel

inEdgeLabeled :: (Monad m) => Text -> Node -> PG m Edge
inEdgeLabeled wantedLabel = inEdge >=> hasEdgeLabel wantedLabel

anyEdgeLabeled :: (Monad m) => Text -> Node -> PG m Edge
anyEdgeLabeled wantedLabel = anyEdge >=> hasEdgeLabel wantedLabel

following :: (Monad m) => Node -> PG m Node
following = outEdge >=> target

previous :: (Monad m) => Node -> PG m Node
previous = inEdge >=> source

neighbour :: (Monad m) => Node -> PG m Node
neighbour node = Select (do
    enumerate (previous node)
    enumerate (following node))

followingLabeled :: (Monad m) => Label -> Node -> PG m Node
followingLabeled wantedLabel = outEdgeLabeled wantedLabel >=> target

previousLabeled :: (Monad m) => Label -> Node -> PG m Node
previousLabeled wantedLabel = inEdgeLabeled wantedLabel >=> source

neighbourLabeled :: (Monad m) => Label -> Node -> PG m Node
neighbourLabeled wantedLabel node = Select (do
    enumerate (previousLabeled wantedLabel node)
    enumerate (followingLabeled wantedLabel node))

nodeLabel :: (Monad m) => Node -> PG m Label
nodeLabel = lift . Neo.nodeLabels >=> Select . each

hasNodeLabel :: (Monad m) => Text -> Node -> PG m Node
hasNodeLabel wantedLabel = has (nodeLabel >=> strain (== wantedLabel))

nodeProperty :: (Monad m,FromJSON a) => Text -> Node -> PG m a
nodeProperty key = nodePropertyValue key >=> (\value ->
    case fromJSON value of
        Error _             -> mzero
        Success parsedValue -> return parsedValue)

nodePropertyValue :: (Monad m) => Text -> Node -> PG m Value
nodePropertyValue key = lift . Neo.nodeProperties >=> lookupProperty key

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

edgeProperty :: (Monad m,FromJSON a) => Text -> Edge -> PG m a
edgeProperty key = edgePropertyValue key >=> (\value ->
    case fromJSON value of
        Error _             -> mzero
        Success parsedValue -> return parsedValue)

edgePropertyValue :: (Monad m) => Text -> Edge -> PG m Value
edgePropertyValue key = lift . Neo.edgeProperties >=> lookupProperty key

cypherRows :: (Monad m) => CypherQuery -> CypherParameters -> PG m Properties
cypherRows cypherQuery cypherParameters = do
    cypherResult <- lift (Neo.cypher cypherQuery cypherParameters)
    let toProperties = HashMap.fromList . zip (columnHeaders cypherResult)
    scatter (map toProperties (rowValues cypherResult))   

-- | Gather all results in a list.
gather :: (Monad m) => PG m a -> PG m [a]
gather = lift . toListM . enumerate

-- | Produce each element of the given list.
scatter :: (Monad m) => [a] -> PG m a
scatter = Select . each

-- | Only let an element through if the given pipe produces something.
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


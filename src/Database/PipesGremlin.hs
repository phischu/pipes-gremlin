{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
module Database.PipesGremlin where

import Control.Proxy (Producer,(/>/),(\>\),(>~>),(>>~),runProxy,printD,fromListS,foreverK,Proxy,ProduceT,RespondT(RespondT,runRespondT),Pipe,request,respond,mapD,(>->),eachS,toListD,liftP,ProxyFast)
import Control.Proxy.Class (C)
import Control.Proxy.Trans.Writer (WriterP,execWriterK)
import Control.Proxy.Trans.Identity (runIdentityK)
import Control.Proxy.Safe (ExceptionP,SafeIO,tryIO,throw)

import Database.Neo4j (
    NodeID,Node,defaultClient,lookupNode,getNodeID,
    Relationship,RelationshipType,relationshipType,outgoingRelationships,
    relationshipTo)

import Control.Monad (forever,liftM,(>=>),replicateM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer (WriterT,tell,execWriterT)
import Control.Exception (Exception)
import Data.Typeable (Typeable)

vertex :: (Proxy p) => NodeID -> ProduceT (ExceptionP p) SafeIO Node
vertex nodeid = RespondT (tryIO (lookupNode defaultClient nodeid) >>= either (throw.PipesGremlinError) respond)

nodeid :: (Proxy p,Monad m) => Node -> ProduceT p m NodeID
nodeid = return . getNodeID

edgelabel :: (Proxy p,Monad m) => Relationship -> ProduceT p m RelationshipType
edgelabel = return . relationshipType

out :: (Proxy p) => Node -> ProduceT (ExceptionP p) SafeIO Node
out = outEdges >=> inVertices

outEdges :: (Proxy p) => Node -> ProduceT (ExceptionP p) SafeIO Relationship
outEdges node = RespondT (do
	result <- tryIO (outgoingRelationships defaultClient node)
	either (throw . PipesGremlinError) respond result) >>= eachS

inVertices :: (Proxy p,Monad m) => Relationship -> ProduceT p m Node
inVertices = return . relationshipTo

gatherK :: (Monad (p C () () [b] m),Proxy p,Monad m) => (a -> ProduceT p m b) -> (a -> ProduceT p m [b])
gatherK = (gather .)

gather :: (Monad (Producer p [b] m),Proxy p,Monad m) => ProduceT p m b -> ProduceT p m [b]
gather = RespondT . gatherPipe . runRespondT

gatherPipe :: (Monad (p x' x () [b] m),Monad m,Proxy p) =>
              (p x' x ()  b  m r ) ->
              (p x' x () [b] m ())
gatherPipe p = (execWriterK (const (liftP p) >-> toListD >-> exhaust) ()) >>= respond

exhaust :: (Monad (p a' a b' b1 m), Monad m, Proxy p) =>
           a' -> p a' a b' b1 m b
exhaust x = forever (request x)

scatter :: (Proxy p,Monad m) =>
           [b] -> ProduceT p m b
scatter = eachS

data PipesGremlinError = PipesGremlinError String deriving (Show,Typeable)

instance Exception PipesGremlinError

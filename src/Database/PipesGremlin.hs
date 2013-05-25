{-# LANGUAGE DeriveDataTypeable #-}
module Database.PipesGremlin where

import Control.Proxy (Proxy,ProduceT,RespondT(RespondT),Pipe,request,respond,mapD,(>->),eachS)
import Control.Proxy.Safe (ExceptionP,SafeIO,tryIO,throw)

import Database.Neo4j (
    NodeID,Node,defaultClient,lookupNode,getNodeID,
    Relationship,RelationshipType,relationshipType,outgoingRelationships,
    relationshipTo)

import Control.Monad (forever,liftM,(>=>))
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

data PipesGremlinError = PipesGremlinError String deriving (Show,Typeable)

instance Exception PipesGremlinError

{-# LANGUAGE DeriveDataTypeable #-}
module Database.PipesGremlin where

import Control.Proxy (Proxy,Producer,Pipe,request,respond,mapD,(>->))
import Control.Proxy.Safe (ExceptionP,SafeIO,tryIO,throw)

import Database.Neo4j (
    NodeID,Node,defaultClient,lookupNode,getNodeID,
    Relationship,RelationshipType,relationshipType,typedRelationships,
    relationshipTo)

import Control.Exception (Exception,toException)
import Data.Typeable (Typeable)

vertex :: (Proxy p) => NodeID ->  () -> Producer (ExceptionP p) Node SafeIO ()
vertex nodeid () = tryIO (lookupNode defaultClient nodeid) >>= either (throw.PipesGremlinError) respond

nodeid :: (Proxy p,Monad m) => () -> Pipe p Node NodeID m r
nodeid = mapD getNodeID

edgelabel :: (Proxy p,Monad m) => () -> Pipe p Relationship RelationshipType m r
edgelabel = mapD relationshipType

out :: (Proxy p) => RelationshipType -> () -> Pipe (ExceptionP p) Node Node SafeIO ()
out label = outEdges label >-> inVertices

outEdges :: (Proxy p) => RelationshipType -> () -> Pipe (ExceptionP p) Node Relationship SafeIO ()
outEdges label () = do
	node <- request ()
	tryIO (typedRelationships defaultClient label node) >>= either (throw.PipesGremlinError) (mapM_ respond)

inVertices :: (Proxy p,Monad m) => () -> Pipe p Relationship Node m r
inVertices = mapD relationshipTo

data PipesGremlinError = PipesGremlinError String deriving (Show,Typeable)

instance Exception PipesGremlinError

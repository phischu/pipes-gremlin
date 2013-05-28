{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
module Database.PipesGremlin where

import Control.Proxy ((/>/),(\>\),(>~>),(>>~),runProxy,printD,fromListS,foreverK,Proxy,ProduceT,RespondT(RespondT,runRespondT),Pipe,request,respond,mapD,(>->),eachS,toListD,liftP,ProxyFast)
import Control.Proxy.Trans.Writer (WriterP,execWriterK)
import Control.Proxy.Trans.Identity (runIdentityK)
import Control.Proxy.Safe (ExceptionP,SafeIO,tryIO,throw)

import Database.Neo4j (
    NodeID,Node,defaultClient,lookupNode,getNodeID,
    Relationship,RelationshipType,relationshipType,outgoingRelationships,
    relationshipTo)

import Control.Monad (forever,liftM,(>=>),replicateM_)
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

gather :: (Proxy p,Monad m) => (a -> ProduceT p m b) -> (a -> ProduceT p m [b])
gather = undefined

from :: (Monad (p x' a x' b m),Monad m,Proxy p) => (a -> RespondT p x' a x' m b) -> x' -> p x' a x' b m r
from p = foreverK (request >=> runRespondT . p)

--to :: (Proxy p,Monad (p a' a1 b' b' m),Monad m) => (a -> p b' b' b' b m b') -> a -> RespondT p a' a1 b' m b
to p = RespondT . (respond >-> p)

gatherK :: (Monad (p a' a b' [b] m), Monad m, Proxy p) => (b' -> p a' a b' b m r) -> b' -> p a' a b' [b] m r
gatherK p = foreverK ((execWriterK (liftP . p >-> toListD >-> exhaust)) >=> respond)

yoyo = runProxy (runIdentityK (fromListS [1,2,3] >-> gatherK f >-> printD))

exhaust :: (Monad (p a' a b' b1 m), Monad m, Proxy p) =>
           a' -> p a' a b' b1 m b
exhaust x = forever (request x)

f () = do
	x <- request ()
	replicateM_ x (respond (show x))

scatter :: (Proxy p,Monad m) => [b] -> ProduceT p m b
scatter = eachS

data PipesGremlinError = PipesGremlinError String deriving (Show,Typeable)

instance Exception PipesGremlinError

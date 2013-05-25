{-# LANGUAGE DeriveDataTypeable #-}
module Database.PipesGremlin where

import Control.Proxy (Proxy,Producer,respond)
import Control.Proxy.Safe (ExceptionP,SafeIO,tryIO,throw)

import Database.Neo4j (NodeID,Node,defaultClient,lookupNode)

import Control.Exception (Exception,toException)
import Data.Typeable (Typeable)

v :: (Proxy p) => NodeID ->  () -> Producer (ExceptionP p) Node (SafeIO) ()
v nodeid () = tryIO (lookupNode defaultClient nodeid) >>= either (throw.NodeLookupError) respond

data PipesGremlinError = NodeLookupError String deriving (Show,Typeable)

instance Exception PipesGremlinError

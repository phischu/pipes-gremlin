module Database.PipesGremlinTest where

import Control.Proxy (Proxy,ProxyFast,(>->),printD,runProxy,filterD,RespondT(runRespondT),ProduceT)
import Control.Proxy.Trans.Either (runEitherK)
import Control.Proxy.Trans.Identity (runIdentityK)
import Control.Proxy.Safe (ExceptionP,SafeIO,trySafeIO,tryK)

import Database.Neo4j (Node)

import Database.PipesGremlin

runTest :: ProduceT (ExceptionP ProxyFast) SafeIO String -> IO ()
runTest test = (trySafeIO $ runProxy $ runEitherK $ (const $ runRespondT test) >-> tryK printD) >>= print

test :: ProduceT (ExceptionP ProxyFast) SafeIO String
test = vertex 1 >>= out >>= out >>= outEdges >>= edgelabel

module Database.PipesGremlinTest where

import Control.Proxy (Proxy,ProxyFast,(>->),printD,runProxy,filterD,RespondT(runRespondT),ProduceT)
import Control.Proxy.Trans.Either (runEitherK)
import Control.Proxy.Trans.Identity (runIdentityK)
import Control.Proxy.Safe (ExceptionP,SafeIO,trySafeIO,tryK)

import Control.Monad ((>=>))

import Database.Neo4j (Node)

import Database.PipesGremlin

runTest :: ProduceT (ExceptionP ProxyFast) SafeIO String -> IO ()
runTest test = (trySafeIO $ runProxy $ runEitherK $ (const $ runRespondT test) >-> tryK printD) >>= print

test :: Integer -> ProduceT (ExceptionP ProxyFast) SafeIO String
test = vertex >=> out >=> out >=> gatherK (outEdges >=> edgelabel) >=> return . show

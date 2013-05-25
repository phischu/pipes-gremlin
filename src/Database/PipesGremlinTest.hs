module Database.PipesGremlinTest where

import Control.Proxy ((>->),printD,runProxy)
import Control.Proxy.Trans.Either (runEitherK)
import Control.Proxy.Safe (trySafeIO,tryK)

import Database.PipesGremlin

test :: IO ()
test = (trySafeIO $ runProxy $ runEitherK $ v 1 >-> tryK printD) >>= print


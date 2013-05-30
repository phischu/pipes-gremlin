{-# LANGUAGE OverloadedStrings #-}
module Database.PipesGremlinTest where

import Control.Proxy (Proxy,ProxyFast,(>->),printD,runProxy,filterD,RespondT(runRespondT),ProduceT)
import Control.Proxy.Trans.Either (runEitherK)
import Control.Proxy.Trans.Identity (runIdentityK)
import Control.Proxy.Safe (ExceptionP,SafeIO,trySafeIO,tryK)

import Control.Monad ((>=>))

import Database.Neo4j (Node)

import Database.PipesGremlin

import Data.Aeson (Value)

runTest :: (Show a) => ProduceT (ExceptionP ProxyFast) SafeIO a -> IO ()
runTest test = (trySafeIO $ runProxy $ runEitherK $ (const $ runRespondT test) >-> tryK printD) >>= print

test :: ProduceT (ExceptionP ProxyFast) SafeIO (Value,[(Value,Value)])
test = do
    package <- nodeById 1 >>= nextLabeled "PACKAGE"
    version <- nextLabeled "VERSION" package
    versionname <- property "versionname" version
    fragments <- gather (do
        variant <- return version >>= nextLabeled "VARIANT"
        modul <- return variant >>= nextLabeled "MODULE"
        fragment <- return modul >>= nextLabeled "FRAGMENT"
        modulename <- property "modulename" modul
        functionname <- property "functionname" fragment
        return (modulename,functionname))
    return (versionname,fragments)

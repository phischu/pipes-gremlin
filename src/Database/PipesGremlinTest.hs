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
    package <- vertex 1 >>= follow "PACKAGE"
    version <- follow "VERSION" package
    versionname <- nodeProperty "versionname" version
    fragments <- gather (do
        variant <- from version >>= follow "VARIANT"
        modul <- from variant >>= follow "MODULE"
        fragment <- from modul >>= follow "FRAGMENT"
        modulename <- nodeProperty "modulename" modul
        functionname <- nodeProperty "functionname" fragment
        return (modulename,functionname))
    return (versionname,fragments)

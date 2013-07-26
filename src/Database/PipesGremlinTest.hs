{-# LANGUAGE OverloadedStrings #-}
module Database.PipesGremlinTest where

import Control.Proxy ((>->),printD,runProxy,hoist)

import Control.Monad.Trans (lift)

import Database.PipesGremlin
import Web.Neo (defaultRunNeoT)

import Data.Aeson (Value)

runTest :: (Show a) => PG IO a -> IO ()
runTest t = (defaultRunNeoT (runProxy (const (runPG t) >-> (hoist (lift.lift) .) printD))) >>= print

test :: (Monad m) => PG m (Value,[(Value,Value)])
test = do
    package <- nodeById 1 >>= nextLabeled "PACKAGE"
    version <- nextLabeled "VERSION" package
    versionname <- nodeProperty "versionname" version
    fragments <- gather (do
        variant <- return version >>= nextLabeled "VARIANT"
        modul <- return variant >>= nextLabeled "MODULE"
        fragment <- return modul >>= nextLabeled "FRAGMENT"
        modulename <- nodeProperty "modulename" modul
        functionname <- nodeProperty "functionname" fragment
        return (modulename,functionname))
    return (versionname,fragments)

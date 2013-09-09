{-# LANGUAGE OverloadedStrings #-}
module Database.PipesGremlinTest where

import Database.PipesGremlin

import Data.Aeson (Value)

test :: (Monad m) => PG m (Value,[(Value,Value)])
test = do
    package <- nodeById 1 >>= followingLabeled "PACKAGE"
    version <- followingLabeled "VERSION" package
    versionname <- nodeProperty "versionname" version
    fragments <- gather (do
        variant <- return version >>= followingLabeled "VARIANT"
        modul <- return variant >>= followingLabeled "MODULE"
        fragment <- return modul >>= followingLabeled "FRAGMENT"
        modulename <- nodeProperty "modulename" modul
        functionname <- nodeProperty "functionname" fragment
        return (modulename,functionname))
    return (versionname,fragments)

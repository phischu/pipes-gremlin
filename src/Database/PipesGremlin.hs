module Database.PipesGremlin (
    -- * Basics
    PG,runPG,printPG,
    -- * Start
    nodeById,nodesByLabel,
    -- * Node
    Node(..),
    outEdge,inEdge,anyEdge,
    following,previous,neighbour,
    outEdgeLabeled,inEdgeLabeled,anyEdgeLabeled,
    followingLabeled,previousLabeled,neighbourLabeled,
    nodeLabel,hasNodeLabel,nodeProperty,nodePropertyValue,
    -- * Edge
    Edge(..),
    source,target,
    edgeLabel,hasEdgeLabel,edgeProperty,edgePropertyValue,
    -- * Control
    gather,scatter,(>=>),
    has,hasnot,strain,ensure,ensurenot
    ) where

import Database.PipesGremlin.Internal

import Web.Neo (Node(..),Edge(..))
import Control.Monad ((>=>))

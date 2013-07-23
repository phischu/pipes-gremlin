module Database.PipesGremlin (
    -- * Start
    nodeById,
    -- * Walk
    next,nextLabeled,outEdge,outEdgeLabeled,
    previous,previousLabeled,inEdge,inEdgeLabeled,
    neighbour,neighbourLabeled,anyEdge,anyEdgeLabeled,
    source,target,
    -- * Control
    gather,scatter,
    has,hasnot,ensure,ensurenot,strain,
    -- * Properties
    properties,property,edgeProperties,edgeProperty,
    nodeId,label,
    -- * Creation
    newNode,newEdge,addNodeProperty,addEdgeProperty
    ) where

import Database.PipesGremlin.Internal


-- | Re-export database types and functions to simplify external use.
module Database.Drasil (
    -- * Chunk
      Chunk
    , mkChunk
    , unChunk
    , chunkType
    
    -- * ChunkDB
    , ChunkDB
    , mkChunkDB
    , find, findOrErr
    , findAll
    , insert
    , registered

    -- * UID
    , UID
    , HasUID(uid)
    , mkUid, (+++), (+++.), (+++!)
    , showUID
) where

{-
  -- * Chunk Database
  -- ** Types
  ChunkDB(symbolTable, termTable, defTable, CDB), RefbyMap, TraceMap, UMap
  -- ** Constructors
  , cdb, idMap, termMap, conceptMap, traceMap, generateRefbyMap
  -- ** Lookup Functions
  , asOrderedList, collectUnits
  , termResolve, defResolve, symbResolve
  , traceLookup, refbyLookup
  , datadefnLookup, insmodelLookup, gendefLookup, theoryModelLookup
  , conceptinsLookup, sectionLookup, labelledconLookup, refResolve
  -- ** Lenses
  , unitTable, traceTable, refbyTable
  , dataDefnTable, insmodelTable, gendefTable, theoryModelTable
  , conceptinsTable, sectionTable, labelledcontentTable, refTable
  -- ** Utility Helper Functions
  -- ChunkDB.GetChunk
  , ccss, ccss', combine, getIdeaDict, vars
  -- * System Information
  , SystemInformation(..), Block(Parallel), sysinfodb
  -- * Reference Database
  , ReferenceDB, RefMap, citeDB, rdb, simpleMap
  , citationDB, conceptDB


import Database.Drasil.ChunkDB
import Database.Drasil.ChunkDB.GetChunk
import Database.Drasil.SystemInformation
-}

import Database.Drasil.Chunk
import Database.Drasil.NewChunkDB
import Database.Drasil.UID

{-# LANGUAGE TypeApplications#-}

module Database.Drasil.NewChunkDB (
      ChunkDB
    , mkChunkDB
    , find, findOrErr
    , findAll
    , insert
    , union
    , registered
) where

import Database.Drasil.Chunk (Chunk, mkChunk, unChunk, chunkType)
import Database.Drasil.UID (UID, HasUID(..))

import qualified Data.Map as M
import Data.List (nub)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Typeable

type ChunkByUID      = M.Map UID Chunk
type ChunksByTypeRep = M.Map TypeRep [Chunk]

newtype ChunkDB = ChunkDB (ChunkByUID, ChunksByTypeRep)

{-
TODO: Discuss important design decisions. These will become rather critical to Drasil projects that rely on ChunkDBs.

1. UID uniqueness and immutability after placement
2. `union`
3. No `ChunkDB`s inside of `ChunkDB`s!


TODO: Near-future:
1. Should all Chunks, on insertion, be checked to ensure that the knowledge they depend on already exists in the ChunkDB?

-}

find :: Typeable a => UID -> ChunkDB -> Maybe a
find u (ChunkDB tc) = do
    c' <- M.lookup u $ fst tc
    unChunk c'

findOrErr :: Typeable a => UID -> ChunkDB -> a
findOrErr u = fromMaybe (error $ "Failed to find chunk " ++ show u) . find u

-- Is this TypeRep really needed? Well, for now, it's a hack to shorten our lists a bit and pre-cache our type lists by their typerep.
-- Justified,... but not optimal. It would be nice if we could have the chunks pre-unChunked or if we could avoid the TypeRep altogether!
-- On the bright side, we get order lists (by insertion order) for cheap!
findAll :: Typeable a => TypeRep -> ChunkDB -> [a]
findAll tr (ChunkDB (_, trm)) = maybe [] (mapMaybe unChunk) (M.lookup tr trm)

insert :: (HasUID a, Typeable a) => ChunkDB -> a -> ChunkDB
insert (ChunkDB (cu, ctr)) c
    | typeRep cu == typeRep (Proxy @ChunkDB) = 
        error "Insertion of ChunkDBs in ChunkDBs is disallowed; please perform unions with them instead."
    | M.member (uid c) cu =
        error $ "Attempting to register a chunk which already contains a UID; `" ++ show (uid c) ++ "`"
    | otherwise           = ChunkDB (cu', ctr')
        where
            c' :: Chunk
            c' = mkChunk c

            cu' :: ChunkByUID
            cu' = M.insert (uid c) c' cu

            ctr' :: ChunksByTypeRep
            ctr' = M.alter (Just . maybe [c'] (++ [c'])) (typeOf c) ctr

union :: ChunkDB -> ChunkDB -> ChunkDB
union (ChunkDB (lum, ltrm)) (ChunkDB (rum, rtrm)) = ChunkDB (um, trm)
    where
        um :: ChunkByUID
        um = M.unionWith (\conflict _ -> error $ "Unioned ChunkDBs contains at least one UID collision; `" ++ show (uid conflict) ++ "`!") lum rum
        
        -- NOTE: This is an important design decision, we are joining with preference for assuming that the LEFT comes first (in other words, is the "earlier" discovered information)
        trm :: ChunksByTypeRep
        trm = M.unionWith (++) ltrm rtrm

mkChunkDB :: [Chunk] -> ChunkDB
mkChunkDB cs = ChunkDB (cbu, csbtr)
    where
        cbu :: ChunkByUID
        cbu = M.fromListWithKey (\k _ _ -> error $ "At least 2 chunks provided contain the same UID; `" ++ show k ++ "`") $ map (\c -> (uid c, c)) cs

        trs :: [TypeRep]
        trs = nub $ map chunkType cs

        trcs :: [(TypeRep, [Chunk])]
        trcs = map (\tr -> (tr, filter ((==) tr . chunkType) cs)) trs

        csbtr :: ChunksByTypeRep
        csbtr = M.fromList trcs

registered :: ChunkDB -> [UID]
registered (ChunkDB (x, _)) = M.keys x

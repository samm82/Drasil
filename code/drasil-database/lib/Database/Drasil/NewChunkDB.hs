{-# LANGUAGE TypeApplications#-}

module Database.Drasil.NewChunkDB (
      ChunkDB
    , empty
    , mkChunkDB
    , find, findOrErr
    , findRefs, findRefsOrErr
    , findAll
    , insert, insertAll, insertAllOrIgnore
    , union
    , registered, isRegistered
    , refbyTable -- FIXME: This function should be re-examined. Some functions can probably be moved here!
) where

import Database.Drasil.Chunk (Chunk, mkChunk, unChunk, chunkType, HasChunkRefs (chunkRefs))
import Database.Drasil.UID (UID, HasUID(..))

import qualified Data.Map as M
import Data.List (nub)
import Data.Maybe (fromMaybe, mapMaybe, isJust)
import Data.Typeable

type ReferredBy      = [UID]
type ChunkByUID      = M.Map UID (Chunk, ReferredBy)
type ChunksByTypeRep = M.Map TypeRep [Chunk]

newtype ChunkDB = ChunkDB (ChunkByUID, ChunksByTypeRep)

empty :: ChunkDB
empty = ChunkDB (M.empty, M.empty)

{-
TODO: Discuss important design decisions. These will become rather critical to Drasil projects that rely on ChunkDBs.

1. UID uniqueness and immutability after placement
2. `union`
3. No `ChunkDB`s inside of `ChunkDB`s!
4. Try to avoid manually written UID references.


TODO: Near-future:
1. Should all Chunks, on insertion, be checked to ensure that the knowledge they depend on already exists in the ChunkDB?

-}

find :: Typeable a => UID -> ChunkDB -> Maybe a
find u (ChunkDB (tc, _)) = do
    (c', _) <- M.lookup u tc
    unChunk c'

findOrErr :: Typeable a => UID -> ChunkDB -> a
findOrErr u = fromMaybe (error $ "Failed to find chunk " ++ show u) . find u

-- Is this TypeRep really needed? Well, for now, it's a hack to shorten our lists a bit and pre-cache our type lists by their typerep.
-- Justified,... but not optimal. It would be nice if we could have the chunks pre-unChunked or if we could avoid the TypeRep altogether!
-- On the bright side, we get order lists (by insertion order) for cheap!
findAll :: Typeable a => TypeRep -> ChunkDB -> [a]
findAll tr (ChunkDB (_, trm)) = maybe [] (mapMaybe unChunk) (M.lookup tr trm)

findRefs :: UID -> ChunkDB -> Maybe [UID]
findRefs u (ChunkDB (tc, _)) = do
    (_, refs) <- M.lookup u tc
    Just refs

findRefsOrErr :: UID -> ChunkDB -> [UID]
findRefsOrErr u = fromMaybe (error $ "Failed to find references for unknown chunk " ++ show u) . find u

insert :: (HasUID a, HasChunkRefs a, Typeable a) => ChunkDB -> a -> ChunkDB
insert (ChunkDB (cu, ctr)) c
    | typeOf c == typeRep (Proxy @ChunkDB) =
        error "Insertion of ChunkDBs in ChunkDBs is disallowed; please perform unions with them instead."
    | M.member (uid c) cu =
        error $ "Attempting to register a chunk which already contains a UID; `" ++ show (uid c) ++ "`"
    | otherwise           = ChunkDB (finalCu, ctr')
        where
            c' :: Chunk
            c' = mkChunk c

            cu' :: ChunkByUID
            cu' = M.insert (uid c) (c', []) cu -- insert our chunk, it is not currently referred by anything.

            insertRefExpectingExistence :: UID -> ChunkByUID -> ChunkByUID
            insertRefExpectingExistence u cbu = if isJust prev
                                            then cbu'
                                            else error $ "Referred knowledge is missing for `" ++ show (uid c) ++ "`; needs `" ++ show u ++ "`"
                where
                    (prev, cbu') = M.insertLookupWithKey (\_ _ (rcc, rcref) -> (rcc, u : rcref)) u (undefined, []) cbu

            finalCu :: ChunkByUID
            finalCu = foldr insertRefExpectingExistence cu' $ chunkRefs c

            ctr' :: ChunksByTypeRep
            ctr' = M.alter (Just . maybe [c'] (++ [c'])) (typeOf c) ctr

insertAll :: (HasUID a, HasChunkRefs a, Typeable a) => ChunkDB -> [a] -> ChunkDB
insertAll = foldr (flip insert)

insertAllOrIgnore :: (HasUID a, HasChunkRefs a, Typeable a) => ChunkDB -> [a] -> ChunkDB
insertAllOrIgnore cdb = foldr (\next old -> if isRegistered (uid next) cdb then old else insert old next) cdb

union :: ChunkDB -> ChunkDB -> ChunkDB
union (ChunkDB (lum, ltrm)) (ChunkDB (rum, rtrm)) = ChunkDB (um, trm)
    where
        um :: ChunkByUID
        um = M.unionWith (\(conflict, _) _ -> error $ "Unioned ChunkDBs contains at least one UID collision; `" ++ show (uid conflict) ++ "`!") lum rum

        -- NOTE: This is an important design decision, we are joining with preference for assuming that the LEFT comes first (in other words, is the "earlier" discovered information)
        trm :: ChunksByTypeRep
        trm = M.unionWith (++) ltrm rtrm

mkChunkDB :: [Chunk] -> ChunkDB
mkChunkDB cs = ChunkDB (cbu, csbtr)
    where
        cbu :: ChunkByUID -- TODO: build a proper reference list, post-facto
        cbu = M.fromListWithKey (\k (r1, _) (r2, _) ->
                error $ "At least 2 chunks provided contain the same UID, `"
                ++ show k ++ "`, with types: " ++ show (chunkType r1)
                ++ " and " ++ show (chunkType r2)
            )
            $ map (\c -> (uid c, (c, []))) cs

        trs :: [TypeRep]
        trs = nub $ map chunkType cs

        trcs :: [(TypeRep, [Chunk])]
        trcs = map (\tr -> (tr, filter ((==) tr . chunkType) cs)) trs

        csbtr :: ChunksByTypeRep
        csbtr = M.fromList trcs

registered :: ChunkDB -> [UID]
registered (ChunkDB (x, _)) = M.keys x

isRegistered :: UID -> ChunkDB -> Bool
isRegistered u (ChunkDB (x, _)) = M.member u x

{- FIXME: TO BE REWRITTEN -}
refbyTable :: ChunkDB -> M.Map UID [UID]
refbyTable (ChunkDB (x, _)) = M.map snd x

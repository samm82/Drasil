module Database.Drasil.NewChunkDB (
      ChunkDB
    , mkChunkDB
    , find, findOrErr
    , findAll
    , insert
) where

import Database.Drasil.Chunk (Chunk, mkChunk, unChunk, chunkType)
import Database.Drasil.UID (UID, HasUID(..))

import qualified Data.Map as M
import Data.List (nub)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Typeable (Typeable, typeOf, TypeRep)

type ChunkByUID      = M.Map UID Chunk
type ChunksByTypeRep = M.Map TypeRep [Chunk]

newtype ChunkDB = ChunkDB (ChunkByUID, ChunksByTypeRep)

find :: Typeable a => UID -> ChunkDB -> Maybe a
find u (ChunkDB tc) = do
    c' <- M.lookup u $ fst tc
    unChunk c'

findOrErr :: Typeable a => UID -> ChunkDB -> a
findOrErr u = fromMaybe (error $ "Failed to find chunk " ++ show u) . find u

-- Is this TypeRep really needed? Well, for now, it's like a hack to shorten our lists a bit and pre-cache our type lists by their typerep.
-- Justified, but not optimal. It would be nice if we could have the chunks pre-unChunked.
findAll :: Typeable a => TypeRep -> ChunkDB -> [a]
findAll tr (ChunkDB (_, trm)) = maybe [] (mapMaybe unChunk) (M.lookup tr trm)

insert :: (HasUID a, Typeable a) => ChunkDB -> a -> ChunkDB
insert (ChunkDB (cu, ctr)) c
    | M.member (uid c) cu = error $ "Attempting to register a chunk which already contains a UID; `" ++ show (uid c) ++ "`"
    | otherwise           = ChunkDB (cu', ctr')
        where
            c' :: Chunk
            c' = mkChunk c

            cu' :: ChunkByUID
            cu' = M.insert (uid c) c' cu

            ctr' :: ChunksByTypeRep
            ctr' = M.alter (Just . maybe [c'] ([c'] ++)) (typeOf c) ctr

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

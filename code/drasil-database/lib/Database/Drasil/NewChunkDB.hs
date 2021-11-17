module Database.Drasil.NewChunkDB (
      ChunkDB
    , mkChunkDB
    , lookup, lookupOrErr
    , findAll
    , insert
) where

import Prelude hiding (lookup)

import Database.Drasil.Chunk
import Database.Drasil.UID

import qualified Data.Map as M
import Data.List hiding (lookup, insert)
import Data.Maybe
import Data.Typeable

type ChunkByUID      = M.Map UID Chunk
type ChunksByTypeRep = M.Map TypeRep [Chunk]

newtype ChunkDB = ChunkDB (ChunkByUID, ChunksByTypeRep)

lookup :: Typeable a => UID -> ChunkDB -> Maybe a
lookup u (ChunkDB tc) = do
    c' <- M.lookup u $ fst tc
    unChunk c'

lookupOrErr :: Typeable a => UID -> ChunkDB -> a
lookupOrErr u = fromMaybe (error $ "Failed to find chunk " ++ show u) . lookup u

-- Is this TypeRep really needed? Well, for now, it's just a hacky to shorten our lists a bit.
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
            ctr' = M.alter (maybe (Just [c']) (Just . (++) [c'])) (typeOf c) ctr

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

{-# LANGUAGE ExistentialQuantification #-}
module Database.Drasil.Chunk (
      Chunk
    , mkChunk
    , unChunk
    , chunkType
) where

import Database.Drasil.UID (HasUID(..))
import Data.Typeable (Typeable, cast, TypeRep, typeOf)


data Chunk = forall a. (HasUID a, Typeable a) => Chunk a

mkChunk :: (HasUID a, Typeable a) => a -> Chunk
mkChunk = Chunk

unChunk :: Typeable a => Chunk -> Maybe a
unChunk = cast

chunkType :: Chunk -> TypeRep
chunkType (Chunk c) = typeOf c

instance HasUID Chunk where
    uid (Chunk t) = uid t


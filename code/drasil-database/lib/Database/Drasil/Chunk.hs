{-# LANGUAGE ExistentialQuantification #-}
module Database.Drasil.Chunk (
      Chunk
    , HasChunkRefs(..)
    , mkChunk
    , unChunk
    , chunkType
) where

import Database.Drasil.UID (HasUID(..), UID)
import Data.Typeable (Typeable, cast, TypeRep, typeOf)


-- TODO: Is this just the ConceptDomain?
class HasChunkRefs a where
  chunkRefs :: a -> [UID]

data Chunk = forall a. (HasUID a, HasChunkRefs a, Typeable a) => Chunk a

instance Eq Chunk where
  l == r = uid l == uid r

instance HasUID Chunk where
    uid (Chunk t) = uid t

mkChunk :: (HasUID a, HasChunkRefs a, Typeable a) => a -> Chunk
mkChunk = Chunk

unChunk :: Typeable a => Chunk -> Maybe a
unChunk = cast

chunkType :: Chunk -> TypeRep
chunkType (Chunk c) = typeOf c


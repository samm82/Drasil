{-# LANGUAGE GADTs #-}

module Language.Drasil.Literal.Lang where

import Database.Drasil (HasChunkRefs (chunkRefs))

data Literal where
    Int      :: Integer -> Literal
    Str      :: String -> Literal
    Dbl      :: Double -> Literal
    ExactDbl :: Integer -> Literal
    Perc     :: Integer -> Integer -> Literal
    deriving Eq

instance HasChunkRefs Literal where
    chunkRefs _ = []

{- TODO: When typing the Expression language, this will be usable
instance Eq (Literal a) where
    (Int l)      == (Int r)      =  l == r
    (Str l)      == (Str r)      =  l == r
    (Dbl l)      == (Dbl r)      =  l == r
    (ExactDbl l) == (ExactDbl r) =  l == r
    (Perc l1 l2) == (Perc r1 r2) =  l1 == r1 && l2 == r2
    _            == _            =  False
-}

{-# LANGUAGE TemplateHaskell, Rank2Types, ScopedTypeVariables, PostfixOperators  #-}
module Theory.Drasil.ModelKinds (ModelKinds(..), RealmVariant(..),
  setMk, elimMk, lensMk, getEqModQds) where

import Control.Lens ((^.), to, lens, set, makeLenses, makeLensesFor,
  Getter, Lens', Setter')
import Data.Maybe (mapMaybe)
import Data.List (union)
import qualified Data.List.NonEmpty as NE

import Language.Drasil (($=), sy, Expr, RelationConcept,
  NamedIdea(..), HasUID(..), ExprRelat(..), ConceptDomain(..), Definition(..),
  Idea(..), DefiningExpr(..), UID, Sentence, QDefinition, QuantityDict)

-- | 'RealmVariant's are partial components of QDefinitions, containing only
--   the defining expressions and descriptions.
--   Any 'RealmVariant' paired with a 'QuantityDict' will form a "whole" 'QDefinition'.
data RealmVariant = RV {
  _cd   :: [UID],
  _desc :: Sentence,
  _expr :: Expr
}

makeLensesFor [("_expr", "expr")] ''RealmVariant

-- | Models can be of different kinds: 
--
--     * 'EquationalModel's represent quantities that are calculated via a single definition/'QDefinition'.
--     * 'EquationalRealm's represent quantities that may be calculated using any one of many 'RealmVariant's (e.g., 'x = A = ... = Z')
--     * 'DEModel's represent differential equations as 'RelationConcept's
--     * 'OthModel's are placeholders for models. No new 'OthModel's should be created, they should be using one of the other kinds.
data ModelKinds = EquationalModel QDefinition
                | EquationalRealm QuantityDict (NE.NonEmpty RealmVariant)
                | DEModel RelationConcept
                | OthModel RelationConcept

makeLenses ''ModelKinds

-- | Finds the 'UID' of the 'ModelKinds'.
instance HasUID        ModelKinds where uid      = lensMk uid uid uid
-- | Finds the term ('NP') of the 'ModelKinds'.
instance NamedIdea     ModelKinds where term     = lensMk term term term
-- | Finds the idea of the 'ModelKinds'.
instance Idea          ModelKinds where getA     = elimMk (to getA) (to getA) (to getA)
-- | Finds the definition of the 'ModelKinds'.
instance Definition    ModelKinds where defn     = lensMk defn (error "ambiguous definition (defn) in EquationalRealm") defn
-- | Finds the domain of the 'ModelKinds'.
instance ConceptDomain ModelKinds where
  cdom (EquationalRealm _ rvs) = foldr1 union $ NE.toList (NE.map _cd rvs)
  cdom m                       = elimMk (to cdom) undefined (to cdom) m
-- | Finds the defining expression of the 'ModelKinds'.
instance DefiningExpr  ModelKinds where defnExpr = lensMk defnExpr (error "ambiguous defining expression (defnExpr) for EquationalRealm") defnExpr
-- | Finds the relation expression of the 'ModelKinds'.
instance ExprRelat     ModelKinds where 
  relat (EquationalRealm q rs) = sy q $= foldr1 ($=) (NE.map (^. expr) rs)
  relat (EquationalModel q)    = relat q
  relat (DEModel q)            = relat q
  relat (OthModel q)           = relat q

-- TODO: implement MayHaveUnit for ModelKinds once we've sufficiently removed OthModels & RelationConcepts (else we'd be breaking too much of `stable`)

-- | Retrieve internal data from ModelKinds
elimMk :: Getter QDefinition a -> Getter QuantityDict a -> Getter RelationConcept a -> ModelKinds -> a
elimMk l _ _ (EquationalModel q)   = q ^. l
elimMk _ l _ (EquationalRealm q _) = q ^. l
elimMk _ _ l (DEModel q)           = q ^. l
elimMk _ _ l (OthModel q)          = q ^. l

-- | Map into internal representations of ModelKinds
setMk :: ModelKinds -> Setter' QDefinition a -> Setter' QuantityDict a -> Setter' RelationConcept a -> a -> ModelKinds
setMk (EquationalModel q)    f _ _ x = EquationalModel $ set f x q
setMk (EquationalRealm q vs) _ f _ x = EquationalRealm (set f x q) vs
setMk (DEModel q)            _ _ g x = DEModel $ set g x q
setMk (OthModel q)           _ _ g x = OthModel $ set g x q

-- | Make a 'Lens' for 'ModelKinds'.
lensMk :: forall a. Lens' QDefinition a -> Lens' QuantityDict a -> Lens' RelationConcept a -> Lens' ModelKinds a
lensMk lq lqd lr = lens g s
    where g :: ModelKinds -> a
          g mk = elimMk lq lqd lr mk
          s :: ModelKinds -> a -> ModelKinds
          s mk_ x = setMk mk_ lq lqd lr x

-- | Extract a list of 'QDefinition's from a list of 'ModelKinds'.
getEqModQds :: [ModelKinds] -> [QDefinition]
getEqModQds = mapMaybe isEqMod
  where
    isEqMod (EquationalModel f) = Just f
    isEqMod _                   = Nothing

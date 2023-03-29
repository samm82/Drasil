{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
module Language.Drasil.Chunk.IntLinProgModel (
  -- * Export Data Type
  IntLinProgModel(..), --ODESolverFormat(..), InitialValueProblem(..),
  -- * Input Language
  -- ($^^), ($*), ($+),
  -- * Constructors
  formCanonILP, maxILP, minILP
  -- makeAODESolverFormat, makeAIVP, makeASystemDE, makeASingleDE
) where

import Prelude hiding (Real)
import Control.Lens (makeLenses, (^.), view)
-- import Data.List (find)
import qualified Data.List.NonEmpty as NE

import Language.Drasil.Chunk.Concept (ConceptChunk, dcc)
-- import Language.Drasil.Chunk.Constrained (ConstrConcept)
import Language.Drasil.Chunk.DefinedQuantity (DefinedQuantityDict)
-- import Language.Drasil.Chunk.Quantity (qw)
-- import Language.Drasil.Chunk.Unital (UnitalChunk)
import Language.Drasil.Classes (Express(..),
  ConceptDomain(..), Definition(..), Idea(..), NamedIdea(..))
import Language.Drasil.Expr.Class (ExprC(..), ($.)) --, columnVec)
import Language.Drasil.Expr.Lang (Expr(..))
-- import Language.Drasil.Literal.Class (LiteralC(exactDbl, int))
-- import Language.Drasil.ModelExpr.Class (ModelExprC(nthderiv, equiv))
import Language.Drasil.ModelExpr.Convert (expr)
import Language.Drasil.ModelExpr.Lang (ModelExpr)
-- import Language.Drasil.NounPhrase.Core (NP)
import Language.Drasil.NounPhrase (cn')
-- import Language.Drasil.Sentence (Sentence)
import Language.Drasil.Space (Space, HasSpace(..), getInnerSpace)
import Language.Drasil.UID (HasUID(uid))
import Language.Drasil.WellTyped (RequiresChecking (requiredChecks))

-- -- | Unknown is nth order of the dependent variable 
-- type Unknown = Integer

-- -- | Term consist of a coefficient and an unknown (order)
-- data Term = T{
--   -- | the coefficient
--   _coeff :: Expr,
--   -- | the order
--   _unk :: Unknown
-- }
-- makeLenses ''Term

-- -- | LHS is a collection of Terms
-- type LHS = [Term]

-- -- | Operation connect the dependent variable and the order
-- {-
--   e.g. depVar $^^ d
--   note: depVar is a dummy variable. It keeps the shape of the syntax.
-- -}
-- ($^^) :: ConstrConcept -> Integer -> Unknown
-- ($^^) _ unk' = unk'

-- -- | Operation represent multiple
-- {-
--   e.g. exactDbl 1 $* (opProcessVariable $^^ 2), 
--   exactDbl 1 is the the coefficient, 
--   (opProcessVariable $^^ 2) is the 2rd order of opProcessVariable
-- -}
-- ($*) :: Expr -> Unknown -> Term
-- ($*) = T

-- -- | Operation represent plus (collection Terms)
-- {-
--   e.g. [exactDbl 1 $* (opProcessVariable $^^ 2)]
--        $+ (exactDbl 1 `addRe` sy qdDerivGain $* (opProcessVariable $^^ 1))
--   [exactDbl 1 $* (opProcessVariable $^^ 2)] is a collection with a single Term, 
--   (exactDbl 1 `addRe` sy qdDerivGain $* (opProcessVariable $^^ 1)) is the appended element
-- -}
-- ($+) :: [Term] -> Term -> LHS
-- ($+) xs x  = xs ++ [x]

data Extremum = Min | Max

-- | Describe the structural content of a system of linear ODEs with six necessary fields
data IntLinProgModel = ILP {
  -- | meta data
  _ilpconc :: ConceptChunk,
  -- | whether it is a minimization or a maximization problem
  _ext :: Extremum,
  -- | the value to solve for
  _x :: DefinedQuantityDict,
  -- | the weight used in the objective function
  _c :: DefinedQuantityDict,
  -- | constraints on x
  _cons :: NE.NonEmpty Expr
}
makeLenses ''IntLinProgModel

-- | Finds the 'UID' of the 'ConceptChunk' used to make the 'IntLinProgModel'.
instance HasUID        IntLinProgModel where uid = ilpconc . uid
-- | Equal if 'UID's are equal.
instance Eq            IntLinProgModel where a == b = (a ^. uid) == (b ^. uid)
-- | Finds the term ('NP') of the 'ConceptChunk' used to make the 'IntLinProgModel'.
instance NamedIdea     IntLinProgModel where term = ilpconc . term
-- | Finds the idea contained in the 'ConceptChunk' used to make the 'IntLinProgModel'.
instance Idea          IntLinProgModel where getA = getA . view ilpconc
-- | Finds the definition contained in the 'ConceptChunk' used to make the 'IntLinProgModel'.
instance Definition    IntLinProgModel where defn = ilpconc . defn
-- | Finds the domain of the 'ConceptChunk' used to make the 'IntLinProgModel'.
instance ConceptDomain IntLinProgModel where cdom = cdom . view ilpconc
-- | Convert the 'IntLinProgModel' into the model expression language.
instance Express       IntLinProgModel where express = formCanonILP

instance RequiresChecking IntLinProgModel Expr Space where
  requiredChecks ilp = [(makeObjFunc ilp, getInnerSpace $ ilp ^. (c . typ))] 
  --map (, dmo ^. (depVar . typ)) $ formEquations (coeffVects dm) (unknownVect dm) (constantVect dm) (_depVar dmo)
    --where dm = makeAODESolverFormat dmo

-- | Set the expression to be in canonical form
formCanonILP :: IntLinProgModel -> ModelExpr
formCanonILP ilp = expr $ makeObjFunc ilp

-- | Helper for making the objective function (not exported)
makeObjFunc :: IntLinProgModel -> Expr
makeObjFunc ilp = ($.) (sy $ ilp ^. c) (sy $ ilp ^. x)

-- | Smart constructor for minimum ILPS
minILP :: DefinedQuantityDict -> DefinedQuantityDict -> NE.NonEmpty Expr -> IntLinProgModel
minILP = ILP (dcc "minILP" (cn' s) s) Min
  where 
    s = "minimum integer linear program"

-- | Smart constructor for maximum ILPS
maxILP :: DefinedQuantityDict -> DefinedQuantityDict -> NE.NonEmpty Expr -> IntLinProgModel
maxILP = ILP (dcc "maxILP" (cn' s) s) Max
  where 
    s = "maximum integer linear program"
{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.DifferentialModel (
  -- * Export Data Type
  DifferentialModel(..), ODESolverFormat(..), InitialValueProblem(..),
  -- * Input Language
  ($^^), ($*), ($+),
  -- * Constructors
  makeAODESolverFormat, makeAIVP, makeASystemDE, makeASingleDE,
  formEquations
) where

import Control.Lens (makeLenses, (^.), view)
import Language.Drasil.Chunk.Concept (ConceptChunk, dccWDS)
import Language.Drasil.UID (HasUID(uid))
import Language.Drasil.Classes (Express(..),
  ConceptDomain(..), Definition(..), Idea(..), NamedIdea(..))
import Language.Drasil.ModelExpr.Lang (ModelExpr)
import Language.Drasil.NounPhrase.Core (NP)
import Language.Drasil.Sentence (Sentence)
import Language.Drasil.Expr.Lang (Expr(..))
import Language.Drasil.Chunk.Unital (UnitalChunk)
import Language.Drasil.ModelExpr.Class (ModelExprC(nthderiv, equiv))
import Language.Drasil.Expr.Class (mulRe, addRe, sy, ExprC (($.), matrix,
  columnVec, ($/), idx), neg)
import Language.Drasil.Chunk.Constrained (ConstrConcept)
import Language.Drasil.Chunk.Quantity (qw)
import Language.Drasil.Literal.Class (LiteralC(exactDbl, int))
import Data.List (find)

-- Unknown is nth degree of derivative of its dependent variable 
type Unknown = Integer

-- Term is the relation of a coefficient and an unknown
data Term = T{
  _coeff :: Expr,
  _unk :: Unknown
}
makeLenses ''Term

-- LHS is a collection of Terms
type LHS = [Term]

{-
  Input Language represent a derivative of a dependent variable
  e.g. depVar $^^ d, 
  depVar is the dependent variable, d is the dth derivative
-}
($^^) :: ConstrConcept -> Integer -> Unknown
($^^) _ unk' = unk'

{-
  Input Language represent Term which a coefficient multiple an unknown
  e.g. exactDbl 1 $* (opProcessVariable $^^ 2), 
  exactDbl 1 is the the coefficient, 
  (opProcessVariable $^^ 2) is the 2rd derivative of opProcessVariable
-}
($*) :: Expr -> Unknown -> Term
($*) = T

{-
  Input Language represent a collection of Terms
  e.g. [exactDbl 1 $* (opProcessVariable $^^ 2)]
       $+ (exactDbl 1 `addRe` sy qdDerivGain $* (opProcessVariable $^^ 1))
  [exactDbl 1 $* (opProcessVariable $^^ 2)] is a collection with a single Term, 
  (exactDbl 1 `addRe` sy qdDerivGain $* (opProcessVariable $^^ 1)) is the appended element
-}
($+) :: [Term] -> Term -> LHS
($+) xs x  = xs ++ [x]

data DifferentialModel = SystemOfLinearODEs {
  -- independent variable, usually time
  _indepVar :: UnitalChunk,
  -- Dependent variable, it is a vector
  _depVar :: ConstrConcept,
  -- coefficients matrix
  _coefficients :: [[Expr]],
  -- Unknowns column vector
  _unknowns :: [Unknown],
  -- Constant Column vector 
  _dmConstants :: [Expr],
  _dmconc :: ConceptChunk
}
makeLenses ''DifferentialModel


-- Information for solving an initial value problem
data InitialValueProblem = IVP{
  initTime :: Expr,
  finalTime :: Expr,
  initValues :: [Expr]
}

{-
  Acceptable format for ODE solvers
  X' = AX + B
  A  is coefficient matrix with identity matrix
  X  is unknown column vector after reduce the highest order
  B  is constant column vector with identity matrix
  X' is a column vector of first-order unknowns
-}
data ODESolverFormat = X'{
  coeffVects :: [[Expr]],
  unknownVect :: [Integer],
  constantVect :: [Expr]
}

-- | Finds the 'UID' of the 'ConceptChunk' used to make the 'DifferentialModel'.
instance HasUID        DifferentialModel where uid = dmconc . uid
-- | Equal if 'UID's are equal.
instance Eq            DifferentialModel where a == b = (a ^. uid) == (b ^. uid)
-- | Finds the term ('NP') of the 'ConceptChunk' used to make the 'DifferentialModel'.
instance NamedIdea     DifferentialModel where term = dmconc . term
-- | Finds the idea contained in the 'ConceptChunk' used to make the 'DifferentialModel'.
instance Idea          DifferentialModel where getA = getA . view dmconc
-- | Finds the definition contained in the 'ConceptChunk' used to make the 'DifferentialModel'.
instance Definition    DifferentialModel where defn = dmconc . defn
-- | Finds the domain of the 'ConceptChunk' used to make the 'DifferentialModel'.
instance ConceptDomain DifferentialModel where cdom = cdom . view dmconc
-- | Convert the 'DifferentialModel' into the model expression language.
instance Express       DifferentialModel where express = formStdODE

-- | Set the expression be a system of linear ODE to Ax = b
formStdODE :: DifferentialModel -> ModelExpr
formStdODE d
  | size == 1 = formASingleODE (head (d ^. coefficients)) unknownVec (d ^. dmConstants)
  | otherwise = equiv (coeffsMatix $. columnVec unknownVec : constantVec)
  where size = length (d ^. coefficients)
        coeffsMatix = express(matrix (d ^. coefficients))
        unknownVec = formAllUnknown (d ^. unknowns) (d ^. depVar) (d ^. indepVar)
        constantVec = [express (columnVec (d ^. dmConstants))]

-- | Set the single ODE to a flat equation form, rhs = lhs 
formASingleODE :: [Expr] -> [ModelExpr] -> [Expr] -> ModelExpr
formASingleODE coeffs unks consts = equiv (lhs : rhs)
  where lhs = foldl1 addRe (map (\x-> express (fst x) `mulRe` snd x) $ filterZeroCoeff coeffs unks)
        rhs = map express consts

-- | Remove zero coefficients for display purpose
filterZeroCoeff :: [Expr] -> [ModelExpr] -> [(Expr, ModelExpr)]
filterZeroCoeff es mes = filter (\x -> fst x /= exactDbl 0) $ zip es mes

-- | Form a n-vector of derivatives dependent variables
formAllUnknown :: [Unknown] -> ConstrConcept -> UnitalChunk -> [ModelExpr]
formAllUnknown unks dep ind = map (\x -> formAUnknown x dep ind) unks

-- | Form a derivative of a dependent variable
formAUnknown :: Unknown -> ConstrConcept-> UnitalChunk -> ModelExpr
formAUnknown unk'' dep = nthderiv (toInteger unk'') (sy (qw dep))

{-
  Create a 'DifferentialModel' by giving a independent variable, a dependent variable 
  a canonical matrix form, and conceptChuck.
  canonical matrix form: Ax = b
    A is a known m*n matrix that contains coefficients, 
    x is an n-vector that contain derivatives of dependent variables
    b is an m-vector that contain constants
  conceptChuck: 
    uid ('String'), term ('NP'), definition ('Sentence').
-}
makeASystemDE :: UnitalChunk -> ConstrConcept -> [[Expr]] -> [Unknown] -> [Expr]-> String -> NP -> Sentence -> DifferentialModel
makeASystemDE indepVar' depVar' coeffs unks const' id' term' defn'
 | length coeffs /= length const' =
  error "Length of coefficients matrix should equal to the length of the constant vector"
 | not $ isCoeffsMatchUnknowns coeffs unks =
  error "The length of each row vector in coefficients need to equal to the length of unknowns vector"
 | otherwise = SystemOfLinearODEs indepVar' depVar' coeffs unks const'(dccWDS id' term' defn')

-- | Create a single ODE with its left hand side and right hand side
makeASingleDE :: UnitalChunk -> ConstrConcept -> LHS -> Expr-> String -> NP -> Sentence -> DifferentialModel
makeASingleDE indepVar'' depVar'' lhs const'' id'' term'' defn''
 | length coeffs /= length [const''] =
  error "Length of coefficients matrix should equal to the length of the constant vector"
 | not $ isCoeffsMatchUnknowns coeffs unks =
  error "The length of each row vector in coefficients need to equal to the length of unknowns vector"
 | otherwise = SystemOfLinearODEs indepVar'' depVar'' coeffs unks [const''](dccWDS id'' term'' defn'')
  where unks = createAllUnknowns(findHighestOrder lhs ^. unk) depVar''
        coeffs = [createCoefficients lhs unks]

-- | Function to check whether dimension of coefficient is match with the unknown vector
isCoeffsMatchUnknowns :: [[Expr]] -> [Unknown] -> Bool
isCoeffsMatchUnknowns [] _ = error "Coefficients matrix can not be empty"
isCoeffsMatchUnknowns _ [] = error "Unknowns column vector can not be empty"
isCoeffsMatchUnknowns coeffs unks = foldr (\ x -> (&&) (length x == length unks)) True coeffs

-- | Find the highest order in left hand side
findHighestOrder :: LHS -> Term
findHighestOrder = foldr1 (\x y -> if x ^. unk >= y ^. unk then x else y)

-- | Create all possible unknowns based on the highest order.
-- | The order of the result list is from the highest degree to zero degree.
createAllUnknowns :: Unknown -> ConstrConcept -> [Unknown]
createAllUnknowns highestUnk depv
  | highestUnk  == 0  = [highestUnk]
  | otherwise = highestUnk : createAllUnknowns (highestUnk - 1) depv

-- | Create Coefficients base on all possible unknowns
-- | The order of the result list is from the highest degree to zero degree.
createCoefficients :: LHS -> [Unknown] -> [Expr]
createCoefficients [] _ = error "Left hand side is an empty list"
createCoefficients _ [] = []
createCoefficients lhs (x:xs) = genCoefficient (findCoefficient x lhs) : createCoefficients lhs xs

-- | Get the coefficient, if it is Nothing, return zero
genCoefficient :: Maybe Term -> Expr
genCoefficient Nothing = exactDbl 0
genCoefficient (Just x) = x ^. coeff

-- | Find the term that match with the unknown
findCoefficient :: Unknown -> LHS -> Maybe Term
findCoefficient u = find(\x -> x ^. unk == u)

-- | Delete the highest order
transUnknowns :: [Unknown] -> [Unknown]
transUnknowns = tail

-- | Reduce the target coefficient to one, delete it, apply negative
transCoefficients :: [Expr] -> [Expr]
transCoefficients es
  | head es == exactDbl 1 = mapNeg $ tail es
  | otherwise = mapNeg $ tail $ map ($/ head es) es
    where mapNeg = map (\x -> if x == exactDbl 0 then exactDbl 0 else neg x)

-- | Add Identity Matrix to Coefficients
-- | len is the length of the identity row,
-- | index is the location of identity value (start with 0)
addIdentityCoeffs :: [[Expr]] -> Int -> Int -> [[Expr]]
addIdentityCoeffs es len index
  | len == index + 1 = es
  | otherwise = addIdentityCoeffs (constIdentityRowVect len index : es) len (index + 1)

-- | Construct an identity row vector.
constIdentityRowVect :: Int -> Int -> [Expr]
constIdentityRowVect len index = addIdentityValue index $ replicate len $ exactDbl 0

-- | Recreate the identity row vector with identity value 
addIdentityValue :: Int -> [Expr] -> [Expr]
addIdentityValue n es = fst splits ++ [exactDbl 1] ++ tail (snd splits)
  where splits = splitAt n es

-- | Add Identity Matrix to Constants
-- | len is the size of new constant vector
addIdentityConsts :: [Expr] -> Int -> [Expr]
addIdentityConsts expr len = replicate (len - 1) (exactDbl 0) ++ expr

-- | divide the leading coefficient of the highest order in constant
divideCosntants :: Expr -> Expr -> Expr
divideCosntants a b
  | b == exactDbl 0 = error "Divisor can't be zero"
  | b == exactDbl 1 = a
  | otherwise       = a $/ b

-- | Construct an ODESolverFormat for solving the ODE.
makeAODESolverFormat :: DifferentialModel -> ODESolverFormat
makeAODESolverFormat dm = X' transEs transUnks transConsts
  where transUnks = transUnknowns $ dm ^. unknowns
        transEs = addIdentityCoeffs [transCoefficients $ head (dm ^. coefficients)] (length transUnks) 0
        transConsts = addIdentityConsts [head (dm ^. dmConstants) `divideCosntants` head (head (dm ^. coefficients))] (length transUnks)

{-
  Form well-formatted ODE equations which the ODE solvers can solve. For example:

  the original fourth-order ODE: 
  y'''' + 3y′′ − sin(t)y′ + 8y = t2

  can be re-written to

  A                 *  X      + B     = X'

  0  0      1   0      x₄       0       equation 1
  0  1      0   0      x₃       0       equation 2
  1  0      0   0      x₂       0       equation 3 
  0 -3  sin(t) -8      x₁       t^2     equation 4 

  X = x₄,x₃,x₂,x₁ and x₁ = y, x₂ = y', x₃ = y'', x₄ = y'''

  A: [[Expr]], X: [Unknown], B: [Expr]
  return [equation 1, equation 2, equation 3, equation 4]
-}
formEquations :: [[Expr]] -> [Unknown] -> [Expr] -> ConstrConcept-> [Expr]
formEquations [] _ _ _ = []
formEquations _ [] _ _ = []
formEquations _ _ [] _ = []
formEquations (ex:exs) unks (y:ys) depVa =
  (if y == exactDbl 0 then finalExpr else finalExpr `addRe` y) : formEquations exs unks ys depVa
  where indexUnks = map (idx (sy depVa) . int) unks -- create X
        filteredExprs = filter (\x -> fst x /= exactDbl 0) (zip ex indexUnks) -- remove zero coefficients
        termExprs = map (uncurry mulRe) filteredExprs -- multiple coefficient with depend variables
        finalExpr = foldl1 addRe termExprs -- add terms together

{-
  Construct an InitialValueProblem.
  the first Expr: start time
  the second Expr: final time
  [Expr] : initial values
-}
makeAIVP :: Expr -> Expr -> [Expr] -> InitialValueProblem
makeAIVP = IVP

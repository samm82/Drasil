module Drasil.ChemCode.Quantities where

import Language.Drasil -- hiding (matrix)
import Language.Drasil.ShortHands

import Data.Drasil.Concepts.Chemistry (chemical, compound, element, reaction)

inputs :: [QuantityDict]
inputs = [inputChemEqn]

quants :: [QuantityDict]
quants = inputs ++ [aMat, bVec, cVec, eMat, xVec, unaryVec, zeroVec, genE,
  genC, genR, tupC, count, elems, elemT, compT, reacT]

inputChemEqn, aMat, bVec, cVec, eMat, xVec, unaryVec, zeroVec, genE, genC,
  genR, tupC, count, elems, elemT, compT, reacT :: QuantityDict

inputChemEqn = vcSt "inputChemEqn"
  (nounPhraseSP "representation of a chemical equation")
  (autoStage $ sub lR $ label "in") String
  -- FIXME: should this be a string?

aMat = vc "aMat" (nounPhraseSP "generic matrix")                               (vec cA) (Vect Real)
bVec = vc "bVec" (nounPhraseSP "generic vector")                               (vec lB) (Vect Real)
cVec = vc "cVec" (nounPhraseSP "generic vector")                               (vec lC) (Vect Real)
eMat = vc "eMat" (nounPhraseSP "matrix representation of a chemical equation") (vec cE) (Vect Real)
xVec = vc "xVec" (nounPhraseSP "generic vector")                               (vec lX) (Vect Integer)

unaryVec = vc "unaryVec" (nounPhraseSP "unary vector") (vec $ variable "1") (Vect Integer)
zeroVec  = vc "zeroVec"  (nounPhraseSP "zero vector")  (vec $ variable "0") (Vect Integer)

genE = vc "genE" (nounPhraseSent $ S "generic" +:+ phrase element) lE Element
genC = vc "genC" (nounPhraseSent $ S "generic" +:+ phrase compound) lC Compound
genR = vc "genR" (nounPhraseSent $ S "generic" +:+ phrase reaction) lR Reaction

compoundTuple :: Space
compoundTuple = Tuple [("elem", Element), ("count", Real)]

tupC = vc "tupC" (nounPhraseSent $ S "generic tuple of a" +:+ phrase compound)
  (sub lT cC) compoundTuple

count = vc "count"
  (nounPhraseSent $ foldlSent_ [S "count of an", phrase element, S "in a", phrase compound])
  (label "count") (mkFunction [Element, Compound] Real)

elems = vc "elems"
  (nounPhraseSent $ foldlSent_ [S "set of", plural element, S "in a",
    phrase chemical, phrase reaction])
  (label "elems") (mkFunction [Reaction] $ Sequence Element)

elemT = vcSt "elemT" (nounPhraseSent $ phrase element +:+ S "data type")
  (autoStage cE)
  (Enum ["H", "He", "Li", "Be", "B", "C", "N", "O", "F", "Ne", "Na", "Mg",
         "Al", "Si", "P", "S", "Cl", "Ar", "K", "Ca", "Sc", "Ti", "V", "Cr",
         "Mn", "Fe", "Co", "Ni", "Cu", "Zn", "Ga", "Ge", "As", "Se", "Br",
         "Kr", "Rb", "Sr", "Y", "Zr", "Nb", "Mo", "Tc", "Ru", "Rh", "Pd", "Ag",
         "Cd", "In", "Sn", "Sb", "Te", "I", "Xe", "Cs", "Ba", "La", "Ce", "Pr",
         "Nd", "Pm", "Sm", "Eu", "Gd", "Tb", "Dy", "Ho", "Er", "Tm", "Yb",
         "Lu", "Hf", "Ta", "W", "Re", "Os", "Ir", "Pt", "Au", "Hg", "Tl", "Pb",
         "Bi", "Po", "At", "Rn", "Fr", "Ra", "Ac", "Th", "Pa", "U", "Np", "Pu",
         "Am", "Cm", "Bk", "Cf", "Es", "Fm", "Md", "No", "Lr", "Rf", "Db",
         "Sg", "Bh", "Hs", "Mt", "Ds", "Rg", "Cn", "Nh", "Fl", "Mc", "Lv",
         "Ts", "Og"])
  -- FIXME: move to drasil-data

compT = vcSt "compT" (nounPhraseSent $ phrase compound +:+ S "data type")
  (autoStage cC) (Sequence compoundTuple)

reacT = vcSt "reacT" (nounPhraseSent $ phrase reaction +:+ S "data type")
  (autoStage cR)
  (Tuple [("prod", reacSide), ("reac", reacSide)])
  where 
    reacSide = Sequence $ Tuple [("comp", Compound), ("coeff", Integer)]

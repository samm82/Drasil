module Drasil.ChemCode.Quantities where

import Language.Drasil -- hiding (matrix)
import Language.Drasil.ShortHands

import Data.Drasil.Concepts.Chemistry (chemical, compound, element, reaction)

inputs :: [QuantityDict]
inputs = [inputChemEqn]

quants :: [QuantityDict]
quants = inputs ++ [aMat, bVec, cVec, qMat, qEnt, xVec, unaryVec, zeroVec,
  genE, genC, genI, genJ, genR, genX, genY, tupC, count, elems, elemT, compT,
  reacT]

constants :: [ConstQDef]
constants = [maintainFrac]

inputChemEqn, aMat, bVec, cVec, qMat, qEnt, xVec, unaryVec, zeroVec, genE,
  genC, genI, genJ, genR, genX, genY, tupC, count, elems, elemT, compT,
  reacT :: QuantityDict

inputChemEqn = vcSt "inputChemEqn"
  (nounPhraseSP "representation of a chemical equation")
  (autoStage $ sub lR $ label "in") Reaction

aMat = vc "aMat" (nounPhraseSP "generic matrix")                               (vec cA) (Vect Real)
bVec = vc "bVec" (nounPhraseSP "generic vector")                               (vec lB) (Vect Real)
cVec = vc "cVec" (nounPhraseSP "generic vector")                               (vec lC) (Vect Real)
qMat = vc "qMat" (nounPhraseSP "matrix representation of a chemical equation") (vec cQ) (Vect Real)
qEnt = vc "qEnt" (nounPhraseSent $ S "element of" +:+ ch qMat)                 (sub lQ $ label "ij") Real -- FIXME: symbol hack
xVec = vc "xVec" (nounPhraseSP "generic vector")                               (vec lX) (Vect Integer)

unaryVec = vc "unaryVec" (nounPhraseSP "unary vector") (vec $ variable "1") (Vect Integer)
zeroVec  = vc "zeroVec"  (nounPhraseSP "zero vector")  (vec $ variable "0") (Vect Integer)

genE = vc "genE" (nounPhraseSent $ S "generic" +:+ phrase element)  lE Element
genC = vc "genC" (nounPhraseSent $ S "generic" +:+ phrase compound) lC Compound
genI = vc "genI" (nounPhraseSent $ S "generic integer")             lI Integer
genJ = vc "genJ" (nounPhraseSent $ S "generic integer")             lJ Integer
genR = vc "genR" (nounPhraseSent $ S "generic" +:+ phrase reaction) lR Reaction
genX = vc "genX" (nounPhraseSent $ S "generic real number")         lX Real
genY = vc "genY" (nounPhraseSent $ S "generic integer")             lY Integer

compoundRecord :: Space
compoundRecord = Record [("elem", Element), ("count", Real)]

tupC = vc "tupC" (nounPhraseSent $ S "generic record of a" +:+ phrase compound)
  (sup lC lR) compoundRecord

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
  (autoStage cC) (Sequence compoundRecord)

reacT = vcSt "reacT" (nounPhraseSent $ phrase reaction +:+ S "data type")
  (autoStage cR)
  (Record [("prod", reacSide), ("reac", reacSide)])
  where 
    reacSide = Sequence $ Record [("comp", Compound), ("coeff", Integer)]

maintainFrac :: ConstQDef
maintainFrac = mkQuantDef (vc "maintainFrac"
  (nounPhraseSP "maintainability fraction") (label "MAINTAIN_FRAC") Real)
  (perc 10 2)
  -- FIXME: move concept to drasil-data?

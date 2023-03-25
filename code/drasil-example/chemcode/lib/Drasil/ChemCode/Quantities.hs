module Drasil.ChemCode.Quantities where

import Language.Drasil hiding (matrix)
import Language.Drasil.ShortHands

import Data.Drasil.Concepts.Chemistry (element)

inputs :: [QuantityDict]
inputs = [r]

quants :: [QuantityDict]
quants = inputs ++ [cVec, xVec, elemT]

r, cVec, xVec, elemT :: QuantityDict

r = vcSt "r" (nounPhraseSP "representation of a chemical equation")
  (autoStage lR) String -- FIXME: should this be a string?

cVec = vc "cVec" (nounPhraseSP "generic vector") (vec lC) (Vect Real)

xVec = vc "xVec" (nounPhraseSP "generic vector") (vec lX) (Vect Real)

elemT = vcSt "elemT" (nounPhraseSent $ phrase element +:+ S "data type")
  (autoStage cE)
  (DiscreteS ["H", "He", "Li", "Be", "B", "C", "N", "O", "F", "Ne", "Na", "Mg",
              "Al", "Si", "P", "S", "Cl", "Ar", "K", "Ca", "Sc", "Ti", "V",
              "Cr", "Mn", "Fe", "Co", "Ni", "Cu", "Zn", "Ga", "Ge", "As", "Se",
              "Br", "Kr", "Rb", "Sr", "Y", "Zr", "Nb", "Mo", "Tc", "Ru", "Rh",
              "Pd", "Ag", "Cd", "In", "Sn", "Sb", "Te", "I", "Xe", "Cs", "Ba",
              "La", "Ce", "Pr", "Nd", "Pm", "Sm", "Eu", "Gd", "Tb", "Dy", "Ho",
              "Er", "Tm", "Yb", "Lu", "Hf", "Ta", "W", "Re", "Os", "Ir", "Pt",
              "Au", "Hg", "Tl", "Pb", "Bi", "Po", "At", "Rn", "Fr", "Ra", "Ac",
              "Th", "Pa", "U", "Np", "Pu", "Am", "Cm", "Bk", "Cf", "Es", "Fm",
              "Md", "No", "Lr", "Rf", "Db", "Sg", "Bh", "Hs", "Mt", "Ds", "Rg",
              "Cn", "Nh", "Fl", "Mc", "Lv", "Ts", "Og"])
  -- FIXME: move to drasil-data

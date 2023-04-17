module Drasil.ChemCode.IMods where

import qualified Data.List.NonEmpty as NE

import Language.Drasil
import Theory.Drasil

import Drasil.ChemCode.Quantities (aMat, bVec, cVec, qMat, qEnt, inputChemEqn,
  unaryVec, xVec, zeroVec, elems, count, genC, genE, genI, genJ)
import Drasil.ChemCode.TMods (intLinProg)

ims :: [InstanceModel]
ims = [matRepresentation, chemEqIntLinProg]

convertMatEq :: SimpleQDef
convertMatEq = mkQuantDef qMat (forall [genI, genJ]
  $ ((int 0 $<= sy genI $< abs_ (apply elems [sy inputChemEqn])) $&&
      (int 0 $<= sy genJ $< (abs_ (access inputChemEqn "reac") `addRe`
        abs_ (access inputChemEqn "prod")))) $=>
        sy qEnt $= completeCase [
          (apply count $ map sy [genE, genC],
            sy genJ $< abs_ (access inputChemEqn "reac")),
          (neg $ apply count $ map sy [genE, genC],
            not_ (sy genJ $< abs_ (access inputChemEqn "reac")))
        ])

matRepresentation :: InstanceModel
matRepresentation = imNoRefs
  (equationalModel' convertMatEq)
  [qwUC inputChemEqn] -- FIXME: why should I need to convert the Unitals to QuantityDicts?
  qMat
  []
  Nothing -- Derivation: (Just $ mkDerivName [S "parts of deriv"])
  "matRepresentation"
  [] -- TODO: Notes

chemEqIntLinProg :: InstanceModel
chemEqIntLinProg = imNoRefs
  (ilpModel "chemEqIntLinProg" (cn' "integer linear program for a chemical equation")
    $ minILP xVec unaryVec
      $ (sy qMat `mulRe` sy xVec $= sy zeroVec) NE.:| [sy xVec $> sy zeroVec, isIn (sy xVec) (Vect Integer)])
  [qwUC qMat] -- FIXME: why should I need to convert the Unitals to QuantityDicts?
  xVec
  []
  Nothing -- Derivation: (Just $ mkDerivName [S "parts of deriv"])
  "chemEqIntLinProg"
  [foldlSent [S "This is a specific instance of the ILP from", refS intLinProg,
    S "with" +:+. foldlList Comma List [
      E (($=) (sy aMat) (sy qMat)), E (($=) (sy bVec) (sy zeroVec)), E (($=) (sy cVec) (sy unaryVec))
    ], S "The goal and constraints are also modified"]] -- TODO: Notes

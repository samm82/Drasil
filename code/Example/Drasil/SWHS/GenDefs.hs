module Drasil.SWHS.GenDefs where

import Prelude hiding (sin, cos, tan)

import Language.Drasil

import Data.Drasil.SentenceStructures (foldlSent, acroA)
import Data.Drasil.Quantities.PhysicalProperties as QPP (vol, mass)
import Data.Drasil.Quantities.Thermodynamics as QT (ht_flux, heat_cap_spec,
  temp)
import Data.Drasil.Quantities.Physics as QP (time)
import Drasil.SWHS.Unitals (vol_ht_gen, temp_diff, temp_env, pcm_SA,
  out_SA, in_SA, ht_flux_in, ht_flux_out, htTransCoeff, thFluxVect)
import Data.Drasil.SentenceStructures (isThe, sAnd)
import Data.Drasil.Utils (getS, unwrap)
import Data.Drasil.Concepts.Math (equation, rOfChng, rate)
import Data.Drasil.Concepts.Thermodynamics (law_conv_cooling)

---------------------------
--  General Definitions  --
---------------------------

swhsGenDefs :: [RelationConcept]
swhsGenDefs = [nwtnCooling, rocTempSimp]

--
nwtnCooling :: RelationConcept
nwtnCooling = makeRC "nwtnCooling" (nounPhraseSP "Newton's law of cooling") 
  nwtnCooling_desc nwtnCooling_rel

nwtnCooling_rel :: Relation
nwtnCooling_rel = FCall (C thFluxVect) [C QP.time] := C htTransCoeff :*
  FCall (C temp_diff) [C QP.time]

nwtnCooling_desc :: Sentence
nwtnCooling_desc = foldlSent [at_start law_conv_cooling +:+.
  S "describes convective cooling from a surface" +:
  S "The law is stated as", S "the", phrase rate,
  S "of heat loss from a body is proportional to the",
  S "difference in", plural temp, S "between the body" +:+.
  S "and its surroundings", E (FCall (C thFluxVect) [C QP.time]) `isThe`
  S "thermal flux" +:+. sParen (Sy $ unit_symb thFluxVect),
  getS htTransCoeff `isThe` S "heat transfer coefficient" `sC`
  S "assumed independant of", getS QT.temp, sParen (acroA 2) +:+.
  sParen (Sy $ unit_symb htTransCoeff),
  E (FCall (C temp_diff) [C QP.time] := FCall (C temp) [C QP.time] :-
  FCall (C temp_env) [C QP.time]) `isThe` S "time-dependant thermal gradient",
  S "between the environment and the object",
  sParen (Sy $ unit_symb temp_diff)]

--
rocTempSimp :: RelationConcept
rocTempSimp = makeRC "rocTempSimp" (nounPhraseSP $ "Simplified rate" ++
  "of change of temperature") rocTempSimp_desc rocTempSimp_rel

rocTempSimp_rel :: Relation
rocTempSimp_rel = (C QPP.mass) :* (C QT.heat_cap_spec) :*
  Deriv Total (C QT.temp) (C QP.time) := C ht_flux_in :* C in_SA :-
  C ht_flux_out :* C out_SA :+ C vol_ht_gen :* C QPP.vol

rocTempSimp_desc :: Sentence
rocTempSimp_desc = foldlSent [S "The basic", phrase equation,
  S "governing the", phrase rOfChng, S "of", phrase temp `sC`
  S "for a given", phrase QPP.vol, getS QPP.vol `sC` S "with" +:+.
  phrase QP.time, getS QPP.mass `isThe` phrase QPP.mass +:+.
  sParen (Sy $ unit_symb QPP.mass), getS QT.heat_cap_spec `isThe` 
  phrase QT.heat_cap_spec +:+. sParen (Sy $ unit_symb QT.heat_cap_spec),
  getS temp `isThe` phrase temp, sParen (Sy $ unit_symb temp) `sAnd`
  getS QP.time `isThe` phrase QP.time +:+. sParen (Sy $ unit_symb QP.time),
  getS ht_flux_in `sAnd` getS ht_flux_out, S "are the in and out heat",
  S "transfer rates, respectively" +:+. sParen (Sy $ unit_symb QT.ht_flux),
  getS in_SA `sAnd` getS out_SA, S "are the surface areas over which the",
  S "heat is being transferred in and out, respectively" +:+.
  sParen (unwrap $ getUnit pcm_SA), getS vol_ht_gen `isThe`
  S "volumetric heat generated" +:+. sParen (Sy $ unit_symb vol_ht_gen),
  getS QPP.vol `isThe` phrase QPP.vol, sParen (Sy $ unit_symb QPP.vol)]
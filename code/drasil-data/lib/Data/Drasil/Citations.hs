-- | Defines citations used in Drasil.
module Data.Drasil.Citations where

import Language.Drasil --(S,(:+:),(+:+),sC,phrase,F,Accent(..),Citation(..),CiteField(..))
import Data.Drasil.People (aBergholm, atHutton, brooks, dParnas, dScime,
  dSzymczak, dmWiess, dong, gpMoss, haFavre, harpCollins, iupac, jBalaci,
  jCarette, jRalyte, jRobertson, lLai, lLund, mCampidelli, mNiazi, nKoothoor,
  nKraiem, ngConnelly, pWexler, pcClements, pjAgerfalk, rKhedri, rcHibbeler,
  rmHartshorn, rodPierce, sRobertson, samCrawford, spencerSmith, tDahmus, tWu,
  whPowell, wikiAuthors)

-- * Citations

-- ** Papers

campidelli, koothoor2013, parnas1972, parnasClements1984,
  parnasClements1986, rbrtsn2012, smithLai2005, lineSource, pointSource,
  hibbeler2004, lund2023, hydrateSource, polymerSource, nonIntCoeffSource,
  inorganicIUPAC, organicIUPAC, elemListWiki, smithChemSpec,
  smithEtAl2007, smithKoothoor2016 :: Citation

campidelli = cBooklet
  "Glass-BR Software for the design and risk assessment of glass facades subjected to blast loading"
  [author [Data.Drasil.People.mCampidelli]] "campidelli"

koothoor2013 = 
  cMThesis [Data.Drasil.People.nKoothoor] 
  "A Document Driven Approach to Certifying Scientific Computing Software"
  "McMaster University" 2013 [address "Hamilton, ON, Canada"]
  "koothoor2013"

parnas1972 = cArticle [Data.Drasil.People.dParnas]
  "On the Criteria To Be Used in Decomposing Systems into Modules"
  "Communications of the ACM" 1972
  [pages [1053..1058]] "dParnas1972"
  
parnasClements1984 = cInProceedings
  [Data.Drasil.People.dParnas, Data.Drasil.People.pcClements, Data.Drasil.People.dmWiess]
  "The Modular Structure of Complex Systems"
  "ICSE '84: Proceedings of the 7th international conference on Software engineering"
  1984 [pages [408..417]] "parnasClements1984"

parnasClements1986 = cArticle [Data.Drasil.People.dParnas, Data.Drasil.People.pcClements] 
  "A rational design process: How and why to fake it"
  "IEEE Transactions on Software Engineering" 1986
  [month Feb, volume 12, number 2, pages [251..257], address "Washington, USA"]
  "parnasClements1986"

rbrtsn2012 = cMisc [author [Data.Drasil.People.jRobertson, Data.Drasil.People.sRobertson], title
  "Volere requirements specification template edition 16",
  howPublishedU "https://pdfs.semanticscholar.org/cf57/27a59801086cbd3d14e587e09880561dbe22.pdf"
  , year 2012]
  "rbrtsn2012"

smithLai2005 = cInProceedings [Data.Drasil.People.spencerSmith, Data.Drasil.People.lLai]
  "A new requirements template for scientific computing"
  ("Proceedings of the First International Workshop on " ++
  "Situational Requirements Engineering Processes - Methods, " ++
  "Techniques and Tools to Support Situation-Specific Requirements " ++
  "Engineering Processes, SREP'05") 2005
  [ editor [Data.Drasil.People.pjAgerfalk, Data.Drasil.People.nKraiem, Data.Drasil.People.jRalyte], address "Paris, France"
  , pages [107..121], 
  note "In conjunction with 13th IEEE International Requirements Engineering Conference,"] 
  "smithLai2005"

smithEtAl2007 = cArticle [Data.Drasil.People.spencerSmith, Data.Drasil.People.lLai, Data.Drasil.People.rKhedri]
  ("Requirements Analysis for Engineering Computation: A Systematic Approach for" ++ 
    " Improving Software Reliability")
  "Reliable Computing, Special Issue on Reliable Engineering Computation" 2007
  [month Feb, volume 13, number 1, pages [83..107], howPublishedU "https://doi.org/10.1007/s11155-006-9020-7"]
  "smithEtAl2007"

smithKoothoor2016 = cArticle [Data.Drasil.People.spencerSmith, Data.Drasil.People.nKoothoor]
  ("A Document-Driven Method for Certifying Scientific Computing Software for Use" ++ 
    " in Nuclear Safety Analysis")
  " Nuclear Engineering and Technology" 2016
  [month Apr, volume 48, number 2, pages[404..418], 
    howPublishedU "http://www.sciencedirect.com/science/article/pii/S1738573315002582"]
  "smithKoothoor2016"

lineSource = cMisc
  [author [mononym "The Editors of Encyclopaedia Britannica"], title "Line",
  howPublishedU "https://www.britannica.com/science/line-mathematics",
  month Jun, year 2019]
  "lineSource"

pointSource = cMisc
  [author [Data.Drasil.People.rodPierce], title "Point",
  howPublishedU "https://www.mathsisfun.com/geometry/point.html",
  month May, year 2017]
  "pointSource"

hibbeler2004 = cBookA [Data.Drasil.People.rcHibbeler]
  "Engineering Mechanics: Dynamics"
  "Pearson Prentice Hall"
  2004 [] "hibbeler2004"

lund2023 = cBookA [Data.Drasil.People.lLund] "Introduction to Chemistry" "LibreTexts"
  2023 [address "Cambridge and Coon Rapids, MN, USA", month Jan,
  howPublishedU "https://chem.libretexts.org/Courses/Anoka-Ramsey_Community_College/Introduction_to_Chemistry"]
  "lund2023"

hydrateSource = cMisc [author [Data.Drasil.People.harpCollins], title "hydrate",
  publisher "HarperCollins", address "New York, NY, USA", journal "Collins"]
  "hydrateSource"

-- FIXME: should use cBookE, but gets a "No author found" error when building
polymerSource = cBookA [Data.Drasil.People.pWexler] "Encyclopedia of Toxicology" "Academic Press"
  2014 [address "Amsterdam, NL", edition 3] "polymerSource"

nonIntCoeffSource = cTechReport [Data.Drasil.People.aBergholm] "Oxidation of Pyrite"
  "U. S. Department of the Interior" 1995 [address "Boulder, CO, USA"]
  "nonIntCoeffSource"
  -- FIXME: this source also has editors; are they necessary to add?

inorganicIUPAC = cBookA [Data.Drasil.People.iupac, Data.Drasil.People.ngConnelly, Data.Drasil.People.tDahmus, Data.Drasil.People.rmHartshorn, Data.Drasil.People.atHutton]
  "Nomenclature of Inorganic Chemistry: IUPAC Recommendations 2005"
  "The Royal Society of Chemistry" 2005 [address "Cambridge, UK",
  howPublishedU "https://iupac.org/wp-content/uploads/2016/07/Red_Book_2005.pdf"]
  "inorganicIUPAC"
  
organicIUPAC = cBookA [Data.Drasil.People.iupac, Data.Drasil.People.gpMoss, Data.Drasil.People.haFavre, Data.Drasil.People.whPowell]
  "Nomenclature of Organic Chemistry: IUPAC Recommendations and Preferred Names 2013"
  "The Royal Society of Chemistry" 2013 [address "Cambridge, UK",
  howPublishedU "https://iupac.qmul.ac.uk/BlueBook/PDF/"]
  "organicIUPAC"

-- ** about Drasil

drasilSource, maclachlan2021, chen2022 :: Citation

drasilSource = cMisc [author [Data.Drasil.People.jCarette, Data.Drasil.People.spencerSmith, Data.Drasil.People.jBalaci, Data.Drasil.People.tWu,
    Data.Drasil.People.samCrawford, Data.Drasil.People.dong, Data.Drasil.People.dSzymczak, Data.Drasil.People.brooks, Data.Drasil.People.dScime, Data.Drasil.People.mNiazi], title "Drasil",
  howPublishedU "https://jacquescarette.github.io/Drasil/",
  month Feb, year 2021]
  "drasilSource"

maclachlan2021 = cPhDThesis [Data.Drasil.People.brooks]
  "A Design Language for Scientific Computing Software in Drasil"
  "McMaster University" 2021 [month Nov, address "Hamilton, ON, Canada",
  editor [Data.Drasil.People.jCarette, Data.Drasil.People.spencerSmith],
  howPublishedU "https://macsphere.mcmaster.ca/bitstream/11375/25542/2/maclachlan_brooks_2020july_masc.pdf"]
  "maclachlan2021"

chen2022 = cMThesis [Data.Drasil.People.dong] "Solving Higher-Order ODEs in Drasil"
  "McMaster University" 2022 [month Sep, address "Hamilton, ON, Canada",
  editor [Data.Drasil.People.jCarette, Data.Drasil.People.spencerSmith],
  howPublishedU "https://github.com/JacquesCarette/Drasil/blob/master/People/Dong/Thesis_Main.pdf"]
  "chen2022"

-- ** Wikipedia

dampingSource, accelerationWiki, velocityWiki, cartesianWiki, laplaceWiki,
  pidWiki, ilpWiki :: Citation

dampingSource = cMisc
  [author [Data.Drasil.People.wikiAuthors], title "Damping",
  howPublishedU "https://en.wikipedia.org/wiki/Damping_ratio",
  month Jul, year 2019]
  "dampingSource"

accelerationWiki = cMisc [author [Data.Drasil.People.wikiAuthors],
  title "Acceleration", howPublishedU "https://en.wikipedia.org/wiki/Acceleration",
  month Jun, year 2019]
  "accelerationWiki"

velocityWiki = cMisc [author [Data.Drasil.People.wikiAuthors],
  title "Velocity", howPublishedU "https://en.wikipedia.org/wiki/Velocity",
  month Jun, year 2019]
  "velocityWiki"

cartesianWiki = cMisc
  [author [Data.Drasil.People.wikiAuthors], title "Cartesian coordinate system",
  howPublishedU "https://en.wikipedia.org/wiki/Cartesian_coordinate_system",
  month Jun, year 2019]
  "cartesianWiki"

laplaceWiki
  = cMisc
      [author [Data.Drasil.People.wikiAuthors], title "Laplace transform",
       howPublishedU "https://en.wikipedia.org/wiki/Laplace_transform",
       month Nov, year 2020]
      "laplaceWiki"

pidWiki
  = cMisc
      [author [Data.Drasil.People.wikiAuthors], title "PID controller",
       howPublishedU "https://en.wikipedia.org/wiki/PID_controller", month Oct,
       year 2020]
      "pidWiki"

ilpWiki
  = cMisc
      [author [Data.Drasil.People.wikiAuthors], title "Integer programming",
       howPublishedU "https://en.wikipedia.org/wiki/Integer_programming", month Mar,
       year 2023]
      "ilpWiki"

elemListWiki
  = cMisc
      [author [Data.Drasil.People.wikiAuthors], title "List of chemical elements",
       howPublishedU "https://en.wikipedia.org/wiki/List_of_chemical_elements",
       month Jan, year 2023]
      "elemListWiki"

-- * Misc

smithChemSpec
  = cMisc
      [author [Data.Drasil.People.spencerSmith], title "Assignment 2",
       howPublishedU "https://gitlab.cas.mcmaster.ca/smiths/se2aa4_cs2me3/-/blob/master/Assignments/PreviousYears/2020/A2-ChemReacts/A2.pdf",
       month Feb, year 2020]
      "smithChemSpec"


-- * Common Cite Fields

jnlCGJ :: String
jnlCGJ = "Canadian Geotechnical Journal"

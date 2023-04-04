-- | Defines citations used in Drasil.
module Data.Drasil.Citations where

import Language.Drasil --(S,(:+:),(+:+),sC,phrase,F,Accent(..),Citation(..),CiteField(..))
import Data.Drasil.People

-- * Citations

-- ** Papers

campidelli, koothoor2013, parnas1972, parnasClements1984,
  parnasClements1986, smithLai2005, lineSource, pointSource,
  hibbeler2004, lund2023, inorganicIUPAC, organicIUPAC,
  elemListWiki, smithChemSpec :: Citation

campidelli = cBooklet
  "Glass-BR Software for the design and risk assessment of glass facades subjected to blast loading"
  [author [mCampidelli]] "campidelli"

koothoor2013 = 
  cMThesis [nKoothoor] 
  "A document drive approach to certifying scientific computing software"
  "McMaster University" 2013 [address "Hamilton, ON, Canada"]
  "koothoor2013"

parnas1972 = cArticle [dParnas]
  "On the Criteria To Be Used in Decomposing Systems into Modules"
  "Communications of the ACM" 1972
  [pages [1053..1058]] "dParnas1972"
  
parnasClements1984 = cInProceedings
  [dParnas, pcClements, dmWiess]
  "The Modular Structure of Complex Systems"
  "ICSE '84: Proceedings of the 7th international conference on Software engineering"
  1984 [pages [408..417]] "parnasClements1984"

parnasClements1986 = cArticle [dParnas, pcClements] 
  "A rational design process: How and why to fake it"
  "IEEE Transactions on Software Engineering" 1986
  [month Feb, volume 12, number 2, pages [251..257], address "Washington, USA"]
  "parnasClements1986"

smithLai2005 = cInProceedings [spencerSmith, lLai]
  "A new requirements template for scientific computing"
  ("Proceedings of the First International Workshop on " ++
  "Situational Requirements Engineering Processes - Methods, " ++
  "Techniques and Tools to Support Situation-Specific Requirements " ++
  "Engineering Processes, SREP'05") 2005
  [ editor [pjAgerfalk, nKraiem, jRalyte], address "Paris, France"
  , pages [107..121], 
  note "In conjunction with 13th IEEE International Requirements Engineering Conference,"] 
  "smithLai2005"

lineSource = cMisc
  [author [mononym "The Editors of Encyclopaedia Britannica"], title "Line",
  howPublishedU "https://www.britannica.com/science/line-mathematics",
  month Jun, year 2019]
  "lineSource"

pointSource = cMisc
  [author [rodPierce], title "Point",
  howPublishedU "https://www.mathsisfun.com/geometry/point.html",
  month May, year 2017]
  "pointSource"

hibbeler2004 = cBookA [rcHibbeler]
  "Engineering Mechanics: Dynamics"
  "Pearson Prentice Hall"
  2004 [] "hibbeler2004"

lund2023 = cBookA [lLund] "Introduction to Chemistry" "LibreTexts"
  2023 [address "Cambridge and Coon Rapids, MN, USA", month Jan,
  howPublishedU "https://chem.libretexts.org/Courses/Anoka-Ramsey_Community_College/Introduction_to_Chemistry"]
  "lund2023"

inorganicIUPAC = cBookA [iupac, ngConnelly, tDahmus, rmHartshorn, atHutton]
  "Nomenclature of Inorganic Chemistry: IUPAC Recommendations 2005"
  "The Royal Society of Chemistry" 2005 [address "Cambridge, UK",
  howPublishedU "https://iupac.org/wp-content/uploads/2016/07/Red_Book_2005.pdf"]
  "inorganicIUPAC"

organicIUPAC = cBookA [iupac, gpMoss, haFavre, whPowell]
  "Nomenclature of Organic Chemistry: IUPAC Recommendations and Preferred Names 2013"
  "The Royal Society of Chemistry" 2013 [address "Cambridge, UK",
  howPublishedU "https://iupac.qmul.ac.uk/BlueBook/PDF/"]
  "organicIUPAC"

-- ** about Drasil

drasilSource, maclachlan2021, chen2022 :: Citation

drasilSource = cMisc [author [jCarette, spencerSmith, jBalaci, tWu,
    samCrawford, dong, dSzymczak, brooks, dScime, mNiazi], title "Drasil",
  howPublishedU "https://jacquescarette.github.io/Drasil/",
  month Feb, year 2021]
  "drasilSource"

maclachlan2021 = cPhDThesis [brooks]
  "A Design Language for Scientific Computing Software in Drasil"
  "McMaster University" 2021 [month Nov, address "Hamilton, ON, Canada",
  editor [jCarette, spencerSmith],
  howPublishedU "https://macsphere.mcmaster.ca/bitstream/11375/25542/2/maclachlan_brooks_2020july_masc.pdf"]
  "maclachlan2021"

chen2022 = cMThesis [dong] "Solving Higher-Order ODEs in Drasil"
  "McMaster University" 2022 [month Sep, address "Hamilton, ON, Canada",
  editor [jCarette, spencerSmith],
  howPublishedU "https://github.com/JacquesCarette/Drasil/blob/master/People/Dong/Thesis_Main.pdf"]
  "chen2022"

-- ** Wikipedia

dampingSource, accelerationWiki, velocityWiki, cartesianWiki, laplaceWiki,
  pidWiki, ilpWiki :: Citation

dampingSource = cMisc
  [author [wikiAuthors], title "Damping",
  howPublishedU "https://en.wikipedia.org/wiki/Damping_ratio",
  month Jul, year 2019]
  "dampingSource"

accelerationWiki = cMisc [author [wikiAuthors],
  title "Acceleration", howPublishedU "https://en.wikipedia.org/wiki/Acceleration",
  month Jun, year 2019]
  "accelerationWiki"

velocityWiki = cMisc [author [wikiAuthors],
  title "Velocity", howPublishedU "https://en.wikipedia.org/wiki/Velocity",
  month Jun, year 2019]
  "velocityWiki"

cartesianWiki = cMisc
  [author [wikiAuthors], title "Cartesian coordinate system",
  howPublishedU "https://en.wikipedia.org/wiki/Cartesian_coordinate_system",
  month Jun, year 2019]
  "cartesianWiki"

laplaceWiki
  = cMisc
      [author [wikiAuthors], title "Laplace transform",
       howPublishedU "https://en.wikipedia.org/wiki/Laplace_transform",
       month Nov, year 2020]
      "laplaceWiki"

pidWiki
  = cMisc
      [author [wikiAuthors], title "PID controller",
       howPublishedU "https://en.wikipedia.org/wiki/PID_controller", month Oct,
       year 2020]
      "pidWiki"

ilpWiki
  = cMisc
      [author [wikiAuthors], title "Integer programming",
       howPublishedU "https://en.wikipedia.org/wiki/Integer_programming", month Mar,
       year 2023]
      "ilpWiki"

elemListWiki
  = cMisc
      [author [wikiAuthors], title "List of chemical elements",
       howPublishedU "https://en.wikipedia.org/wiki/List_of_chemical_elements",
       month Jan, year 2023]
      "elemListWiki"

-- * Misc

smithChemSpec
  = cMisc
      [author [spencerSmith], title "Assignment 2",
       howPublishedU "https://gitlab.cas.mcmaster.ca/smiths/se2aa4_cs2me3/-/blob/master/Assignments/PreviousYears/2020/A2-ChemReacts/A2.pdf",
       month Feb, year 2020]
      "smithChemSpec"


-- * Common Cite Fields

jnlCGJ :: String
jnlCGJ = "Canadian Geotechnical Journal"

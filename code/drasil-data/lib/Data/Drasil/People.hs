-- | Define people for use in Drasil. Used often in authors and citations.
module Data.Drasil.People where

import Language.Drasil (Person, person, person', personWM, personWM', mononym)
  
spencerSmith, henryFrankis, nKoothoor, dParnas, daAruliah, gWilson,
  cTitus, kdHuff, nChueHong, mDavis, rGuy, shdHaddock, imMitchell, mdPlumblet,
  bWaugh, epWhite, pWilson, pcClements, luthfi, alex, nikitha, thulasi,
  lLund, brooks, mLightstone, lLai, pjAgerfalk, nKraiem, jRalyte, jBueche,
  fIncropera, dDewitt, tBergman, aLavine, jRobertson, sRobertson, 
  wlBeason, tlKohutek, jmBracci, qhQian, dyZhu, cfLee, grChen, dgFredlund,
  jKrahn, dStolle, yCLi, ymChen, tltZhan, ssLing, pjCleall, pGuo, aBergholm,
  mCampidelli,   dmWiess, sPalmer, scottSmith, bKarchewski, rHuston, 
  hJosephs, nrMorgenstern, vePrice, samCrawford, rcHibbeler, olu, rodPierce,
  pWexler, dong, jCarette, jBalaci, ngConnelly, haFavre, atHutton, mNiazi,
  dScime, dSzymczak, tWu, tDahmus, rmHartshorn, gpMoss, whPowell :: Person
  
pjAgerfalk    = person    "PJ"                        "Agerfalk"
daAruliah     = personWM  "D"         ["A"]           "Aruliah"
jBalaci       = person    "Jason"                     "Balaci"
wlBeason      = personWM  "W"         ["Lynn"]        "Beason"
aBergholm     = person    "A"                         "Bergholm"
tBergman      = personWM  "T"         ["L"]           "Bergman"
jmBracci      = personWM  "Joseph"    ["M"]           "Bracci"
jBueche       = personWM  "J"         ["Frederick"]   "Bueche"
mCampidelli   = person    "Manuel"                    "Campidelli"
jCarette      = person    "Jacques"                   "Carette"
grChen        = personWM' "G"         ["R"]           "Chen"
ymChen        = person'   "Yun-Min"                   "Chen"
pjCleall      = personWM  "Peter"     ["John"]        "Cleall"
pcClements    = personWM  "P"         ["C"]           "Clements" -- The Modular Structure of Complex Systems ICSE '84
ngConnelly    = personWM  "Neil"      ["G"]           "Connelly" -- IUPAC
samCrawford   = personWM  "Samuel"    ["J"]           "Crawford"
tDahmus       = person    "Ture"                      "Damhus" -- IUPAC
mDavis        = person    "Matt"                      "Davis"
dDewitt       = personWM  "D"         ["P"]           "Dewitt"
haFavre       = personWM  "Henri"     ["A"]           "Favre" -- IUPAC
henryFrankis  = person    "Henry"                     "Frankis"
dgFredlund    = personWM  "D"         ["G"]           "Fredlund"
rGuy          = personWM  "Richard"   ["T"]           "Guy"
pGuo          = person    "Peijun"                    "Guo"
shdHaddock    = personWM  "Steven"    ["H", "D"]      "Haddock"
rmHartshorn   = personWM  "Richard"   ["M"]           "Hartshorn" -- IUPAC
alex          = person    "Alex"                      "Halliwushka"
rcHibbeler    = personWM' "R"         ["C"]           "Hibbeler"
rHuston       = person    "Ronald"                    "Huston"
atHutton      = personWM  "Alan"      ["T"]           "Hutton" -- IUPAC
nChueHong     = personWM  "Neil"      ["P"]           "Chue Hong"
kdHuff        = personWM  "Kathryn"   ["D"]           "Huff"
fIncropera    = personWM  "F"         ["P"]           "Incropera"
thulasi       = person    "Thulasi"                   "Jegatheesan"
hJosephs      = person    "Harold"                    "Josephs"
bKarchewski   = person    "Brandon"                   "Karchewski"
tlKohutek     = personWM  "Terry"     ["L"]           "Kohutek"
nKoothoor     = person    "Nirmitha"                  "Koothoor"
nKraiem       = person    "N"                         "Kraiem"
jKrahn        = person    "J"                         "Krahn"
nikitha       = person    "Nikitha"                   "Krithnan"
lLai          = person    "Lei"                       "Lai"
aLavine       = personWM  "A"         ["S"]           "Lavine"
cfLee         = personWM' "C"         ["F"]           "Lee"
mLightstone   = person    "Marilyn"                   "Lightstone"
yCLi          = person'   "Yu-Chao"                   "Li"
ssLing        = person'   "Sao-Sheng"                 "Ling"
lLund         = person    "Lance"                     "Lund"
brooks        = person    "Brooks"                    "MacLachlan"
luthfi        = person    "Luthfi"                    "Mawarid"
mNiazi        = person    "Maryyam"                   "Miazi"
imMitchell    = personWM  "Ian"       ["M"]           "Mitchell"
nrMorgenstern = personWM  "N"         ["R"]           "Morgenstern"
gpMoss        = personWM  "G"         ["P"]           "Moss" -- IUPAC
sPalmer       = person    "Steven"                    "Palmer"
dParnas       = personWM  "David"     ["L"]           "Parnas"
mdPlumblet    = personWM  "Mark"      ["D"]           "Plumblet"
whPowell      = personWM  "Warren"    ["H"]           "Powell" -- IUPAC
vePrice       = personWM  "P"         ["E"]           "Price"
qhQian        = personWM' "Q"         ["H"]           "Qian"
jRalyte       = person    "J"                         "Ralyte"
  --FIXME: person takes strings but we need an "e" with an accent
  -- S "J. Ralyt" :+: (F Acute 'e')
jRobertson    = person    "James"                     "Robertson"
sRobertson    = person    "Suzanne"                   "Robertson"
dScime        = person    "Dan"                       "Scime"
scottSmith    = person    "Scott"                     "Smith"
spencerSmith  = personWM  "W"         ["Spencer"]     "Smith"
dStolle       = person    "Dieter"                    "Stolle"
dSzymczak     = person    "Dan"                       "Szymczak"
cTitus        = person    "C"                         "Titus"
bWaugh        = person    "Ben"                       "Waugh" -- Best Practices for Scientific Computing 2013
pWexler       = person    "Philip"                    "Wexler"
epWhite       = personWM  "Ethan"     ["P"]           "White" -- Best Practices for Scientific Computing 2013
gWilson       = person    "Greg"                      "Wilson"
pWilson       = person    "Paul"                      "Wilson" -- Best Practices for Scientific Computing 2013
tWu           = person    "Ting-Yu"                   "Wu"
tltZhan       = personWM  "Tony"      ["L","T"]       "Zhan"
dyZhu         = personWM' "D"         ["Y"]           "Zhu"
dmWiess       = personWM  ""          []              "Wiess"
olu           = person    "Olu"                       "Owojaiye"
rodPierce     = person    "Rod"                       "Pierce"
dong          = person    "Dong"                      "Chen"

-- Right now, we have to say these are each a 'Person', even though they clearly aren't
harpCollins, iupac, wikiAuthors :: Person
harpCollins = mononym "HarperCollins Publishers"
iupac       = mononym "International Union of Pure and Applied Chemistry"
wikiAuthors = mononym "Wikipedia Contributors"

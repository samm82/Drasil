-- | Markdown file creator for generated GOOL code.
module Language.Drasil.Markdown.CreateMd (
    -- * Main Function
    makeMd,
    -- * Section Creators
    introInfo, whatInfo, verInfo, unsupOS, extLibSec, regularSec, instDoc, endNote) 
    where

import Prelude hiding ((<>))
import Text.PrettyPrint.HughesPJ (Doc, empty, isEmpty, vcat, text, (<+>),
    (<>), comma, punctuate, hsep)

-- | Separates document sections.
type Seperator = Doc

-- | Combines a list of sentences into a final Doc, also appends end note.
makeMd :: [Doc] -> Doc
makeMd = vcat . punctuate secSep . filtEmp

-- | Example title, authors, and maybe purpose section.
introInfo :: String -> [String] -> Maybe String -> Doc
introInfo name auths descr = introSec (text name) (prepName name)
  (listToDoc auths) (length auths) (maybePurpDoc descr)

-- | Helper for prepending the example name.
prepName :: String -> Doc
prepName name = text "> The program documented here is called" <+> text name

-- | Instruction section, contains 3 paragraphs, Running, Building and Config Files.
-- The Config file section is only displayed if there are configuration files.
instDoc :: [String] -> Doc
instDoc cfp = regularSec (text "Making Examples") 
    (runInstDoc <> doubleSep <> makeInstDoc) <> configSec cfp 

-- | Helper for creating optional Purpose subsection as Doc
maybePurpDoc :: Maybe String -> Doc
maybePurpDoc = maybe empty (\descr-> doubleSep <> text "> Purpose:" <+> text descr)

-- | 'What' section in generated README file, does not display if empty
whatInfo :: Maybe String -> Doc
whatInfo = maybe empty (regularSec (text "What") . text)

-- | Helper for giving instructions on the command line.
commandLine :: Doc
commandLine = text $ "In your terminal command line, enter the same directory as this " ++
    "README file. Then enter the following line:"

-- | Helper for giving instructions on how to run the program.
runInstDoc :: Doc
runInstDoc = text "How to Run the Program:" <> contSep <>
    commandLine <> contSep <> bkQuote3 <> contSep <> text "make run RUNARGS=input.txt" <> contSep <> bkQuote3

-- | Helper for giving instructions on how to build the program.
makeInstDoc :: Doc
makeInstDoc = text "How to Build the Program:" <> contSep <> commandLine <> contSep <>
    bkQuote3 <> contSep <> text "make build" <> contSep <> bkQuote3

-- | Helper for giving instructions for configuration files.
configSec :: [String] -> Doc
configSec [] = empty
configSec cfp = doubleSep <> regularSec (text "Configuration Files") (text ("Configuration files are files that must be " ++
    "in the same directory as the executable in order to run or build successfully.")
    <> doubleSep <> bkQuote <> listToDoc cfp <> bkQuote)

-- | Language version section.
verInfo :: String -> String -> Doc
verInfo pl plv = regularSec (text "Version") (bkQuote <> text pl <+> text plv <> bkQuote)

-- | Invalid Operating Systems section, does not display unless atleast 1 invalid OS.
unsupOS :: Maybe String -> Doc
unsupOS = maybe empty (\uns-> regularSec (text "Unsupported Operating Systems")
    (text $ "- " ++ uns))

-- | External Libraries section. The inputs are a list of name and version pairs
-- and a list of the corresponding version numbers, these are first combined into a 
-- list of triplets, and then each printed on a new line.
extLibSec:: [(String, String)] -> [String]-> Doc
extLibSec libns libfps = 
    let libs = addListToTuple libns libfps
        formattedLibs = (hsep . punctuate contSep . filtEmp . 
            map libStatment) libs
    in if isEmpty formattedLibs then empty else 
            regularSec (text "External Libraries") formattedLibs

-- | Helper for formatting the library section.
libStatment :: (String, String, String) -> Doc
libStatment ("","", _) = empty
libStatment (nam,vers, fp) = bkQuote <> text nam <+>
    text vers <> bkQuote <> if fp == "" then empty else
    text ". The local file path to the library is" <+> bkQuote <> text fp <> bkQuote

-- | Helper for converting a list of tuples and another list into a list of triplets.
addListToTuple :: [(String,String)] -> [String] -> [(String, String, String)]
addListToTuple [] [] = []
addListToTuple ((n,v):_) [] = [(n,v,"")]
addListToTuple ((n,v):xtup) (l:xlst) = (n,v,l):addListToTuple xtup xlst
addListToTuple _ _ = []

-- TODO: Allow licenses to have updated date information.
-- | License section.
license :: Doc -> Doc
license auth = text "Copyright (c) 2021," <+> auth <>
  text ". All rights reserved. Please see the [full license](https://github.com/JacquesCarette/Drasil/blob/4b9ad0a3016fecb3c7a2aa82ab142f9e805b5cc8/LICENSE) for more details."
-- | Drasil Tree icon. Uses HTML directly to format image since normal markdown doesn't support it.
drasilImage :: Doc
drasilImage = alignImage "../../../../drasil-website/WebInfo/images/Icon.png"
-- | Aligns an image to the center using HTML, since markdown doesn't support it.
alignImage :: FilePath -> Doc
alignImage img = text "<p align=\"center\">" <>
  contSep <> text ("<img src=\"" ++ img ++ "\" alt=\"Drasil Tree\" width=\"200\" />")
  <> contSep <> text "</p>"
-- | End section.
endNote :: [String] -> Doc
endNote auth = text "*This README is a software artifact generated by Drasil.*" <> doubleSep <> license (listToDoc auth) <> doubleSep <> drasilImage 

-- | Section seperators.
secSep, contSep, doubleSep, bkQuote, bkQuote3 :: Seperator
-- | Horizontal line separator.
secSep = text "\n\n------------------------------------------------------------"
-- | Newline separator.
contSep = text "\n"
-- | Double newline separator.
doubleSep = text "\n\n"
-- | Back quote separator.
bkQuote = text "`"
-- | Triple backquote separator.
bkQuote3 = text "```"


-- FIXME as explained in #2224 we still need to add in the purpose section, 
-- this could be done by adding a third parameter to introSec
-- | Constructs introduction section from header and message.
introSec ::  Doc -> Doc -> Doc -> Int -> Doc -> Doc
introSec hd name ms1 l descr = text "#" <+> hd <+> contSep <> name <> doubleSep <> 
  (if l == 1 then text "> Author:" else text "> Authors: ") <+> ms1 <> descr

-- | Constructs regular section section from header and message.
regularSec :: Doc -> Doc -> Doc
regularSec hd ms = text "##" <+> hd <+> contSep <+> ms

-- | Helper for 'makeMd' and 'extLibSec'.
filtEmp :: [Doc] -> [Doc]
filtEmp = filter (not . isEmpty) 

-- | Helper for authors and configuration files.
listToDoc :: [String] -> Doc
listToDoc = hsep . punctuate comma . map text

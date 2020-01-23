{-# LANGUAGE TemplateHaskell #-}

module GOOL.Drasil.State (
  GS, GOOLState(..), FS, CS, MS, lensFStoGS, lensGStoFS, lensFStoCS, lensFStoMS,
  lensCStoMS, lensMStoCS, lensMStoFS, headers, sources, mainMod, currMain, 
  initialState, initialFS, modifyReturn, modifyReturnFunc, modifyReturnFunc2, 
  modifyReturnList, tempStateChange, addODEFilePaths, addFile, 
  addCombinedHeaderSource, addHeader, addSource, addProgNameToPaths, setMainMod,
  addODEFile, getODEFiles, addLangImport, addExceptionImports, getLangImports, 
  addLibImport, addLibImports, getLibImports, addModuleImport, getModuleImports,
  addHeaderLangImport, getHeaderLangImports, addHeaderLibImport, 
  getHeaderLibImports, addHeaderModImport, getHeaderModImports, addDefine, 
  getDefines, addHeaderDefine, getHeaderDefines, addUsing, getUsing, 
  addHeaderUsing, getHeaderUsing, setFilePath, getFilePath, setModuleName, 
  getModuleName, setClassName, getClassName, setCurrMain, getCurrMain, addClass,
  getClasses, updateClassMap, getClassMap, updateMethodExcMap, getMethodExcMap,
  addParameter, getParameters, setODEDepVars, getODEDepVars, setODEOthVars, 
  getODEOthVars, setOutputsDeclared, isOutputsDeclared, addException, 
  addExceptions, getExceptions, setScope, getScope, setCurrMainFunc, 
  getCurrMainFunc
) where

import GOOL.Drasil.Data (FileType(..), ScopeTag(..), Exception(..), FileData)

import Control.Lens (Lens', (^.), lens, makeLenses, over, set)
import Control.Lens.Tuple (_1, _2)
import Control.Monad.State (State, modify, get, gets, put)
import Data.List (sort, nub)
import Data.Maybe (isNothing)
import Data.Map (Map, fromList, empty, insert, union)

data GOOLState = GS {
  _headers :: [FilePath],
  _sources :: [FilePath],
  _mainMod :: Maybe FilePath,
  _classMap :: Map String String,
  _odeFiles :: [FileData],

  -- Only used for Java
  _methodExceptionMap :: Map String [Exception]
} 
makeLenses ''GOOLState

data MethodState = MS {
  _currParameters :: [String],
  _currODEDepVars :: [String],
  _currODEOthVars :: [String],

  -- Only used for Java
  _outputsDeclared :: Bool,
  _exceptions :: [Exception],
  
  -- Only used for C++
  _currScope :: ScopeTag,
  _currMainFunc :: Bool
}
makeLenses ''MethodState

newtype ClassState = CS {
  _currClassName :: String
}
makeLenses ''ClassState

data FileState = FS {
  _currModName :: String,
  _currFilePath :: FilePath,
  _currMain :: Bool,
  _currClasses :: [String],
  _langImports :: [String],
  _libImports :: [String],
  _moduleImports :: [String],

  -- C++ only
  _headerLangImports :: [String],
  _headerLibImports :: [String],
  _headerModImports :: [String],
  _defines :: [String],
  _headerDefines :: [String],
  _using :: [String],
  _headerUsing :: [String]
}
makeLenses ''FileState

type GS = State GOOLState
type FS = State (GOOLState, FileState)
type CS = State ((GOOLState, FileState), ClassState)
type MS = State (((GOOLState, FileState), ClassState), MethodState)

-------------------------------
---- Lenses between States ----
-------------------------------

-- GS - FS --

getFSfromGS :: GOOLState -> (GOOLState, FileState)
getFSfromGS gs = (gs, initialFS)

setFSfromGS :: GOOLState -> (GOOLState, FileState) -> GOOLState
setFSfromGS _ (gs, _) = gs

lensGStoFS :: Lens' GOOLState (GOOLState, FileState)
lensGStoFS = lens getFSfromGS setFSfromGS

lensFStoGS :: Lens' (GOOLState, FileState) GOOLState
lensFStoGS = _1

-- FS - CS --

getCSfromFS :: (GOOLState, FileState) -> ((GOOLState, FileState), ClassState)
getCSfromFS fs = (fs, initialCS)

setCSfromFS :: (GOOLState, FileState) -> ((GOOLState, FileState), ClassState) 
  -> (GOOLState, FileState)
setCSfromFS _ (fs, _) = fs 

lensFStoCS :: Lens' (GOOLState, FileState) ((GOOLState, FileState), ClassState)
lensFStoCS = lens getCSfromFS setCSfromFS

-- FS - MS --

getMSfromFS :: (GOOLState, FileState) -> 
  (((GOOLState, FileState), ClassState), MethodState)
getMSfromFS (gs, fs) = (((gs, fs), initialCS), initialMS)

setMSfromFS :: (GOOLState, FileState) -> 
  (((GOOLState, FileState), ClassState), MethodState) -> (GOOLState, FileState)
setMSfromFS _ ((fs, _), _) = fs

lensFStoMS :: Lens' (GOOLState, FileState) 
  (((GOOLState, FileState), ClassState), MethodState)
lensFStoMS = lens getMSfromFS setMSfromFS

lensMStoFS :: Lens' (((GOOLState, FileState), ClassState), MethodState) 
  (GOOLState, FileState) 
lensMStoFS = _1 . _1

-- CS - MS --

getMSfromCS :: ((GOOLState, FileState), ClassState) -> 
  (((GOOLState, FileState), ClassState), MethodState)
getMSfromCS cs = (cs, initialMS)

setMSfromCS :: ((GOOLState, FileState), ClassState) -> 
  (((GOOLState, FileState), ClassState), MethodState) -> 
  ((GOOLState, FileState), ClassState)
setMSfromCS _ (cs, _) = cs

lensCStoMS :: Lens' ((GOOLState, FileState), ClassState) 
  (((GOOLState, FileState), ClassState), MethodState)
lensCStoMS = lens getMSfromCS setMSfromCS

lensMStoCS :: Lens' (((GOOLState, FileState), ClassState), MethodState)
  ((GOOLState, FileState), ClassState)
lensMStoCS = _1

-------------------------------
------- Initial States -------
-------------------------------

initialState :: GOOLState
initialState = GS {
  _headers = [],
  _sources = [],
  _mainMod = Nothing,
  _classMap = empty,
  _odeFiles = [],

  _methodExceptionMap = empty
}

initialFS :: FileState
initialFS = FS {
  _currModName = "",
  _currFilePath = "",
  _currMain = False,
  _currClasses = [],
  _langImports = [],
  _libImports = [],
  _moduleImports = [],

  _headerLangImports = [],
  _headerLibImports = [],
  _headerModImports = [],
  _defines = [],
  _headerDefines = [],
  _using = [],
  _headerUsing = []
}

initialCS :: ClassState
initialCS = CS {
  _currClassName = ""
}

initialMS :: MethodState
initialMS = MS {
  _currParameters = [],
  _currODEDepVars = [],
  _currODEOthVars = [],

  _outputsDeclared = False,
  _exceptions = [],

  _currScope = Priv,
  _currMainFunc = False
}

-------------------------------
------- State Patterns -------
-------------------------------

modifyReturn :: (s -> s) -> a -> State s a
modifyReturn sf v = do
  modify sf
  return v

modifyReturnFunc :: (b -> s -> s) -> (b -> a) -> State s b -> State s a
modifyReturnFunc sf vf st = do
  v <- st
  modify $ sf v
  return $ vf v

modifyReturnFunc2 :: (c -> b -> s -> s) -> (c -> b -> a) -> State s c -> 
  State s b -> State s a
modifyReturnFunc2 sf vf st1 st2 = do
  v1 <- st1
  v2 <- st2
  modify $ sf v1 v2
  return $ vf v1 v2

modifyReturnList :: [State s b] -> (s -> s) -> 
  ([b] -> a) -> State s a
modifyReturnList l sf vf = do
  v <- sequence l
  modify sf
  return $ vf v

tempStateChange :: (s -> s) -> State s a -> State s a
tempStateChange f st = do
  s <- get
  modify f
  v <- st
  put s
  return v

-------------------------------
------- State Modifiers -------
-------------------------------

addODEFilePaths :: GOOLState -> 
  (((GOOLState, FileState), ClassState), MethodState) -> 
  (((GOOLState, FileState), ClassState), MethodState)
addODEFilePaths s = over (_1 . _1 . _1 . headers) (s ^. headers ++)
  . over (_1 . _1 . _1 . sources) (s ^. sources ++)

addFile :: FileType -> FilePath -> GOOLState -> GOOLState
addFile Combined = addCombinedHeaderSource
addFile Source = addSource
addFile Header = addHeader

addHeader :: FilePath -> GOOLState -> GOOLState
addHeader fp = over headers (\h -> if fp `elem` h then 
  error $ "Multiple files with same name encountered: " ++ fp else h ++ [fp])

addSource :: FilePath -> GOOLState -> GOOLState
addSource fp = over sources (\s -> if fp `elem` s then 
  error $ "Multiple files with same name encountered: " ++ fp else s ++ [fp])

addCombinedHeaderSource :: FilePath -> GOOLState -> GOOLState
addCombinedHeaderSource fp = addSource fp . addHeader fp 

addProgNameToPaths :: String -> GOOLState -> GOOLState
addProgNameToPaths n = over mainMod (fmap f) . over sources (map f) . 
  over headers (map f)
  where f = ((n++"/")++)

setMainMod :: String -> GOOLState -> GOOLState
setMainMod n = over mainMod (\m -> if isNothing m then Just n else error 
  "Multiple modules with main methods encountered")

addODEFile :: FileData -> (((GOOLState, FileState), ClassState), MethodState) 
  -> (((GOOLState, FileState), ClassState), MethodState)
addODEFile f = over _1 $ over _1 $ over _1 $ over odeFiles (f:)

getODEFiles :: GS [FileData]
getODEFiles = gets (^. odeFiles)

addLangImport :: String -> (((GOOLState, FileState), ClassState), MethodState) 
  -> (((GOOLState, FileState), ClassState), MethodState)
addLangImport i = over _1 $ over _1 $ over _2 $ over langImports (\is -> 
  if i `elem` is then is else sort $ i:is)

addExceptionImports :: [Exception] -> 
  (((GOOLState, FileState), ClassState), MethodState) -> 
  (((GOOLState, FileState), ClassState), MethodState)
addExceptionImports es = over (_1 . _1 . _2 . langImports) (\is -> sort $ nub $ 
  is ++ imps)
  where mkImport l e = if null l then "" else l ++ "." ++ e
        imps = filter (not . null) $ zipWith mkImport (map loc es) (map exc es)

getLangImports :: FS [String]
getLangImports = gets ((^. langImports) . snd)

addLibImport :: String -> (((GOOLState, FileState), ClassState), MethodState)
  -> (((GOOLState, FileState), ClassState), MethodState)
addLibImport i = over _1 $ over _1 $ over _2 $ over libImports (\is -> 
  if i `elem` is then is else sort $ i:is)

addLibImports :: [String] -> (((GOOLState, FileState), ClassState), MethodState)
  -> (((GOOLState, FileState), ClassState), MethodState)
addLibImports is s = foldl (flip addLibImport) s is

getLibImports :: FS [String]
getLibImports = gets ((^. libImports) . snd)

addModuleImport :: String -> (((GOOLState, FileState), ClassState), MethodState)
  -> (((GOOLState, FileState), ClassState), MethodState)
addModuleImport i = over _1 $ over _1 $ over _2 $ over moduleImports (\is -> 
  if i `elem` is then is else sort $ i:is)

getModuleImports :: FS [String]
getModuleImports = gets ((^. moduleImports) . snd)

addHeaderLangImport :: String -> 
  (((GOOLState, FileState), ClassState), MethodState) -> 
  (((GOOLState, FileState), ClassState), MethodState)
addHeaderLangImport i = over _1 $ over _1 $ over _2 $ over headerLangImports 
  (\is -> if i `elem` is then is else sort $ i:is)

getHeaderLangImports :: FS [String]
getHeaderLangImports = gets ((^. headerLangImports) . snd)

addHeaderLibImport :: String -> 
  (((GOOLState, FileState), ClassState), MethodState) -> 
  (((GOOLState, FileState), ClassState), MethodState)
addHeaderLibImport i = over _1 $ over _1 $ over _2 $ over headerLibImports 
  (\is -> if i `elem` is then is else sort $ i:is)

getHeaderLibImports :: FS [String]
getHeaderLibImports = gets ((^. headerLibImports) . snd)

addHeaderModImport :: String -> 
  (((GOOLState, FileState), ClassState), MethodState) -> 
  (((GOOLState, FileState), ClassState), MethodState)
addHeaderModImport i = over _1 $ over _1 $ over _2 $ over headerModImports 
  (\is -> if i `elem` is then is else sort $ i:is)

getHeaderModImports :: FS [String]
getHeaderModImports = gets ((^. headerModImports) . snd)

addDefine :: String -> (((GOOLState, FileState), ClassState), MethodState) -> 
  (((GOOLState, FileState), ClassState), MethodState)
addDefine d = over _1 $ over _1 $ over _2 $ over defines (\ds -> if d `elem` ds 
  then ds else sort $ d:ds)

getDefines :: FS [String]
getDefines = gets ((^. defines) . snd)
  
addHeaderDefine :: String -> (((GOOLState, FileState), ClassState), MethodState)
  -> (((GOOLState, FileState), ClassState), MethodState)
addHeaderDefine d = over _1 $ over _1 $ over _2 $ over headerDefines (\ds -> 
  if d `elem` ds then ds else sort $ d:ds)

getHeaderDefines :: FS [String]
getHeaderDefines = gets ((^. headerDefines) . snd)

addUsing :: String -> (((GOOLState, FileState), ClassState), MethodState) -> 
  (((GOOLState, FileState), ClassState), MethodState)
addUsing u = over _1 $ over _1 $ over _2 $ over using (\us -> if u `elem` us 
  then us else sort $ u:us)

getUsing :: FS [String]
getUsing = gets ((^. using) . snd)

addHeaderUsing :: String -> (((GOOLState, FileState), ClassState), MethodState) 
  -> (((GOOLState, FileState), ClassState), MethodState)
addHeaderUsing u = over _1 $ over _1 $ over _2 $ over headerUsing (\us -> 
  if u `elem` us then us else sort $ u:us)

getHeaderUsing :: FS [String]
getHeaderUsing = gets ((^. headerUsing) . snd)

setFilePath :: FilePath -> (GOOLState, FileState) -> (GOOLState, FileState)
setFilePath fp = over _2 (set currFilePath fp)

getFilePath :: FS FilePath
getFilePath = gets ((^. currFilePath) . snd)

setModuleName :: String -> (GOOLState, FileState) -> (GOOLState, FileState)
setModuleName n = over _2 (set currModName n)

getModuleName :: FS String
getModuleName = gets ((^. currModName) . snd)

setClassName :: String -> ((GOOLState, FileState), ClassState) -> 
  ((GOOLState, FileState), ClassState)
setClassName n = over _2 (set currClassName n)

getClassName :: MS String
getClassName = gets ((^. currClassName) . snd . fst)

setCurrMain :: (((GOOLState, FileState), ClassState), MethodState) -> 
  (((GOOLState, FileState), ClassState), MethodState)
setCurrMain = over _1 $ over _1 $ over _2 $ over currMain (\b -> if b then 
  error "Multiple main functions defined" else not b)

getCurrMain :: FS Bool
getCurrMain = gets ((^. currMain) . snd)

addClass :: String -> ((GOOLState, FileState), ClassState) -> (
  (GOOLState, FileState), ClassState)
addClass c = over _1 $ over _2 $ over currClasses (\cs -> if c `elem` cs then 
  error "Multiple classes with same name in same file" else c:cs)

getClasses :: FS [String]
getClasses = gets ((^. currClasses) . snd)

updateClassMap :: String -> (GOOLState, FileState) -> (GOOLState, FileState)
updateClassMap n (gs, fs) = over _1 (over classMap (union (fromList $ 
  zip (repeat n) (fs ^. currClasses)))) (gs, fs)

getClassMap :: MS (Map String String)
getClassMap = gets ((^. classMap) . fst . fst . fst)

updateMethodExcMap :: String ->
  (((GOOLState, FileState), ClassState), MethodState) 
  -> (((GOOLState, FileState), ClassState), MethodState)
updateMethodExcMap n (((gs, fs), cs), ms) = over (_1 . _1 . _1 . 
  methodExceptionMap) (insert (mn ++ "." ++ n) (ms ^. exceptions)) 
  (((gs, fs), cs), ms)
  where mn = fs ^. currModName

getMethodExcMap :: MS (Map String [Exception])
getMethodExcMap = gets ((^. methodExceptionMap) . fst . fst . fst)

addParameter :: String -> (((GOOLState, FileState), ClassState), MethodState) 
  -> (((GOOLState, FileState), ClassState), MethodState)
addParameter p = over _2 $ over currParameters (\ps -> if p `elem` ps then 
  error $ "Function has duplicate parameter: " ++ p else ps ++ [p])

getParameters :: MS [String]
getParameters = gets ((^. currParameters) . snd)

setODEDepVars :: [String] -> (((GOOLState, FileState), ClassState), MethodState)
  -> (((GOOLState, FileState), ClassState), MethodState)
setODEDepVars vs = over _2 $ set currODEDepVars vs

getODEDepVars :: MS [String]
getODEDepVars = gets ((^. currODEDepVars) . snd)

setODEOthVars :: [String] -> (((GOOLState, FileState), ClassState), MethodState)
  -> (((GOOLState, FileState), ClassState), MethodState)
setODEOthVars vs = over _2 $ set currODEOthVars vs

getODEOthVars :: MS [String]
getODEOthVars = gets ((^. currODEOthVars) . snd)

setOutputsDeclared :: (((GOOLState, FileState), ClassState), MethodState) -> 
  (((GOOLState, FileState), ClassState), MethodState)
setOutputsDeclared = over _2 $ set outputsDeclared True

isOutputsDeclared :: MS Bool
isOutputsDeclared = gets ((^. outputsDeclared) . snd)

addException :: Exception -> (((GOOLState, FileState), ClassState), MethodState)
  -> (((GOOLState, FileState), ClassState), MethodState)
addException e = over (_2 . exceptions) (\es -> if e `elem` es then es else 
  es ++ [e])

addExceptions :: [Exception] -> 
  (((GOOLState, FileState), ClassState), MethodState) -> 
  (((GOOLState, FileState), ClassState), MethodState)
addExceptions es = over (_2 . exceptions) (\exs -> nub $ exs ++ es)

getExceptions :: MS [Exception]
getExceptions = gets ((^. exceptions) . snd)

setScope :: ScopeTag -> (((GOOLState, FileState), ClassState), MethodState) -> 
  (((GOOLState, FileState), ClassState), MethodState)
setScope scp = over _2 $ set currScope scp

getScope :: MS ScopeTag
getScope = gets ((^. currScope) . snd)

setCurrMainFunc :: Bool -> (((GOOLState, FileState), ClassState), MethodState) 
  -> (((GOOLState, FileState), ClassState), MethodState)
setCurrMainFunc m = over _2 $ set currMainFunc m

getCurrMainFunc :: MS Bool
getCurrMainFunc = gets ((^. currMainFunc) . snd)
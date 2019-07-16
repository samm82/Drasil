{-# LANGUAGE PostfixOperators #-}

module Test.HelloWorld (helloWorld) where

import Language.Drasil.Code (
  PackageSym(..), RenderSym(..), PermanenceSym(..),
  BodySym(..), BlockSym(..), ControlBlockSym(..), StateTypeSym(..), 
  StatementSym(..), ControlStatementSym(..),  VariableSym(..), ValueSym(..), 
  NumericExpression(..), BooleanExpression(..), ValueExpression(..), 
  Selector(..), FunctionSym(..), SelectorFunction(..), MethodSym(..), 
  ModuleSym(..))
import Prelude hiding (return,print,log,exp,sin,cos,tan,const)
import Test.Helper (helper)

helloWorld :: (PackageSym repr) => repr (Package repr)
helloWorld = packMods "HelloWorld" [docMod description $ 
  fileDoc (buildModule "HelloWorld" ["Helper"] [helloWorldMain] []), helper]

description :: String
description = "Tests various GOOL functions. It should run without errors."

helloWorldMain :: (RenderSym repr) => repr (Method repr)
helloWorldMain = mainMethod "HelloWorld" (body [ helloInitVariables, 
    helloListSlice,
    block [ifCond [(varVal (var "b" int) ?>= litInt 6, bodyStatements [varDecDef (var "dummy" string) (litString "dummy")]),
      (varVal (var "b" int) ?== litInt 5, helloIfBody)] helloElseBody, helloIfExists,
    helloSwitch, helloForLoop, helloWhileLoop, helloForEachLoop, helloTryCatch]])

helloInitVariables :: (RenderSym repr) => repr (Block repr)
helloInitVariables = block [comment "Initializing variables",
  varDec $ var "a" int, 
  varDecDef (var "b" int) (litInt 5),
  listDecDef (var "myOtherList" (listType static_ float)) [litFloat 1.0, 
    litFloat 1.5],
  varDecDef (var "oneIndex" int) (indexOf (varVal $ var "myOtherList" (listType 
    static_ float)) (litFloat 1.0)),
  printLn (varVal $ var "oneIndex" int),
  var "a" int &= listSize (varVal $ var "myOtherList" (listType static_ float)),
  valState (listAdd (varVal $ var "myOtherList" (listType static_ float))
    (litInt 2) (litFloat 2.0)),
  valState (listAppend (varVal $ var "myOtherList" (listType static_ float)) 
    (litFloat 2.5)),
  varDec $ var "e" float,
  var "e" int &= listAccess (varVal $ var "myOtherList"
    (listType static_ float)) (litInt 1),
  valState (listSet (varVal $ var "myOtherList" (listType static_ float)) 
    (litInt 1) (litFloat 17.4)),
  listDec 7 (var "myName" (listType static_ string)),
  stringSplit ' ' (var "myName" (listType static_ string)) (litString "Brooks Mac"),
  printLn (varVal $ var "myName" (listType static_ string)),
  listDecDef (var "boringList" (listType dynamic_ bool)) 
    [litFalse, litFalse, litFalse, litFalse, litFalse],
  printLn (varVal $ var "boringList" (listType dynamic_ bool)),
  listDec 2 $ var "mySlicedList" (listType static_ float)]

helloListSlice :: (RenderSym repr) => repr (Block repr)
helloListSlice = listSlice (var "mySlicedList" (listType static_ float)) 
  (varVal $ var "myOtherList" (listType static_ float)) (Just (litInt 1)) 
  (Just (litInt 3)) Nothing

helloIfBody :: (RenderSym repr) => repr (Body repr)
helloIfBody = addComments "If body" (body [
  block [
    varDec $ var "c" int,
    varDec $ var "d" int,
    assign (var "a" int) (litInt 5),
    var "b" int &= (varVal (var "a" int) #+ litInt 2),
    var "c" int &= (varVal (var "b" int) #+ litInt 3),
    var "d" int &= varVal (var "b" int),
    var "d" int &-= varVal (var "a" int),
    var "c" int &-= varVal (var "d" int),
    var "b" int &+= litInt 17,
    var "c" int &+= litInt 17,
    (&++) (var "a" int),
    (&++) (var "d" int),
    (&~-) (var "c" int),
    (&~-) (var "b" int),

    listDec 5 (var "myList" (listType static_ int)),
    objDecDef (var "myObj" char) (litChar 'o'),
    constDecDef (const "myConst" string) (litString "Imconstant"),

    printLn (varVal $ var "a" int),
    printLn (varVal $ var "b" int),
    printLn (varVal $ var "c" int),
    printLn (varVal $ var "d" int),
    printLn (varVal $ var "myOtherList" (listType static_ float)),
    printLn (varVal $ var "mySlicedList" (listType static_ float)),
    
    printStrLn "Type an int",
    getInput (var "d" int),
    printStrLn "Type another",
    discardInput],
  
  block [
    printLn (litString " too"),
    printStr "boo",
    print litTrue,
    print (litInt 0),
    print (litChar 'c'),
    printLn (litTrue ?!),
    printLn (litInt 1 #~),
    printLn (litFloat 4.0 #/^),
    printLn (litInt (-4) #|),
    printLn (log (litFloat 2.0)),
    multi [printLn (ln (litFloat 2.0)),
    printLn (exp (litFloat 2.0 #~)),
    printLn (sin (litFloat 2.0)),
    printLn (cos (litFloat 2.0)),
    printLn (tan (litFloat 2.0))],
    printLn (tan (litFloat 2.0)),
    printLn (litTrue ?&& litFalse),
    printLn (litTrue ?|| litFalse),
    printLn (litTrue ?&& (litFalse ?!)),
    printLn ((litTrue ?&& litTrue) ?!),
    printLn (litInt 6 #+ litInt 2),
    printLn (litInt 6 #- litInt 2),
    printLn (litInt 6 #* litInt 2),
    printLn (litInt 6 #/ litInt 2),
    printLn (litInt 6 #% litInt 4),
    printLn (litInt 6 #^ litInt 2),
    printLn (litInt 6 #+ (litInt 2 #* litInt 3)),
    printLn (csc (litFloat 1.0)),
    printLn (sec (litFloat 1.0)),
    printLn (varVal $ var "a" int),
    printLn (inlineIf litTrue (litInt 5) (litInt 0)),
    printLn (cot (litFloat 1.0))]])

helloElseBody :: (RenderSym repr) => repr (Body repr)
helloElseBody = bodyStatements [printLn (arg 5)]

helloIfExists :: (RenderSym repr) => repr (Statement repr)
helloIfExists = ifExists (varVal $ var "boringList" (listType dynamic_ bool)) 
  (oneLiner (printStrLn "Ew, boring list!")) (oneLiner (printStrLn "Great, no bores!"))

helloSwitch :: (RenderSym repr) => repr (Statement repr)
helloSwitch = switch (var "a" int) [(litInt 5, oneLiner (var "b" int &= litInt 10)), 
  (litInt 0, oneLiner (var "b" int &= litInt 5))]
  (oneLiner (var "b" int &= litInt 0))

helloForLoop :: (RenderSym repr) => repr (Statement repr)
helloForLoop = forRange "i" (litInt 0) (litInt 9) (litInt 1) (oneLiner (printLn (varVal $ var "i" int)))

helloWhileLoop :: (RenderSym repr) => repr (Statement repr)
helloWhileLoop = while (varVal (var "a" int) ?< litInt 13) (bodyStatements 
  [printStrLn "Hello", (&++) (var "a" int)]) 

helloForEachLoop :: (RenderSym repr) => repr (Statement repr)
helloForEachLoop = forEach "num" (listVar "myOtherList" static_ float) 
  (oneLiner (printLn (extFuncApp "Helper" "doubleAndAdd" float [varVal $ 
  iterVar "num" float, litFloat 1.0])))

helloTryCatch :: (RenderSym repr) => repr (Statement repr)
helloTryCatch = tryCatch (oneLiner (throw "Good-bye!"))
  (oneLiner (printStrLn "Caught intentional error"))

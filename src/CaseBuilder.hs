{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module CaseBuilder ( Def(..), Sel(..), buildCase, convert, switchState, deepCopy, ensure, buildCaseFromString ) where

import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.ByteString.Builder
import Parsing (Expr(..), parse, parseExpr)
import Control.Lens (makeLenses, (^.), (.~))

data Sel = Sel {
    _selNum :: Int,
    _selCom :: BS.ByteString,
    _selDef :: [Def]
} deriving (Show, Eq)

data Def = Case {
    _caseName :: BS.ByteString,
    _caseWidth :: Int,
    _caseSel ::  [Sel]
} | RTL {
    _rtlCode :: BS.ByteString
}           deriving (Show, Eq)

$(makeLenses ''Sel)
$(makeLenses ''Def)


indent :: Int -> Builder -> Builder
indent n x = lazyByteString (BS.replicate (toEnum n) ' ') <> x

buildCase :: Int -> Def -> Builder
buildCase n x = case x of
    Case switch bits sels ->
        indent n $ "case(" <> lazyByteString switch <> ")\n" <> foldr1 (<>) (map (buildSel bits (n + 2)) sels)
            <> indent (n + 2) "default: id_we = 1'b0;\n" <> indent n "endcase\n"
    RTL text -> indent n (lazyByteString text <> "\n")

buildSel :: Int -> Int -> Sel -> Builder
buildSel bits n x = case x of
    Sel val comment defs ->
        indent n $ intDec bits <> "'d" <> intDec val <> ": // " <> lazyByteString comment <> "\n" <>
        foldr1 (<>) (map (buildCase (n + 2)) defs)



type State = [Expr]

switchState :: State -> Expr -> State
switchState [] e = [e]
switchState (st:sts) e = case e of
    Code sp code -> case st of
        CaseSel st_sp _ _ _ -> if st_sp < sp then e:st:sts else switchState sts e
        Code _ _ -> undefined
    CaseSel sp name (w, num) com -> case st of
        CaseSel st_sp _ _ _ -> if st_sp < sp then e:st:sts else switchState sts e
        Code st_sp _ -> if st_sp < sp then e:st:sts else switchState sts e

popState :: State -> State
popState [] = []
popState (_:sts) = sts

deepCopy :: Def -> Def
deepCopy def = case def of
    Case name w sels -> Case name w $ map deepCopySel sels
    RTL code -> RTL code

deepCopySel :: Sel -> Sel
deepCopySel sel = case sel of
    Sel num com defs -> Sel num com $ map deepCopy defs

ensure :: State -> [Def] -> [Def]
ensure st = ensureDef (reverse st)

ensureDef :: State -> [Def] -> [Def]
ensureDef [] defs = defs
ensureDef (st:sts) [] = case st of
    CaseSel sp name (w, num) com -> [Case (BS.pack name) w (ensureSel (st:sts) [])]
    Code sp code -> [RTL (BS.pack code)]
ensureDef (st:sts) (def:defs) = ensureDefLoop (st:sts) (def:defs)


ensureDefLoop :: State -> [Def] -> [Def]
ensureDefLoop [] defs = defs
ensureDefLoop (st:sts) [] = []
ensureDefLoop (st:sts) (def:defs) = case st of
    CaseSel sp name (w, num) com -> case def of
        Case def_name def_w sels -> if def_name == (BS.pack name) then
                Case (BS.pack name) w (ensureSel (st:sts) sels) : ensureDefLoop (st:sts) defs
            else
                def : ensureDefLoop (st:sts) defs
        RTL code -> def : ensureDefLoop (st:sts) defs
    Code sp code -> (def : defs) ++ [RTL (BS.pack code)]

ensureSel :: State -> [Sel] -> [Sel]
ensureSel [] [] = []
ensureSel [] sels = sels
ensureSel (st:sts) [] = case st of
    CaseSel sp name (w, num) com -> [Sel num (BS.pack com) (ensureDef sts [])]
    Code sp code -> undefined
ensureSel (st:sts) (sel:sels) = ensureSelLoop (st:sts) (sel:sels)

ensureSelLoop :: State -> [Sel] -> [Sel]
ensureSelLoop [] sels = sels
ensureSelLoop (CaseSel sp name (w, num) com:sts) [] = [Sel num (BS.pack com) []]
ensureSelLoop (Code _ _:sts) [] = undefined
ensureSelLoop (st:sts) (sel:sels) = case st of
    CaseSel sp name (w, num) com -> case sel of
        Sel sel_num sel_com defs -> if num == sel_num then
            Sel sel_num sel_com (ensureDef sts defs) : sels
        else
            sel : ensureSelLoop (st:sts) sels
    Code sp code -> undefined



convert :: State -> [Def] -> [Expr] -> [Def]
convert _ defs [] = defs
convert st defs (e:es) = let
    new_st = switchState st e
    new_defs = ensure new_st defs in
        convert new_st new_defs es

buildCaseFromString :: String -> BS.ByteString
buildCaseFromString s =
              let conv = convert [] [] (parse parseExpr s) in 
                  toLazyByteString (foldr1 (<>) (map (buildCase 0) conv))
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module CaseBuilder ( Def(..), Sel(..), buildCase, convert, switchState ) where

import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.ByteString.Builder
import Parsing (Expr(..))
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
    Code sp code -> st:sts
    CaseSel sp name (w, num) com -> case st of
        Code _ _ -> st:sts
        CaseSel st_sp _ _ _ -> if st_sp < sp then e:st:sts else switchState sts e

{-
getContext :: State -> Def -> Def
getContext st ctx = getContext1 (reverse st) ctx

getContext1 :: State -> Def -> Def
getContext1 [] ctx = ctx
getContext1 (st:sts) ctx = case st of
    CaseSel sp name (w, num) com -> case ctx of
        Case 
-}


convert :: State -> Def -> [Expr] -> Def
convert st ctx [] = undefined
convert st ctx (e:es) = case e of
    CaseSel sp name (w, num) com -> let new_st = switchState st e in
        Case (BS.pack name) w [Sel num (BS.pack com) []]
    Code sp code -> RTL (BS.pack code)

{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module CaseBuilder ( Def(..), Sel(..), Expr(..), buildCase, parseCDF ) where

import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.ByteString.Builder

data Sel = Sel  Int BS.ByteString [Def]
data Def = Case BS.ByteString Int [Sel] |
           RTL  BS.ByteString

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

data Expr = CaseSel Int BS.ByteString (Int, Int) BS.ByteString [Expr] |
            Code Int BS.ByteString
            deriving (Show, Eq)

parseCDF :: String -> Expr
parseCDF x =
    CaseSel 0 "op" (2, 0b11) "[32bit instruction]" []


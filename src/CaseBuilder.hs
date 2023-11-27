{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module CaseBuilder ( Def(..), Sel(..), Expr(..), buildCase, parseCDF ) where

import qualified Data.ByteString.Char8 as BS

data Sel = Sel  Int BS.ByteString [Def]
data Def = Case BS.ByteString Int [Sel] |
           RTL  BS.ByteString

indent :: Int -> BS.ByteString -> BS.ByteString
indent n x = BS.replicate n ' ' `mappend` x

buildCase :: Int -> Def -> [BS.ByteString]
buildCase n x = case x of
    Case switch bits sels ->
        (indent n ("case(" `mappend` switch `mappend` ")\n") : concatMap (buildSel bits (n + 2)) sels)
         ++ [indent (n + 2) "default: id_we = 1'b0;\n", indent n "endcase\n"]
    RTL text -> [indent n (text `mappend` "\n")]

buildSel :: Int -> Int -> Sel -> [BS.ByteString]
buildSel bits n x = case x of
    Sel val comment defs ->
        BS.concat [indent n $ BS.pack (show bits), "'d", BS.pack (show val), ": // ", comment, "\n"] : 
            concatMap (buildCase (n + 2)) defs

data Expr = CaseSel Int BS.ByteString (Int, Int) BS.ByteString [Expr] |
            Code Int BS.ByteString
            deriving (Show, Eq)

parseCDF :: String -> Expr
parseCDF x =
    CaseSel 0 "op" (2, 0b11) "[32bit instruction]" []


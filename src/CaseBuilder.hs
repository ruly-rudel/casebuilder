{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module CaseBuilder ( Def(..), Sel(..), buildCase ) where

data Sel = Sel  Int String [Def]
data Def = Case String Int [Sel] |
           RTL  String

indent :: Int -> String -> String
indent n x = replicate n ' ' ++ x

buildCase :: Int -> Def -> [String]
buildCase n x = case x of
    Case switch bits sels ->
        (indent n ("case(" ++ switch ++ ")\n") : concatMap (buildSel bits (n + 2)) sels)
         ++ [indent (n + 2) "default: id_we = 1'b0;\n", indent n "endcase\n"]
    RTL text -> [indent n (text ++ "\n")]


buildSel :: Int -> Int -> Sel -> [String]
buildSel bits n x = case x of
    Sel val comment defs ->
        indent n (show bits ++ "'d" ++ show val ++ ": // " ++ comment ++ "\n") :
        concatMap (buildCase (n + 2)) defs

{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module CaseBuilder ( Defs(..), Sel(..), buildCase ) where

import qualified Data.Text as T

data Sel = Sel (Int, Int) T.Text [Defs]
data Defs = Case T.Text [Sel] |
            RTL T.Text

indent :: Int -> T.Text -> T.Text
indent n x = T.replicate n " " `T.append` x

buildCase :: Int -> Defs -> [T.Text]
buildCase n x = case x of
    Case switch sels ->
        (indent n ("case(" `T.append` switch `T.append` ")\n") : Prelude.concatMap (buildSel (n + 2)) sels)
         ++ [T.append (indent (n + 2) "default: id_we = 1'b0;\n") $ indent n "endcase\n"]
    RTL text -> [indent n (text `T.append` "\n")]


buildSel :: Int -> Sel -> [T.Text]
buildSel n x = case x of
    Sel (width, val) comment defs ->
        indent n (T.pack (show width) `T.append` "'d" `T.append` T.pack (show val) `T.append` ": // " `T.append` comment `T.append` "\n") :
        Prelude.concatMap (buildCase (n + 2)) defs

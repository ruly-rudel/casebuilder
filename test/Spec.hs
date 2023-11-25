{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import CaseBuilder (Defs(..), Sel(..), buildCase)
import qualified Data.Text.IO as T

main :: IO ()
main = let x = Case "op" 
                [Sel (2, 0b11) "[32bit instruction]" 
                  [Case "opcode" 
                    [Sel (5, 0b00101) "[AUIPC]" 
                        [RTL "rd = PC + imm_u;"]]]] in
    do
        T.putStrLn $ T.concat $ buildCase 0 x
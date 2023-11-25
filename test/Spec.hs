{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import CaseBuilder (Def(..), Sel(..), buildCase)
import qualified Data.Text.IO as T

main :: IO ()
main = let x = Case "op" 2
                [Sel 0b11 "[32bit instruction]" 
                  [Case "opcode" 5 
                    [Sel 0b11000 "[BRANCH]"
                      [Case "funct3" 3
                        [Sel 0b000 "[BEQ]"
                          [RTL "next_pc = RS1 == RS2 ? PC + imm_b : PC + 'h4;"],
                         Sel 0b001 "[BNE]"
                          [RTL "next_pc = RS1 != RS2 ? PC + imm_b : PC + 'h4;"]]],
                     Sel 0b00101 "[AUIPC]" 
                        [RTL "rd = PC + imm_u;"],
                     Sel 0b01101 "[LUI]"
                        [RTL "rd = imm_u;"],
                     Sel 0b00110 "[OP-IMM-32]"
                        [Case "funct3" 3
                          [Sel 0b000 "[ADDIW]"
                             [RTL "rd = s32to64(RS1[31:0] + imm_i[31:0]);"],
                           Sel 0b001 ""
                             [Case "funct7" 7
                               [Sel 0b0000000 "[SLLIW]"
                                  [RTL "rd = s32to64(RS1[31:0] << shamt[4:0]);"]]],
                           Sel 0b101 ""
                             [Case "funct7" 7
                               [Sel 0b0000000 "[SRLIW]"
                                  [RTL "rd = s32to64(RS1[31:0] >> shamt[4:0]);"],
                                Sel 0b0100000 "[SRAIW]"
                                  [RTL "rd = s32to64($signed(RS1[31:0]) >>> shamt[4:0]);"]]]]]]]] in
    do
        putStrLn $ concat $ buildCase 0 x
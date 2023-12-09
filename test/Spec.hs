{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit
import Control.Monad ( void )
import CaseBuilder (Def(..), Sel(..), buildCase, convert, switchState, deepCopy, ensure )
import Parsing (Expr(..), parse, parseExpr)
import Data.ByteString.Builder (toLazyByteString)

main :: IO ()
main = do
  void $ runTestTT $ TestList
    [
      "00_00" ~: "rd = imm_u;\n" ~=? toLazyByteString (buildCase 0 (RTL "rd = imm_u;")),

      "00_01" ~:
        "case(op)\n" <>
        "  2'd3: // [32bit instruction]\n" <>
        "    rd = imm_u;\n" <>
        "  default: id_we = 1'b0;\n" <>
        "endcase\n"
         ~=?
        toLazyByteString (buildCase 0 (Case "op" 2 [Sel 0b11 "[32bit instruction]" [RTL "rd = imm_u;"]])),

      "00_02" ~:
        "case(op)\n" <>
        "  2'd3: // [32bit instruction]\n" <>
        "    case(opcode)\n" <>
        "      5'd24: // [BRANCH]\n" <>
        "        case(funct3)\n" <>
        "          3'd0: // [BEQ]\n" <>
        "            next_pc = RS1 == RS2 ? PC + imm_b : PC + 'h4;\n" <>
        "          3'd1: // [BNE]\n" <>
        "            next_pc = RS1 != RS2 ? PC + imm_b : PC + 'h4;\n" <>
        "          default: id_we = 1'b0;\n" <>
        "        endcase\n" <>
        "      5'd5: // [AUIPC]\n" <>
        "        rd = PC + imm_u;\n" <>
        "      5'd13: // [LUI]\n" <>
        "        rd = imm_u;\n" <>
        "      5'd6: // [OP-IMM-32]\n" <>
        "        case(funct3)\n" <>
        "          3'd0: // [ADDIW]\n" <>
        "            rd = s32to64(RS1[31:0] + imm_i[31:0]);\n" <>
        "          3'd1: // \n" <>
        "            case(funct7)\n" <>
        "              7'd0: // [SLLIW]\n" <>
        "                rd = s32to64(RS1[31:0] << shamt[4:0]);\n" <>
        "              default: id_we = 1'b0;\n" <>
        "            endcase\n" <>
        "          3'd5: // \n" <>
        "            case(funct7)\n" <>
        "              7'd0: // [SRLIW]\n" <>
        "                rd = s32to64(RS1[31:0] >> shamt[4:0]);\n" <>
        "              7'd32: // [SRAIW]\n" <>
        "                rd = s32to64($signed(RS1[31:0]) >>> shamt[4:0]);\n" <>
        "              default: id_we = 1'b0;\n" <>
        "            endcase\n" <>
        "          default: id_we = 1'b0;\n" <>
        "        endcase\n" <>
        "      default: id_we = 1'b0;\n" <>
        "    endcase\n" <>
        "  default: id_we = 1'b0;\n" <>
        "endcase\n"
      ~=?
        toLazyByteString (buildCase 0 (
          Case "op" 2
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
                                  [RTL "rd = s32to64($signed(RS1[31:0]) >>> shamt[4:0]);"]]]]]]]])),

      "00_03" ~: [CaseSel 0 "op" (2, 0b11) "[32bit instruction]"] ~=? parse parseExpr "op 2'b11 [32bit instruction]\n",

      "00_04" ~: [CaseSel 0 "op" (2, 0b11) "[32bit instruction]",
                  CaseSel 2 "opcode" (5, 0b00101) "[AUIPC]",
                  Code 4 "rd = PC + imm_u;"] ~=?
          parse parseExpr "op 2'b11 [32bit instruction]\n  opcode 5'b00101 [AUIPC]\n    rd = PC + imm_u;\n",


      "00_05" ~: [Case "op" 2 [Sel 0b11 "[32bit instruction]" []]] ~=?
                  ensure [CaseSel 0 "op" (2, 0b11) "[32bit instruction]"] [],

      "00_06" ~: [Case "op" 2 [Sel 0b11 "[32bit instruction]" [RTL "rd = PC + imm_u;"]]] ~=?
                  ensure [Code 4 "rd = PC + imm_u;" ,CaseSel 0 "op" (2, 0b11) "[32bit instruction]"] [],

      "00_07" ~: [Case "op" 2 [Sel 0b11 "[32bit instruction]" []]] ~=?
                  ensure [CaseSel 0 "op" (2, 0b11) "[32bit instruction]"] [Case "op" 2 []],

      "00_08" ~: [Case "op" 2 [Sel 0b00 "[16bit instruction]" [], Sel 0b11 "[32bit instruction]" []]] ~=?
                  ensure [CaseSel 0 "op" (2, 0b11) "[32bit instruction]"] [Case "op" 2 [Sel 0b00 "[16bit instruction]" []]],

      "00_09" ~: [Case "op" 2 [Sel 0b11 "[32bit instruction]" []]] ~=?
                  ensure [CaseSel 0 "op" (2, 0b11) "[32bit instruction]"] [Case "op" 2 [Sel 0b11 "[32bit instruction]" []]],

      "00_10" ~: [Case "op" 2 [Sel 0b11 "[32bit instruction]" [
                    Case "opcode" 5 [Sel 0b00101 "[AUIPC]" []]
                  ]]] ~=?
                  ensure [CaseSel 4 "opcode" (5, 0b00101) "[AUIPC]", CaseSel 0 "op" (2, 0b11) "[32bit instruction]"] [Case "op" 2 [Sel 0b11 "[32bit instruction]" []]],

      "00_11" ~: [Case "op" 2 [Sel 0b11 "[32bit instruction]" [
                    Case "opcode" 5 [Sel 0b00101 "[AUIPC]" [RTL "id_we <= 1'b0;"]]
                  ]]] ~=?
                  ensure [Code 8 "id_we <= 1'b0;", CaseSel 4 "opcode" (5, 0b00101) "[AUIPC]", CaseSel 0 "op" (2, 0b11) "[32bit instruction]"] [Case "op" 2 [Sel 0b11 "[32bit instruction]" []]],

      "00_12" ~: [Case "op" 2 [Sel 0b11 "[32bit instruction]" [
                    Case "opcode" 5 [Sel 0b00101 "[AUIPC]" [
                      RTL "id_we <= 1'b0;",
                      RTL "id_we <= 1'b1;"
                    ]]
                  ]]] ~=? 
          let st1 = ensure [Code 8 "id_we <= 1'b0;", CaseSel 4 "opcode" (5, 0b00101) "[AUIPC]", CaseSel 0 "op" (2, 0b11) "[32bit instruction]"] [Case "op" 2 []]
              st2 = ensure [Code 8 "id_we <= 1'b1;", CaseSel 4 "opcode" (5, 0b00101) "[AUIPC]", CaseSel 0 "op" (2, 0b11) "[32bit instruction]"] st1 in st2,

      "00_13" ~: [Case "op" 2 [Sel 0b11 "[32bit instruction]" [
                    Case "opcode" 5 [Sel 0b00101 "[AUIPC]" [
                        RTL "id_we <= 1'b0;",
                        RTL "id_we <= 1'b1;"
                      ], Sel 0b01101 "[LUI]" []                    
                    ]
                  ]]] ~=? 
          let st1 = ensure [Code 8 "id_we <= 1'b0;", CaseSel 4 "opcode" (5, 0b00101) "[AUIPC]", CaseSel 0 "op" (2, 0b11) "[32bit instruction]"] []
              st2 = ensure [Code 8 "id_we <= 1'b1;", CaseSel 4 "opcode" (5, 0b00101) "[AUIPC]", CaseSel 0 "op" (2, 0b11) "[32bit instruction]"] st1
              st3 = ensure [CaseSel 4 "opcode" (5, 0b01101) "[LUI]", CaseSel 0 "op" (2, 0b11) "[32bit instruction]"] st2 in st3,


{-

      "00_08" ~: Case "op" 2 [Sel 0b11 "[32bit instruction]" [
                    Case "opcode" 5 [Sel 0b00101 "[AUIPC]" [],
                                     Sel 0b01101 "[LUI]" []]
                  ]] ~=?
                  deepCopy (Case "op" 2 [Sel 0b11 "[32bit instruction]" [
                    Case "opcode" 5 [Sel 0b00101 "[AUIPC]" [],
                                     Sel 0b01101 "[LUI]" []]
                  ]])

-}

{-
      "00_10" ~: [Case "op" 2 [Sel 0b11 "[32bit instruction]" []]]
                   ~=?
                  ensure [CaseSel 0 "op" (2, 0b11) "[32bit instruction]"] []
-}

{-
      "00_05" ~: Case "op" 2 [Sel 0b11 "[32bit instruction]" []] ~=?
          convert [] (Case "root" 0 []) [CaseSel 0 "op" (2, 0b11) "[32bit instruction]"],

      "00_06" ~: Case "op" 2 [Sel 0b11 "[32bit instruction]" [RTL "rd = imm_u;"]] ~=?
          convert [] (Case "root" 0 []) [CaseSel 0 "op" (2, 0b11) "[32bit instruction]",
                   Code 4 "rd = imm_u;"]
-}

      "01_00" ~: [CaseSel 4 "opcode" (5, 5) "[AUIPC]", CaseSel 0 "op" (2, 3) "[32bit instruction]"] ~=?
                   let ss1 = switchState [] (CaseSel 0 "op" (2, 0b11) "[32bit instruction]")
                       ss2 = switchState ss1 (CaseSel 4 "opcode" (5, 0b00101) "[AUIPC]") 
                       ss3 = switchState ss2 (CaseSel 4 "opcode" (5, 0b01101 ) "[LUI]") in ss2,

      "01_01" ~: [CaseSel 4 "opcode" (5, 13) "[LUI]", CaseSel 0 "op" (2, 3) "[32bit instruction]"] ~=?
                   let ss1 = switchState [] (CaseSel 0 "op" (2, 0b11) "[32bit instruction]")
                       ss2 = switchState ss1 (CaseSel 4 "opcode" (5, 0b00101) "[AUIPC]") 
                       ss3 = switchState ss2 (CaseSel 4 "opcode" (5, 0b01101 ) "[LUI]") in ss3,

      "01_02" ~: [Code 8 "id_we <= 1'b0;", CaseSel 4 "opcode" (5, 5) "[AUIPC]", CaseSel 0 "op" (2, 3) "[32bit instruction]"] ~=?
                   let ss1 = switchState [] (CaseSel 0 "op" (2, 0b11) "[32bit instruction]")
                       ss2 = switchState ss1 (CaseSel 4 "opcode" (5, 0b00101) "[AUIPC]") 
                       ss3 = switchState ss2 (Code 8 "id_we <= 1'b0;") in ss3



{-
      "00_07" ~: [CaseSel 4 "opcode" (5,13) "[LUI]",CaseSel 0 "op" (2,3) "[32bit instruction]"] ~=?
                   let ss1 = switchState [] (CaseSel 0 "op" (2, 0b11) "[32bit instruction]")
                       ss2 = switchState ss1 (CaseSel 4 "opcode" (5, 0b00101) "[AUIPC]") 
                       ss3 = switchState ss2 (CaseSel 4 "opcode" (5, 0b01101 ) "[LUI]") in ss3
-}
    ]

{-
  let x = Case "op" 2
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
-}
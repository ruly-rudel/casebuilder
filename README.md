# casebuilder

stdinから下記形式のcase定義を入力し、パースして巨大なcase文を1個作り、stdoutに下記形式で出力する。

### Example 1
input text is:

```
op 2'b11 [32bit instruction]
    opcode 5'b11_000 [BRANCH]
        funct3 3'b000 [BEQ]
            next_pc = RS1 == RS2 ? PC + imm_b : PC + 'h4;
        funct3 3'b001 [BNE]
            next_pc = RS1 != RS2 ? PC + imm_b : PC + 'h4;
    opcode 5'b00_101 [AUIPC]
        rd = PC + imm_u;
    opcode 5'b01_101 [LUI]
        rd = imm_u;
    opcode 5'b00_110 [OP-IMM-32]
        funct3 3'b000 [ADDIW]
            rd = s32to64(RS1[31:0] + imm_i[31:0]);
        funct3 3'b001
            funct7 7'b000_0000 [SLLIW]
                rd = s32to64(RS1[31:0] << shamt[4:0]);
        funct3 3'b101
            funct7 7'b000_0000 [SRLIW]
                rd = s32to64(RS1[31:0] >> shamt[4:0]);
            funct7 7'b010_0000 [SRAIW]
                rd = s32to64($signed(RS1[31:0]) >>> shamt[4:0]);
```

output text is:
```
case (op)
  2'b11: begin // [32bit instruction]
    case(opcode)
      5'b11_000: begin // [BRANCH]
        case (funct3)
          3'b000: // [BEQ]
            next_pc = RS1 == RS2 ? PC + imm_b : PC + 'h4;
          3'b001: // [BNE]
            next_pc = RS1 != RS2 ? PC + imm_b : PC + 'h4;
          default: id_we = 1'b0;
        endcase
      end
      5'b00_101: //[AUIPC]
        rd = PC + imm_u;
      5'b01_101: // [LUI]
        rd = imm_u;
      5'b00_110: begin // [OP-IMM-32]
        case(funct3)
          3'b000: // [ADDIW]
              rd = s32to64(RS1[31:0] + imm_i[31:0]);
          3'b001: begin
              case(funct7)
                7'b000_0000: // [SLLIW]
                  rd = s32to64(RS1[31:0] << shamt[4:0]);
                default: id_we = 1'b0;
              endcase
          end
          3'b101: begin
              case(funct7)
                7'b000_0000: // [SRLIW]
                  rd = s32to64(RS1[31:0] >> shamt[4:0]);
                7'b010_0000: // [SRAIW]
                  rd = s32to64($signed(RS1[31:0]) >>> shamt[4:0]);
                default: id_we = 1'b0;
              endcase
          end
          default: id_we = 1'b0;
        endcase
      end
      default: id_we = 1'b0;
    endcase
  end
  default: id_we = 1'b0;
endcase
```

### Example 2
input text is:

```
op 2'b11 [32bit instruction]
    opcode 5'b01_100 [OP]
        funct3 3'b000
            funct7 7'b000_0000 [ADD]
                rd = RS1 + RS2;
            funct7 7'b000_0001 [MUL]
                rd = $bits(rd)'(RS1 * RS2);
            funct7 7'b010_0000 [SUB]
                rd = RS1 - RS2;
        funct3 3'b001
            funct7 7'b000_0000 [SLL]
                rd = RS1 << RS2[5:0];
            funct7 7'b000_0001 [MULH]
                rd = ($signed(RS1) * $signed(RS2))[`XLEN*2-1:`XLEN];
```

output text is:

```
case(op)
  2'b11: begin
    case(opcode)
      5'b01_100: // [OP]
        case (funct3)
          3'b000: begin
            case (funct7)
              7'b000_0000: // [ADD]
                rd = RS1 + RS2;
              7'b000_0001: // [MUL]
                rd = $bits(rd)'(RS1 * RS2);
              7'b010_0000 // [SUB]
                rd = RS1 - RS2;
              default: id_we = 1'b0;
            endcase
          end
          3'b001: begin
            case (funct7)
              7'b000_0000: // [SLL]
                rd = RS1 << RS2[5:0];
              7'b000_0001: // [MULH]
                rd = ($signed(RS1) * $signed(RS2))[`XLEN*2-1:`XLEN];
              default: id_we = 1'b0;
            endcase
          end
          default: id_we = 1'b0;
        endcase
      default: id_we = 1'b0;
    endcase
  end
  default: id_we = 1'b0;
endcase
```


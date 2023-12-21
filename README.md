# casebuilder

stdinから下記ascii形式のcase定義を入力し、パースして巨大なcase文を1個作り、stdoutに下記形式で出力します。

## 入力形式
入力は行単位(CRLF)で区切られます。入力には2種類存在します。case文とrtl文がそれです。

### case文
case文は以下の3つの構成要素(name, value, comment)からなります。構成要素はスペースで区切られます。コメントのみ、途中のスペースを許容して、行末（CRLF）までコメントとします。
```
    opcode 5'b11_000 [BRANCH]
    ------ --------- --------
     name    value   comment
```

### rtl文
rtl文はひとつの構成要素(rtl)からなります。case文に相当しないものはすべてrtl文です。
```
            next_pc = RS1 == RS2 ? PC + imm_b : PC + 'h4;
            ---------------------------------------------
                               rtl
```

### インデント
case文とrtl文にはそれぞれインデントが存在します。インデントは行頭からはじまる任意個数のスペースからなります。タブは含まれません。
インデントにより、case文とrtl文のネストを表します。

## 出力形式
出力はverilogのcase文のネストになります。case文で分岐する値を示し、その子要素が実際に実行されるrtlとなります。

### 出力例1
以下を入力例1とします。

```
op 2'b11 [32bit instruction]
    opcode 5'b00_101 [AUIPC]
        rd = PC + imm_u;
        id_we = 1'b1;
    opcode 5'b01_101 [LUI]
        rd = imm_u;

```
1行目のcase文のインデントは0、2行目と5行目のcase文のインデントは4、3行目と4行目と6行目のrtl文のインデントは8です。

前の行と比較して、インデントは増える場合と減る場合、変わらない場合が存在します。
増える場合は、ネストが深くなります。例えば、1行目はルートのcase文(opの値で分岐)を示し、2行目は1行目のcase文のop値2'b11の内側のcase文(opcodeの値で分岐)を示します。
減る場合は、ネストを閉じます。例えば、4行目のrtl文から5行目のcase文に対し、ネストを1段閉じます。ネストは複数段同時に閉じることができます。
同じ場合は、ネストの同じ個所に複数の要素を挿入します。例えば、3行目と4行目のrtl文は、同じ2行目のcase文の子要素となります。

同じインデントレベルのcase文のnameは、ネストが閉じられない限り同一である必要があります。
例えば、2行目と5行目のcase文はインデントが同じ4で、ネストは閉じられていないため、同じ名前（opcode）である必要があります。

上記入力例1の入力に対する出力は以下のようになります。

```
case (op)
  2'b11: // [32bit instruction]
    case(opcode)
      5'b00_101: //[AUIPC]
        rd = PC + imm_u;
        id_we = 1'b1;
      5'b01_101: // [LUI]
        rd = imm_u;
      default: id_we = 1'b0;
    endcase
  default: id_we = 1'b0;
endcase
```

入力1行目のcase文で、opの値による分岐をはじめます。最初の分岐なので、まず「case ($name)」を出力し、次にopの値の分岐「$value: // $comment」を出力します。出力のインデントは固定で2です。

```
case (op)
  2'b11: // [32bit instruction]
```

入力2行目のcase文で、opcodeの値による分岐をはじめます。最初の分岐なので、ます「case ($name)」を出力し、次にopcodeの値の分岐「$value : // $comment」を出力します。
```
    case(opcode)
      5'b00_101: //[AUIPC]
```

入力3行目と4行目のrtl文は、より深いインデント(インデント8)ですので、直前のcase文の分岐の子要素としてrtlが挿入されます。(todo: 複数行のrtlに対してbegin/endでくくる)

```
      5'b00_101: //[AUIPC]
        rd = PC + imm_u;
        id_we = 1'b1;
```

入力5行目のcase文は、より浅いインデント(インデント4)ですので、直前のrtl文のネストを閉じ、次のcase文のネスト(opcodeの値の分岐)をはじめます。
```
        id_we = 1'b1;
      5'b01_101: // [LUI]
```

6行目のrtl文はより深いインデント(インデント8)ですので、直前のcase文の分岐の子要素としてrtlが挿入されます。

```
      5'b01_101: // [LUI]
        rd = imm_u;
```

テキストの終わりでは、すべてのネストを閉じます。まず、インデント4のネストを閉じます。
```
      default: id_we = 1'b0;
    endcase
```

次に、インデント0のネストを閉じます。
```
  default: id_we = 1'b0;
endcase
```

### 出力例2

入力例2を以下とします。
```
op 2'b11 [32bit instruction]
    opcode 5'b00_101 [AUIPC]
        rd = PC + imm_u;
op 2'b11 [32bit instruction]
    opcode 5'b01_101 [LUI]
        rd = imm_u;

```

ネストは融合させることができます。つまり、一度ネストを閉じた後に、再び同じ$name, $valueでネストを開きなおすと、そのネストの最後尾に要素が挿入されます。
例2の場合、4行目の先頭までで一度ネストが閉じるので、この時点での（仮の）出力は以下のようになります。

```
case (op)
  2'b11: // [32bit instruction]
    case(opcode)
      5'b00_101: //[AUIPC]
        rd = PC + imm_u;
      default: id_we = 1'b0;
    endcase
  default: id_we = 1'b0;
endcase
```

次に、4行目のcase文は1行目のcase文と$name, $valueが一致します。そのため、case (op), 2'b11: 以下のネストに値が挿入されます。この時、入力のインデントはどちらも0であることに注意してください。融合する場合は、必ずインデントが同じ数字になっていなければなりません。

```
case (op)
  2'b11: // [32bit instruction]
    case(opcode)               +
      5'b00_101: //[AUIPC]     |
        rd = PC + imm_u;       | これらの行のどこかに後続のcase文とrtl文が挿入される
      default: id_we = 1'b0;   |
    endcase                    +
  default: id_we = 1'b0;
endcase
```

5行目のcase文は2行目のcase文と$nameが一致し、$valueが一致しません。そのため、case(opcode)以下のネストの最後尾にcase文とrtl文が挿入されます。
```
case (op)
  2'b11: // [32bit instruction]
    case(opcode)
      5'b00_101: //[AUIPC]
        rd = PC + imm_u;
        id_we = 1'b1;
      5'b01_101: // [LUI]      +
        rd = imm_u;            + case(opcode)の子要素としてcase文とrtl文を挿入
      default: id_we = 1'b0;
    endcase
  default: id_we = 1'b0;
endcase
```


## もう少し大きい例
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


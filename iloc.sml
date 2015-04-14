
structure Iloc = struct

datatype opcode =
     OP_ADD
   | OP_ADDI
   | OP_DIV
   | OP_MULT
   | OP_SUB
   | OP_SUBI
   | OP_AND
   | OP_OR
   | OP_XORI
   | OP_COMP
   | OP_COMPI
   | OP_CBREQ
   | OP_CBRGE
   | OP_CBRGT
   | OP_CBRLE
   | OP_CBRLT
   | OP_CBRNE
   | OP_JUMPI
   | OP_LOADI
   | OP_LOADAI
   | OP_LOADGLOBAL
   | OP_LOADINARGUMENT
   | OP_LOADRET
   | OP_COMPUTEFORMALADDRESS
   | OP_RESTOREFORMAL
   | OP_COMPUTEGLOBALADDRESS
   | OP_STOREAI
   | OP_STOREGLOBAL
   | OP_STOREINARGUMENT
   | OP_STOREOUTARGUMENT
   | OP_STORERET
   | OP_CALL
   | OP_RET
   | OP_NEW
   | OP_DEL
   | OP_PRINT
   | OP_PRINTLN
   | OP_READ
   | OP_MOV
   | OP_MOVEQ
   | OP_MOVGE
   | OP_MOVGT
   | OP_MOVLE
   | OP_MOVLT
   | OP_MOVNE


datatype instruction =
     INS_RRR of {opcode: opcode, dest: int, r1: int, r2: int}
   | INS_RIR of {opcode: opcode, dest: int, r1: int, immed: int}
   | INS_RRC of {opcode: opcode, r1: int, r2: int}
   | INS_RIC of {opcode: opcode, r1: int, immed: int}
   | INS_CLL of {opcode: opcode, l1: string, l2: string}
   | INS_SIR of {opcode: opcode, id: string, immed: int, r1: int}
   | INS_NEW of {opcode: opcode, dest: int, id: string, fields: string list}
   | INS_IR  of {opcode: opcode, dest: int, immed: int}
   | INS_SR  of {opcode: opcode, id: string, r1: int}
   | INS_SI  of {opcode: opcode, id: string, immed: int}
   | INS_L   of {opcode: opcode, l1: string}
   | INS_R   of {opcode: opcode, r1: int}
   | INS_X   of {opcode: opcode}


type basicBlock = string * instruction list

end

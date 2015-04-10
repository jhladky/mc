(*put the rest of the opcodes in later*)
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
   | OP_CBRGE
   | OP_CBRGT
   | OP_CBRLE
   | OP_CBRLT
   | OP_CBRNE
   | OP_JUMPI
;

(*Fill out the rest of these later as well*)
datatype instruction =
     INS_RR of {opcode: opcode, r1: int, r2: int, dest: int}
   | INS_RI of {opcode: opcode, r1: int, immed: int, dest: int}
   | INS_RLL of {opcode: opcode, cc: int, l1: string, l2: string}
   | INS_L of {opcode: opcode, label: string}
   | INS_I of {opcode: opcode, reg: int}
;

datatype basicBlock =
     BB of {prev: basicBlock list ref, next: basicBlock list ref,
            body: instruction list ref, label: string}
;

(* datatype cfg = *)
(*      CFG of {entry: basicBlock, exit: basicBlock} *)
(* ; *)

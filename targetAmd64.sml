structure TargetAmd64 = struct

datatype opcode =
     OP_ADDQ
   | OP_SUBQ
   | OP_MULQ
   | OP_IMULQ
   | OP_IDIVQ
   | OP_ANDQ
   | OP_ORQ
   | OP_XORQ
   | OP_CMP
   | OP_JMP
   | OP_JE
   | OP_JG
   | OP_JGE
   | OP_JL
   | OP_JLE
   | OP_JNE
   | OP_MOVQ
   | OP_CALL
   | OP_RET
   | OP_CMOVEQ
   | OP_CMOVGQ
   | OP_CMOVLQ
   | OP_CMOVLEQ
   | OP_CMOVNEQ
   | OP_SHRQ


datatype directive =
     DVE_GLOBAL
   | DVE_TEXT
   | DVE_SIZE
   | DVE_STRING
   | DVE_SECTION


datatype register =
     REG_RAX
   | REG_RBX
   | REG_RCX
   | REG_RDX
   | REG_RSI
   | REG_RDI
   | REG_RBP
   | REG_RSP
   | REG_N of int


datatype instruction =
     INS_DVE of {dve: directive, arg: string}
   | INS_LABEL of string
   | INS_RR of {opcode: opcode, r1: register, r2: register}
   | INS_IR of {opcode: opcode, immed: int, r2: register}
   | INS_IRR of {opcode: opcode, immed: int, r1: register, r2: register}
   | INS_L of {opcode: opcode, label: string}
   | INS_DBOSR of {opcode: opcode}
   | INS_RDBOS of {opcode: opcode}
   | INS_IDBOS of {opcode: opcode}
   | INS_X of {opcode: opcode}


fun opToStr OP_ADDQ = "addq"
   | opToStr OP_SUBQ = "subq"
   | opToStr OP_MULQ = "mulq"
   | opToStr OP_IMULQ = "imulq"
   | opToStr OP_IDIVQ = "idivq"
   | opToStr OP_ANDQ = "andq"
   | opToStr OP_ORQ = "orq"
   | opToStr OP_XORQ = "xorq"
   | opToStr OP_CMP = "cmp"
   | opToStr OP_JMP = "jmp"
   | opToStr OP_JE = "je"
   | opToStr OP_JG = "jg"
   | opToStr OP_JGE = "jge"
   | opToStr OP_JL = "jl"
   | opToStr OP_JLE = "jle"
   | opToStr OP_JNE = "jne"
   | opToStr OP_MOVQ = "movq"
   | opToStr OP_CALL = "call"
   | opToStr OP_RET = "ret"
   | opToStr OP_CMOVEQ = "cmoveq"
   | opToStr OP_CMOVGQ = "cmovgq"
   | opToStr OP_CMOVLQ = "cmovlq"
   | opToStr OP_CMOVLEQ = "cmovleq"
   | opToStr OP_CMOVNEQ = "cmovneq"
   | opToStr OP_SHRQ = "shrq"


fun dveToStr DVE_GLOBAL = ".global"
  | dveToStr DVE_TEXT = ".text"
  | dveToStr DVE_SIZE = ".size"
  | dveToStr DVE_STRING  = ".string"
  | dveToStr DVE_SECTION = ".section"


fun regToStr REG_RAX = "%rax"
  | regToStr REG_RBX = "%rbx"
  | regToStr REG_RCX = "%rcx"
  | regToStr REG_RDX = "%rdx"
  | regToStr REG_RSI = "%rsi"
  | regToStr REG_RDI = "%rdi"
  | regToStr REG_RBP = "%rbp"
  | regToStr REG_RSP = "%rsp"
  | regToStr (REG_N n) = "%r" ^ (Int.toString n)


fun toString (INS_DVE {dve=dve, arg=arg}) =
    (dveToStr dve) ^ arg
  | toString (INS_LABEL l) =
    l ^ ":"
  | toString (INS_RR {opcode=opcode, r1=r1, r2=r2}) =
    (opToStr opcode) ^ " " ^ (regToStr r1) ^ ", " ^ (regToStr r2)
  | toString (INS_IR {opcode=opcode, immed=immed, r2=r2}) =
    (opToStr opcode) ^ " $" ^ (Int.toString immed) ^ ", " ^ (regToStr r2)
  | toString (INS_IRR {opcode=opcode, immed=immed, r1=r1, r2=r2}) =
    (opToStr opcode) ^ " $" ^ (Int.toString immed) ^ ", " ^ (regToStr r1) ^
    ", " ^ (regToStr r2)
  | toString (INS_L {opcode=opcode, label=label}) =
    (opToStr opcode) ^ " " ^ label
  | toString (INS_DBOSR {opcode=opcode}) =
    (opToStr opcode) ^ " "
  | toString (INS_RDBOS {opcode=opcode}) =
    (opToStr opcode) ^ " "
  | toString (INS_IDBOS {opcode=opcode}) =
    (opToStr opcode) ^ " "
  | toString (INS_X {opcode=opcode}) =
    opToStr opcode

end

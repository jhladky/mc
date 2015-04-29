structure TargetAmd64 = struct

datatype opcode =
     OP_ADDQ
   | OP_SUBQ
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
     INS_RR of {opcode: opcode, r1: register, r2: register}
   | INS_IR of {opcode: opcode, immed: int, r2: register}
   | INS_IRR of {opcode: opcode, immed: int, r1: register, r2: register}
   | INS_L of {opcode: opcode, label: string}
   (* | INS_DBOSR of {opcode: opcode} *)
   (* | INS_RDBOS of {opcode: opcode} *)
   (* | INS_IDBOS of {opcode: opcode} *)
   | INS_X of {opcode: opcode}


type basicBlock = string * instruction list


type function = string * basicBlock list


datatype program = PROGRAM of {text: function list, data: string list}


fun opToStr OP_ADDQ = "addq"
   | opToStr OP_SUBQ = "subq"
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


fun regToStr REG_RAX = "%rax"
  | regToStr REG_RBX = "%rbx"
  | regToStr REG_RCX = "%rcx"
  | regToStr REG_RDX = "%rdx"
  | regToStr REG_RSI = "%rsi"
  | regToStr REG_RDI = "%rdi"
  | regToStr REG_RBP = "%rbp"
  | regToStr REG_RSP = "%rsp"
  | regToStr (REG_N n) = "%r" ^ (Int.toString n)


fun insToStr (INS_RR {opcode=opcode, r1=r1, r2=r2}) =
    (opToStr opcode) ^ " " ^ (regToStr r1) ^ ", " ^ (regToStr r2)
  | insToStr (INS_IR {opcode=opcode, immed=immed, r2=r2}) =
    (opToStr opcode) ^ " $" ^ (Int.toString immed) ^ ", " ^ (regToStr r2)
  | insToStr (INS_IRR {opcode=opcode, immed=immed, r1=r1, r2=r2}) =
    (opToStr opcode) ^ " $" ^ (Int.toString immed) ^ ", " ^ (regToStr r1) ^
    ", " ^ (regToStr r2)
  | insToStr (INS_L {opcode=opcode, label=label}) =
    (opToStr opcode) ^ " " ^ label
  (* | insToStr (INS_DBOSR {opcode=opcode}) = *)
  (*   (opToStr opcode) ^ " " *)
  (* | insToStr (INS_RDBOS {opcode=opcode}) = *)
  (*   (opToStr opcode) ^ " " *)
  (* | insToStr (INS_IDBOS {opcode=opcode}) = *)
  (*   (opToStr opcode) ^ " " *)
  | insToStr (INS_X {opcode=opcode}) =
    opToStr opcode


fun bbToStr (l, L) =
    l ^ ":\n" ^ (foldr (fn (ins, s) => "\t" ^ (insToStr ins) ^ "\n" ^ s) "" L)


fun funcToStr (id, body) =
    "\t.globl " ^ id ^ "\n\t.type " ^ id ^ ", @function\n" ^
    (foldr (fn (bb, s) => (bbToStr bb) ^ s) "" body) ^
    "\t.size " ^ id ^ ", .-" ^ id ^ "\n\n"


(*text is the functions, data is the globals*)
fun programToStr (PROGRAM {text=text, data=data}) =
    "\t.section text\n" ^
    (foldr (fn (func, s) => (funcToStr func) ^ s) "" text) ^
    "\t.section data\n" ^
    (foldr (fn (id, s) => "\t.comm " ^ id ^ ", 8, 8\n" ^ s) "" data) ^
    "\t.section rodata\nL__pstr__:\n\t.asciz \"%s\"\n" ^
    "L__pstre__:\n\t.asciz \"%s\\n\"\n"


end

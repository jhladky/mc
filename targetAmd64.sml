structure TargetAmd64 = struct

datatype opcode =
     OP_ADDQ
   | OP_SUBQ
   | OP_IMULQ
   | OP_IDIVQ
   | OP_ANDQ
   | OP_ORQ
   | OP_XORQ
   | OP_PUSHQ
   | OP_POPQ
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
   | OP_CMOVGEQ
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
   | INS_SR of {opcode: opcode, id: string, dest: register}
   | INS_GR of {opcode: opcode, global: string, dest: register}
   | INS_RG of {opcode: opcode, r1: register, global: string}
   | INS_MR of {opcode: opcode, immed: int, base: register, offset: register,
                scalar: int, dest: register}
   | INS_RM of {opcode: opcode, r1: register, immed: int, base: register,
                offset: register, scalar: int}
   | INS_R of {opcode: opcode, r1: register}
   | INS_L of {opcode: opcode, label: string}
   | INS_X of {opcode: opcode}


type basic_block = string * instruction list


type function = string * basic_block list


datatype program = PROGRAM of {text: function list, data: string list}


val opToStr =
 fn OP_ADDQ      => "addq "
  | OP_SUBQ      => "subq "
  | OP_IMULQ     => "imulq "
  | OP_IDIVQ     => "idivq "
  | OP_ANDQ      => "andq "
  | OP_ORQ       => "orq "
  | OP_XORQ      => "xorq "
  | OP_PUSHQ     => "pushq "
  | OP_POPQ      => "popq "
  | OP_CMP       => "cmp "
  | OP_JMP       => "jmp "
  | OP_JE        => "je "
  | OP_JG        => "jg "
  | OP_JGE       => "jge "
  | OP_JL        => "jl "
  | OP_JLE       => "jle "
  | OP_JNE       => "jne "
  | OP_MOVQ      => "movq "
  | OP_CALL      => "call "
  | OP_RET       => "ret "
  | OP_CMOVEQ    => "cmoveq "
  | OP_CMOVGQ    => "cmovgq "
  | OP_CMOVGEQ   => "cmovgeq "
  | OP_CMOVLQ    => "cmovlq "
  | OP_CMOVLEQ   => "cmovleq "
  | OP_CMOVNEQ   => "cmovneq "
  | OP_SHRQ      => "shrq "


val regToStr =
 fn REG_RAX      => "%rax"
  | REG_RBX      => "%rbx"
  | REG_RCX      => "%rcx"
  | REG_RDX      => "%rdx"
  | REG_RSI      => "%rsi"
  | REG_RDI      => "%rdi"
  | REG_RBP      => "%rbp"
  | REG_RSP      => "%rsp"
  | REG_N n      => "%r" ^ Int.toString n


val insToStr =
 fn INS_RR {opcode=opc, r1=r1, r2=r2} =>
    opToStr opc ^ regToStr r1 ^ ", " ^ regToStr r2
  | INS_IR {opcode=opc, immed=immed, r2=r2} =>
    opToStr opc ^ "$" ^ Int.toString immed ^ ", " ^ regToStr r2
  | INS_GR {opcode=opc, global=global, dest=dest} =>
    opToStr opc ^ global ^ "(%rip), " ^ regToStr dest
  | INS_SR {opcode=opc, id=id, dest=dest} =>
    opToStr opc ^ "$" ^ id ^ ", " ^ regToStr dest
  | INS_RG {opcode=opc, r1=r1, global=global} =>
    opToStr opc ^ regToStr r1 ^ ", " ^ global ^ "(%rip)"
  | INS_R {opcode=opc, r1=r1} => opToStr opc ^ regToStr r1
  | INS_L {opcode=opc, label=label} => opToStr opc ^ label
  | INS_MR {opcode=opc, immed=i, base=base, offset=offset, scalar=s, dest=d} =>
    opToStr opc ^ Int.toString i ^ "(" ^ regToStr base ^ ", " ^
    regToStr offset ^ ", " ^ Int.toString s ^ "), " ^ regToStr d
  | INS_RM {opcode=opc, r1=r1, immed=i, base=base, offset=offset, scalar=s} =>
    opToStr opc ^ regToStr r1 ^ ", " ^  Int.toString i ^ "(" ^ regToStr base ^
    ", " ^ regToStr offset ^ ", " ^ Int.toString s ^ ")"
  | INS_X {opcode=opc} => opToStr opc


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
    "\t.section rodata\nL__s__:\n\t.asciz \"%d\"\n" ^
    "L__sn__:\n\t.asciz \"%d\\n\"\n"


end

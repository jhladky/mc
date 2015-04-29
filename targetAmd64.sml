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
   (* | INS_IRR of {opcode: opcode, immed: int, r1: register, r2: register} *)
   | INS_GR of {opcode: opcode, global: string, dest: register}
   | INS_RG of {opcode: opcode, r1: register, global: string}
   | INS_MR of {opcode: opcode, immed: int, base: register, offset: register,
                scalar: int, dest: register}
   | INS_RM of {opcode: opcode, r1: register, immed: int, base: register,
                offset: register, scalar: int}
   | INS_L of {opcode: opcode, label: string}
   | INS_X of {opcode: opcode}


type basicBlock = string * instruction list


type function = string * basicBlock list


datatype program = PROGRAM of {text: function list, data: string list}


val opToStr =
 fn OP_ADDQ      => "addq "
  | OP_SUBQ      => "subq "
  | OP_IMULQ     => "imulq "
  | OP_IDIVQ     => "idivq "
  | OP_ANDQ      => "andq "
  | OP_ORQ       => "orq "
  | OP_XORQ      => "xorq "
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
  (* | INS_IRR {opcode=opc, immed=immed, r1=r1, r2=r2} => *)
  (*   opToStr opc ^ "$" ^ Int.toString immed ^ ", " ^ regToStr r1 ^ ", " ^ *)
  (*   regToStr r2 *)
  | INS_GR {opcode=opc, global=global, dest=dest} =>
    opToStr opc ^ global ^ "(%rip), " ^ regToStr dest
  | INS_RG {opcode=opc, r1=r1, global=global} =>
    opToStr opc ^ regToStr r1 ^ ", " ^ global ^ "(%rip)"
  | INS_L {opcode=opc, label=label} => opToStr opc ^ label
  | INS_MR {opcode=opc, immed=immed, base=base, offset=offset, scalar=scalar,
            dest=dest} =>
    opToStr opc ^ Int.toString immed ^ "(" ^ regToStr base ^ ", " ^
    regToStr offset ^ ", " ^ Int.toString scalar ^ "), " ^ regToStr dest
  | INS_RM {opcode=opc, r1=r1, immed=immed, base=base, offset=offset,
            scalar=scalar} =>
    opToStr opc ^ regToStr r1 ^ ", " ^  Int.toString immed ^ "(" ^
    regToStr base ^ ", " ^ regToStr offset ^ ", " ^ Int.toString scalar ^ ")"
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
    "\t.section rodata\nL__pstr__:\n\t.asciz \"%s\"\n" ^
    "L__pstre__:\n\t.asciz \"%s\\n\"\n"


end

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
   | OP_LEAQ
   | OP_CALL
   | OP_RET
   | OP_CMOVE
   | OP_CMOVG
   | OP_CMOVGE
   | OP_CMOVL
   | OP_CMOVLE
   | OP_CMOVNE
   | OP_SARQ


datatype register =
     REG_RAX
   | REG_RBX
   | REG_RCX
   | REG_RDX
   | REG_RSI
   | REG_RDI
   | REG_RBP
   | REG_RSP
   | REG_8
   | REG_9
   | REG_10
   | REG_11
   | REG_12
   | REG_13
   | REG_14
   | REG_15
   | REG_V of int


datatype instruction =
     INS_RR of {opcode: opcode, r1: register, r2: register}
   | INS_IR of {opcode: opcode, immed: int, r2: register}
   | INS_SR of {opcode: opcode, id: string, dest: register}
   | INS_GR of {opcode: opcode, global: string, dest: register}
   | INS_RG of {opcode: opcode, r1: register, global: string}
   | INS_MR of {opcode: opcode, immed: int, base: register, dest: register,
                offset: (register * int) option}
   | INS_RM of {opcode: opcode, r1: register, immed: int, base: register,
                offset: (register * int ) option}
   | INS_R of {opcode: opcode, r1: register}
   | INS_L of {opcode: opcode, label: string}
   | INS_X of {opcode: opcode}


type basic_block = string * instruction list


type function = string * basic_block Cfg.cfg


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
  | OP_LEAQ      => "leaq "
  | OP_CALL      => "call "
  | OP_RET       => "ret "
  | OP_CMOVE     => "cmove "
  | OP_CMOVG     => "cmovg "
  | OP_CMOVGE    => "cmovge "
  | OP_CMOVL     => "cmovl "
  | OP_CMOVLE    => "cmovle "
  | OP_CMOVNE    => "cmovne "
  | OP_SARQ      => "sarq "


val regToStr =
 fn REG_RAX      => "%rax"
  | REG_RBX      => "%rbx"
  | REG_RCX      => "%rcx"
  | REG_RDX      => "%rdx"
  | REG_RSI      => "%rsi"
  | REG_RDI      => "%rdi"
  | REG_RBP      => "%rbp"
  | REG_RSP      => "%rsp"
  | REG_8        => "%r8"
  | REG_9        => "%r9"
  | REG_10       => "%r10"
  | REG_11       => "%r11"
  | REG_12       => "%r12"
  | REG_13       => "%r13"
  | REG_14       => "%r14"
  | REG_15       => "%r15"
  (* Put a v in to make sure we remember these are _virtual_. *)
  | REG_V n      => "%rv" ^ Int.toString n


fun nameToStr platform id = if platform = Util.OS_X then "_" ^ id else id


val insToStr =
 fn platform =>
    (fn INS_RR {opcode=opc, r1=r1, r2=r2} =>
        opToStr opc ^ regToStr r1 ^ ", " ^ regToStr r2
    | INS_IR {opcode=opc, immed=immed, r2=r2} =>
      opToStr opc ^ "$" ^ Util.iToS immed ^ ", " ^ regToStr r2
    | INS_GR {opcode=opc, global=g, dest=dest} =>
      opToStr opc ^ nameToStr platform g ^ "(%rip), " ^ regToStr dest
    | INS_SR {opcode=opc, id=id, dest=dest} =>
      if platform = Util.OS_X
      then "leaq " ^ (if hd (explode id) <> #"L" then nameToStr platform id
                      else id) ^ "(%rip), " ^ regToStr dest
      else opToStr opc ^ "$" ^ id ^ ", " ^ regToStr dest
    | INS_RG {opcode=opc, r1=r1, global=g} =>
      opToStr opc ^ regToStr r1 ^ ", " ^ nameToStr platform g ^ "(%rip)"
    | INS_R {opcode=opc, r1=r1} => opToStr opc ^ regToStr r1
    | INS_L {opcode=opc, label=label} =>
      if opc = OP_CALL then opToStr opc ^ nameToStr platform label
      else opToStr opc ^ label
    | INS_MR {opcode=opc, immed=i, base=base, offset=offset, dest=d} =>
      opToStr opc ^ Util.iToS i ^ "(" ^ regToStr base ^
      (case offset of
           SOME (reg, s) => ", " ^ regToStr reg ^ ", " ^ Util.iToS s
         | NONE => "")
      ^ "), " ^ regToStr d
    | INS_RM {opcode=opc, r1=r1, immed=i, base=base, offset=offset} =>
      opToStr opc ^ regToStr r1 ^ ", " ^  Util.iToS i ^ "(" ^ regToStr base ^
      (case offset of
           SOME (reg, s) => ", " ^ regToStr reg ^ ", " ^ Util.iToS s
         | NONE => "")
      ^ ")"
    | INS_X {opcode=opc} => opToStr opc)


fun bbToStr platform (l, L) =
    (if hd (explode l) <> #"L" then nameToStr platform l else l) ^
    ":\n" ^ (foldr (fn (ins, s) => "\t" ^ insToStr platform ins
                                   ^ "\n" ^ s) "" L)


fun funcToStr platform (id, body) =
    "\t.globl " ^ nameToStr platform id ^ "\n" ^
    Cfg.fold (fn (bb, s) => s ^ bbToStr platform bb) "" body ^
    (if platform = Util.OS_X then "" else "\t.size " ^ id ^ ", .-" ^ id
                                          ^ "\n") ^ "\n"

fun globalToStr platform id =
    if platform = Util.OS_X
    then "\t.globl _" ^ id ^ "\n_" ^ id ^ ":\n\t.quad 0\n\n"
    else "\t.comm " ^ id ^ ",8,8\n"


(*text is the functions, data is the globals*)
fun programToStr platform (PROGRAM {text=text, data=data}) =
    (if platform = Util.OS_X then "\t.section __TEXT,__text\n"
     else "\t.text\n") ^
    foldr (fn (func, s) => funcToStr platform func ^ s) "" text ^
    (if platform = Util.OS_X then "\t.section __DATA,__data\n"
     else "\t.data\n") ^
    globalToStr platform "rdest" ^
    foldr (fn (id, s) => globalToStr platform id ^ s) "" data ^
    (if platform = Util.OS_X then "\n\t.section __TEXT,__cstring\n"
     else "\n\t.section .rodata\n") ^
    "L__ss__:\n\t.asciz \"%lld\"\n" ^
    "L__s__:\n\t.asciz \"%lld \"\n" ^
    "L__sn__:\n\t.asciz \"%lld\\n\"\n"

end

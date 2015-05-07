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
   | INS_RRI of {opcode: opcode, r1: int, r2: int, immed: int}
   | INS_RRC of {opcode: opcode, r1: int, r2: int}
   | INS_RIC of {opcode: opcode, r1: int, immed: int}
   | INS_CLL of {opcode: opcode, l1: string, l2: string}
   | INS_SIR of {opcode: opcode, id: string, immed: int, r1: int}
   | INS_NEW of {opcode: opcode, dest: int, id: string, fields: string list}
   | INS_RR  of {opcode: opcode, dest: int, r1: int}
   | INS_IR  of {opcode: opcode, dest: int, immed: int}
   | INS_RI  of {opcode: opcode, dest: int, immed: int}
   | INS_SR  of {opcode: opcode, id: string, r1: int}
   | INS_RS  of {opcode: opcode, id: string, r1: int}
   | INS_L   of {opcode: opcode, l1: string}
   | INS_R   of {opcode: opcode, r1: int}
   | INS_X   of {opcode: opcode}


type basic_block = string * instruction list


type function = string * basic_block Cfg.cfg


type program = function list


val opToStr =
 fn OP_ADD                  => "add "
  | OP_ADDI                 => "addi "
  | OP_DIV                  => "div "
  | OP_MULT                 => "mult "
  | OP_SUB                  => "sub "
  | OP_SUBI                 => "subi "
  | OP_AND                  => "and "
  | OP_OR                   => "or "
  | OP_XORI                 => "xori "
  | OP_COMP                 => "comp "
  | OP_COMPI                => "compi "
  | OP_CBREQ                => "cbreq "
  | OP_CBRGE                => "cbrge "
  | OP_CBRGT                => "cbrgt "
  | OP_CBRLE                => "cbrle "
  | OP_CBRLT                => "cbrlt "
  | OP_CBRNE                => "cbrne "
  | OP_JUMPI                => "jumpi "
  | OP_LOADI                => "loadi "
  | OP_LOADAI               => "loadai "
  | OP_LOADGLOBAL           => "loadglobal "
  | OP_LOADINARGUMENT       => "loadinargument "
  | OP_LOADRET              => "loadret "
  | OP_COMPUTEFORMALADDRESS => "computeformaladdress "
  | OP_RESTOREFORMAL        => "restoreformal "
  | OP_COMPUTEGLOBALADDRESS => "computeglobaladdress "
  | OP_STOREAI              => "storeai "
  | OP_STOREGLOBAL          => "storeglobal "
  | OP_STOREINARGUMENT      => "storeinargument "
  | OP_STOREOUTARGUMENT     => "storeoutargument "
  | OP_STORERET             => "storeret "
  | OP_CALL                 => "call "
  | OP_RET                  => "ret "
  | OP_NEW                  => "new "
  | OP_DEL                  => "del "
  | OP_PRINT                => "print "
  | OP_PRINTLN              => "println "
  | OP_READ                 => "read "
  | OP_MOV                  => "mov "
  | OP_MOVEQ                => "moveq "
  | OP_MOVGE                => "movge "
  | OP_MOVGT                => "movgt "
  | OP_MOVLE                => "movle "
  | OP_MOVLT                => "movlt "
  | OP_MOVNE                => "movne "


fun rToStr r = "r" ^ Int.toString r
fun iToStr i = Int.toString i


fun newToStr opc id fds d =
    opToStr opc ^ id ^ ", [" ^ Ast.foldd ", " (fn f => f) fds ^ "], " ^ rToStr d


val insToStr =
 fn INS_RRR {opcode=opc, r1=r1, r2=r2, dest=d} =>
    opToStr opc ^ rToStr r1 ^ ", " ^ rToStr r2 ^ ", " ^ rToStr d
  | INS_RIR {opcode=opc, r1=r1, immed=i, dest=d} =>
    opToStr opc ^ rToStr r1 ^ ", " ^ iToStr i ^ ", " ^ rToStr d
  | INS_RRI {opcode=opc, r1=r1, r2=r2, immed=i} =>
    opToStr opc ^ rToStr r1 ^ ", " ^ rToStr r2 ^ ", " ^ iToStr i
  | INS_RRC {opcode=opc, r1=r1, r2=r2} =>
    opToStr opc ^ rToStr r1 ^ ", " ^ rToStr r2
  | INS_RIC {opcode=opc, r1=r1, immed=i} =>
    opToStr opc ^ rToStr r1 ^ ", " ^ iToStr i
  | INS_CLL {opcode=opc, l1=l1, l2=l2} => opToStr opc ^ l1 ^ ", " ^ l2
  | INS_L {opcode=opc, l1=l1} => opToStr opc ^ l1
  | INS_IR {opcode=opc, immed=i, dest=d} =>
    opToStr opc ^  iToStr i ^ ", " ^ rToStr d
  | INS_RI {opcode=opc, immed=i, dest=d} =>
    opToStr opc ^ rToStr d ^ ", " ^ iToStr i
  | INS_SR {opcode=opc, id=id, r1=r1} => opToStr opc ^ id ^ ", " ^ rToStr r1
  | INS_RS {opcode=opc, id=id, r1=r1} => opToStr opc ^ rToStr r1 ^ ", " ^ id
  | INS_SIR {opcode=opc, id=id, immed=i, r1=r1} =>
    opToStr opc ^ id ^ ", " ^ iToStr i ^ ", " ^ rToStr r1
  | INS_R {opcode=opc, r1=r1} => opToStr opc ^ rToStr r1
  | INS_X {opcode=opc} => opToStr opc
  | INS_NEW {opcode=opc, id=id, fields=fields, dest=d} =>
    newToStr opc id fields d
  | INS_RR {opcode=opc, r1=r1, dest=d} =>
    opToStr opc ^ rToStr r1 ^ ", " ^ rToStr d


fun bbToStr (l, L) =
    l ^ ":\n" ^ (foldr (fn (ins, s) => "\t" ^ insToStr ins ^ "\n" ^ s) "" L)


fun funcToStr (id, body) =
    (foldr (fn (bb, s) => bbToStr bb ^ s) "" (Cfg.toList body)) ^ "\n"


fun programToStr funcs =
    foldr (fn (func, s) => (funcToStr func) ^ s) "" funcs

end

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


fun opToStr OP_ADD = "add"
  | opToStr OP_ADDI = "addi"
  | opToStr OP_DIV = "div"
  | opToStr OP_MULT = "mult"
  | opToStr OP_SUB = "sub"
  | opToStr OP_SUBI = "subi"
  | opToStr OP_AND = "and"
  | opToStr OP_OR = "or"
  | opToStr OP_XORI = "xori"
  | opToStr OP_COMP = "comp"
  | opToStr OP_COMPI = "compi"
  | opToStr OP_CBREQ = "cbreq"
  | opToStr OP_CBRGE = "cbrge"
  | opToStr OP_CBRGT = "cbrgt"
  | opToStr OP_CBRLE = "cbrle"
  | opToStr OP_CBRLT = "cbrlt"
  | opToStr OP_CBRNE = "cbrne"
  | opToStr OP_JUMPI = "jumpi"
  | opToStr OP_LOADI = "loadi"
  | opToStr OP_LOADAI = "loadai"
  | opToStr OP_LOADGLOBAL = "loadglobal"
  | opToStr OP_LOADINARGUMENT = "loadinargument"
  | opToStr OP_LOADRET = "loadret"
  | opToStr OP_COMPUTEFORMALADDRESS = "computeformaladdress"
  | opToStr OP_RESTOREFORMAL = "restoreformal"
  | opToStr OP_COMPUTEGLOBALADDRESS = "computeglobaladdress"
  | opToStr OP_STOREAI = "storeai"
  | opToStr OP_STOREGLOBAL = "storeglobal"
  | opToStr OP_STOREINARGUMENT = "storeinargument"
  | opToStr OP_STOREOUTARGUMENT = "storeoutargument"
  | opToStr OP_STORERET = "storeret"
  | opToStr OP_CALL = "call"
  | opToStr OP_RET = "ret"
  | opToStr OP_NEW = "new"
  | opToStr OP_DEL = "del"
  | opToStr OP_PRINT = "print"
  | opToStr OP_PRINTLN = "println"
  | opToStr OP_READ = "read"
  | opToStr OP_MOV = "mov"
  | opToStr OP_MOVEQ = "moveq"
  | opToStr OP_MOVGE = "movge"
  | opToStr OP_MOVGT = "movgt"
  | opToStr OP_MOVLE = "movle"
  | opToStr OP_MOVLT = "movlt"
  | opToStr OP_MOVNE = "movne"


fun r2Str r = "r" ^ (Int.toString r)


local
    fun fs2Str [] = ""
      | fs2Str (field::[]) = field
      | fs2Str (field::fields) = field ^ ", " ^ (fs2Str fields)
in
    fun newToStr opcode id fds d =
        (opToStr opcode) ^ " " ^ id ^ ", [" ^ (fs2Str fds) ^ "], " ^ (r2Str d)
end


val insToStr =
 fn INS_RRR {opcode=opc, r1=r1, r2=r2, dest=d} =>
    (opToStr opc) ^ " " ^ (r2Str r1) ^ ", " ^ (r2Str r2) ^ ", " ^ (r2Str d)
  | INS_RIR {opcode=opc, r1=r1, immed=i, dest=d} =>
    (opToStr opc) ^ " " ^ (r2Str r1) ^ ", " ^ (Int.toString i) ^ ", " ^ (r2Str d)
  | INS_RRI {opcode=opc, r1=r1, r2=r2, immed=i} =>
    (opToStr opc) ^ " " ^ (r2Str r1) ^ ", " ^ (r2Str r2) ^ ", " ^ (Int.toString i)
  | INS_RRC {opcode=opc, r1=r1, r2=r2} =>
    (opToStr opc) ^ " " ^ (r2Str r1) ^ ", " ^ (r2Str r2)
  | INS_RIC {opcode=opc, r1=r1, immed=i} =>
    (opToStr opc) ^ " " ^ (r2Str r1) ^ ", " ^ (Int.toString i)
  | INS_CLL {opcode=opc, l1=l1, l2=l2} =>
    (opToStr opc) ^ " " ^ l1 ^ ", " ^ l2
  | INS_L {opcode=opc, l1=l1} =>
    (opToStr opc) ^ " " ^ l1
  | INS_IR {opcode=opc, immed=i, dest=dest} =>
    (opToStr opc) ^ " " ^  (Int.toString i) ^ ", " ^ (r2Str dest)
  | INS_RI {opcode=opc, immed=i, dest=dest} =>
    (opToStr opc) ^ " " ^ (r2Str dest) ^ ", " ^ (Int.toString i)
  | INS_SR {opcode=opc, id=id, r1=r1} =>
    (opToStr opc) ^ " " ^ id ^ ", " ^ (r2Str r1)
  | INS_RS {opcode=opc, id=id, r1=r1} =>
    (opToStr opc) ^ " " ^ (r2Str r1) ^ ", " ^ id
  | INS_SIR {opcode=opc, id=id, immed=i, r1=r1} =>
    (opToStr opc) ^ " " ^ id ^ ", " ^ (Int.toString i) ^ ", " ^ (r2Str r1)
  | INS_R {opcode=opc, r1=r1} => (opToStr opc) ^ " " ^ (r2Str r1)
  | INS_X {opcode=opc} => (opToStr opc)
  | INS_NEW {opcode=opc, id=id, fields=fields, dest=dest} =>
    newToStr opc id fields dest
  | INS_RR {opcode=opc, r1=r1, dest=dest} =>
    (opToStr opc) ^ " " ^ (r2Str r1) ^ ", " ^ (r2Str dest)


fun bbToStr (l, L) =
    l ^ ":\n" ^ (foldr (fn (ins, s) => "\t" ^ (insToStr ins) ^ "\n" ^ s) "" L)

end

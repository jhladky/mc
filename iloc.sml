
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
   | INS_RSR of {opcode: opcode, dest: int, r1: int, field: string}
   | INS_RRS of {opcode: opcode, r1: int, r2: int, field: string}
   | INS_CLL of {opcode: opcode, l1: string, l2: string}
   | INS_SIR of {opcode: opcode, id: string, immed: int, r1: int}
   | INS_NEW of {opcode: opcode, dest: int, id: string, fields: string list}
   | INS_RR  of {opcode: opcode, dest: int, r1: int}
   | INS_IR  of {opcode: opcode, dest: int, immed: int}
   | INS_RI  of {opcode: opcode, dest: int, immed: int}
   | INS_SR  of {opcode: opcode, id: string, r1: int}
   | INS_SI  of {opcode: opcode, id: string, immed: int}
   | INS_L   of {opcode: opcode, l1: string}
   | INS_R   of {opcode: opcode, r1: int}
   | INS_X   of {opcode: opcode}


type basicBlock = string * instruction list


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


fun toString (INS_RRR {opcode=opcode, r1=r1, r2=r2, dest=d}) =
    (opToStr opcode) ^ " " ^ (r2Str r1) ^ ", " ^ (r2Str r2) ^ ", " ^ (r2Str d)
  | toString (INS_RIR {opcode=opcode, r1=r1, immed=immed, dest=d}) =
    (opToStr opcode) ^ " " ^ (r2Str r1) ^ ", " ^ (Int.toString immed) ^ ", " ^ (r2Str d)
  | toString (INS_RRC {opcode=opcode, r1=r1, r2=r2}) =
    (opToStr opcode) ^ " " ^ (r2Str r1) ^ ", " ^ (r2Str r2)
  | toString (INS_RIC {opcode=opcode, r1=r1, immed=immed}) =
    (opToStr opcode) ^ " " ^ (r2Str r1) ^ ", " ^ (Int.toString immed)
  | toString (INS_CLL {opcode=opcode, l1=l1, l2=l2}) =
    (opToStr opcode) ^ " " ^ l1 ^ ", " ^ l2
  | toString (INS_L {opcode=opcode, l1=l1}) =
    (opToStr opcode) ^ " " ^ l1
  | toString (INS_IR {opcode=opcode, immed=immed, dest=dest}) =
    (opToStr opcode) ^ " " ^  (Int.toString immed) ^ ", " ^ (r2Str dest)
  | toString (INS_RI {opcode=opcode, immed=immed, dest=dest}) =
    (opToStr opcode) ^ " " ^ (r2Str dest) ^ ", " ^ (Int.toString immed)
  | toString (INS_SR {opcode=opcode, id=id, r1=r1}) =
    (opToStr opcode) ^ " " ^ id ^ ", " ^ (r2Str r1)
  | toString (INS_SIR {opcode=opcode, id=id, immed=i, r1=r1}) =
    (opToStr opcode) ^ " " ^ id ^ ", " ^ (Int.toString i) ^ ", " ^ (r2Str r1)
  | toString (INS_R {opcode=opcode, r1=r1}) =
    (opToStr opcode) ^ " " ^ (r2Str r1)
  | toString (INS_SI {opcode=opcode, id=id, immed=immed}) =
    (opToStr opcode) ^ " " ^ id ^ ", " ^ (Int.toString immed)
  | toString (INS_X {opcode=opcode}) =
    (opToStr opcode)
  | toString (INS_NEW {opcode=opcode, id=id, fields=fields, dest=d}) =
    let
        fun fs2Str [] = ""
          | fs2Str (field::[]) = field
          | fs2Str (field::fields) = field ^ ", " ^ (fs2Str fields)
    in
        (opToStr opcode) ^ " " ^ id ^ ", [" ^
        (fs2Str fields) ^ "], " ^ (r2Str d)
    end
  | toString (INS_RR {opcode=opcode, r1=r1, dest=dest}) =
    (opToStr opcode) ^ " " ^ (r2Str r1) ^ ", " ^ (r2Str dest)
  | toString (INS_RSR {opcode=opcode, r1=r1, field=field, dest=dest}) =
    (opToStr opcode) ^ " " ^ (r2Str r1) ^ ", " ^ field ^ ", " ^ (r2Str dest)
  | toString (INS_RRS {opcode: opcode, r1: int, r2: int, field: string}) =
    (opToStr opcode) ^ " " ^ (r2Str r1) ^ ", " ^ (r2Str r2) ^ ", " ^ field

end

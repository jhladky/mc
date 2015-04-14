open Iloc;

fun opcode2Str OP_ADD = "add"
  | opcode2Str OP_ADDI = "addi"
  | opcode2Str OP_DIV = "div"
  | opcode2Str OP_MULT = "mult"
  | opcode2Str OP_SUB = "sub"
  | opcode2Str OP_SUBI = "subi"
  | opcode2Str OP_AND = "and"
  | opcode2Str OP_OR = "or"
  | opcode2Str OP_XORI = "xori"
  | opcode2Str OP_COMP = "comp"
  | opcode2Str OP_COMPI = "compi"
  | opcode2Str OP_CBREQ = "cbreq"
  | opcode2Str OP_CBRGE = "cbrge"
  | opcode2Str OP_CBRGT = "cbrgt"
  | opcode2Str OP_CBRLE = "cbrle"
  | opcode2Str OP_CBRLT = "cbrlt"
  | opcode2Str OP_CBRNE = "cbrne"
  | opcode2Str OP_JUMPI = "jumpi"
  | opcode2Str OP_LOADI = "loadi"
  | opcode2Str OP_LOADAI = "loadai"
  | opcode2Str OP_LOADGLOBAL = "loadglobal"
  | opcode2Str OP_LOADINARGUMENT = "loadinargument"
  | opcode2Str OP_LOADRET = "loadret"
  | opcode2Str OP_COMPUTEFORMALADDRESS = "computeformaladdress"
  | opcode2Str OP_RESTOREFORMAL = "restoreformal"
  | opcode2Str OP_COMPUTEGLOBALADDRESS = "computeglobaladdress"
  | opcode2Str OP_STOREAI = "storeai"
  | opcode2Str OP_STOREGLOBAL = "storeglobal"
  | opcode2Str OP_STOREINARGUMENT = "storeinargument"
  | opcode2Str OP_STOREOUTARGUMENT = "storeoutargument"
  | opcode2Str OP_STORERET = "storeret"
  | opcode2Str OP_CALL = "call"
  | opcode2Str OP_RET = "ret"
  | opcode2Str OP_NEW = "new"
  | opcode2Str OP_DEL = "del"
  | opcode2Str OP_PRINT = "print"
  | opcode2Str OP_PRINTLN = "println"
  | opcode2Str OP_READ = "read"
  | opcode2Str OP_MOV = "mov"
  | opcode2Str OP_MOVEQ = "moveq"
  | opcode2Str OP_MOVGE = "movge"
  | opcode2Str OP_MOVGT = "movgt"
  | opcode2Str OP_MOVLE = "movle"
  | opcode2Str OP_MOVLT = "movlt"
  | opcode2Str OP_MOVNE = "movne"


fun r2Str r = "r" ^ (Int.toString r)


fun ins2Str (INS_RRR {opcode=opcode, r1=r1, r2=r2, dest=dest}) =
    (opcode2Str opcode) ^ " " ^ (r2Str r1) ^ ", " ^ (r2Str r2) ^ ", " ^
    (r2Str dest)
  | ins2Str (INS_RIR {opcode=opcode, r1=r1, immed=immed, dest=dest}) =
    (opcode2Str opcode) ^ " " ^ (r2Str r1) ^ ", " ^ (r2Str immed) ^ ", " ^
    (r2Str dest)
  | ins2Str (INS_RRC {opcode=opcode, r1=r1, r2=r2}) =
    (opcode2Str opcode) ^ " " ^ (r2Str r1) ^ ", " ^ (r2Str r2) ^ ", ccr"
  | ins2Str (INS_RIC {opcode=opcode, r1=r1, immed=immed}) =
    (opcode2Str opcode) ^ " " ^ (r2Str r1) ^ ", " ^ (Int.toString immed) ^
    ", ccr"
  | ins2Str (INS_CLL {opcode=opcode, l1=l1, l2=l2}) =
    (opcode2Str opcode) ^ " ccr, " ^ l1 ^ ", " ^ l2
  | ins2Str (INS_L {opcode=opcode, l1=l1}) =
    (opcode2Str opcode) ^ " " ^ l1
  | ins2Str (INS_IR {opcode=opcode, immed=immed, dest=dest}) =
    (opcode2Str opcode) ^ " " ^ (Int.toString immed) ^ ", " ^ (r2Str dest)
  | ins2Str (INS_SR {opcode=opcode, id=id, r1=r1}) =
    (opcode2Str opcode) ^ " " ^ id ^ ", " ^ (r2Str r1)
  | ins2Str (INS_SIR {opcode=opcode, id=id, immed=immed, r1=r1}) =
    (opcode2Str opcode) ^ " " ^ id ^ ", " ^ (Int.toString immed) ^ ", " ^
    (r2Str r1)
  | ins2Str (INS_R {opcode=opcode, r1=r1}) =
    (opcode2Str opcode) ^ " " ^ (r2Str r1)
  | ins2Str (INS_SI {opcode=opcode, id=id, immed=immed}) =
    (opcode2Str opcode) ^ " " ^ id ^ ", " ^ (Int.toString immed)
  | ins2Str (INS_X {opcode=opcode}) =
    (opcode2Str opcode)
  | ins2Str (INS_NEW {opcode=opcode, id=id, fields=fields, dest=dest}) =
    let
        fun fs2Str [] = ""
          | fs2Str (field::[]) = field
          | fs2Str (field::fields) = field ^ ", " ^ (fs2Str fields)
    in
        (opcode2Str opcode) ^ " " ^ id ^ ", [" ^
        (fs2Str fields) ^ "], " ^ (r2Str dest)
    end


fun printNode (l, L) =
    (print ((if String.isPrefix "L" l then "" else "\n") ^ l ^ ":\n");
     app (fn ins => print ("\t" ^ (ins2Str ins) ^ "\n")) L)


fun printCfg ht =
    app printNode (List.concat (map Cfg.toList (HashTable.listItems ht)))

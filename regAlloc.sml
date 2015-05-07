signature REG_ALLOC = sig
    val regAlloc : TargetAmd64.program -> unit
end

structure RegAlloc :> REG_ALLOC = struct
open TargetAmd64
open UnorderedSet


fun regAlloc (gen, kill) (INS_RR {opcode=opcode, r1=r1, r2=r2}) =
    (if not (member (kill, r1)) then add (gen, r1) else gen, add (kill, r2))
  | regAlloc (gen, kill) (INS_IR {opcode=opcode, immed=immed, r2=r2}) =
    (gen, add (kill, r2))
  | regAlloc (gen, kill) (INS_KR {opcode=opcode, k=k, r2=r2}) =
    (gen, add (kill, r2))
  | regAlloc (gen, kill) (INS_SR {opcode=opcode, id=id, dest=dest}) =
    (gen, add (kill, dest))
  | regAlloc (gen, kill) (INS_GR {opcode=opcode, global=global, dest=dest}) =
    (gen, add (kill, dest))
  | regAlloc (gen, kill) (INS_RG {opcode=opcode, r1=r1, global=global}) =
    (if not (member (kill, r1)) then add (gen, r1) else gen , kill)
  | regAlloc (gen, kill) (INS_MR {opcode=opcode, immed=immed, base=base, dest=dest,
                              offset=offset}) =
    (gen, kill) (*fix*)
  | regAlloc (gen, kill) (INS_RM {opcode=opcode, r1=r1, immed=immed, base=base,
                              offset=offset}) =
    (gen, kill) (*fix*)
  | regAlloc (gen, kill) (INS_R {opcode=opcode, r1=r1}) =
    (gen, add (kill, r1)) (*pretty sure this is wrong*)
  | regAlloc gk _ = gk


fun bbAlloc (id, bb) =
    let
        val (gen, kill) = List.foldl (fn (ins, gk) => regAlloc gk ins)
                                     (empty (), empty ()) bb
    in
        ()
    end


(* Function level.*)
fun funcAlloc (id, cfg) =
    ()
    (* List.app bbAlloc bbs *)


fun regAlloc (PROGRAM {text=text, data=data}) =
    List.app funcAlloc text

end

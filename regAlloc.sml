signature REG_ALLOC = sig
    val regAlloc : TargetAmd64.program -> unit
end

structure RegAlloc :> REG_ALLOC = struct
open TargetAmd64
open UnorderedSet

fun condAdd (gen, kill) reg =
    if not (member (kill, reg)) then add (gen, reg) else gen


fun regToGK (gen, kill) (INS_RR {r1=r1, r2=r2, ...}) =
    (condAdd (gen, kill) r1, add (kill, r2))
  | regToGK (gen, kill) (INS_IR {immed=immed, r2=r2, ...}) =
    (gen, add (kill, r2))
  | regToGK (gen, kill) (INS_KR {k=k, r2=r2, ...}) =
    (gen, add (kill, r2))
  | regToGK (gen, kill) (INS_SR {id=id, dest=dest, ...}) =
    (gen, add (kill, dest))
  | regToGK (gen, kill) (INS_GR {global=global, dest=dest, ...}) =
    (gen, add (kill, dest))
  | regToGK (gen, kill) (INS_RG {r1=r1, global=global, ...}) =
    (condAdd (gen, kill) r1, kill)
  | regToGK (gen, kill) (INS_MR {immed=immed, base=base, dest=dest,
                                 offset=offset, ...}) =
    let
        val gen = condAdd (gen, kill) base
    in
        (case offset of SOME (r, _) => condAdd (gen, kill) r | NONE => gen,
         add (kill, dest))
    end
  | regToGK (gen, kill) (INS_RM {r1=r1, immed=immed, base=base,
                                 offset=offset, ...}) =
    let
        val kill = add (kill, base)
    in
        (condAdd (gen, kill) r1,
         case offset of SOME (r, _) => add (kill, r) | NONE => kill)
    end
  | regToGK (gen, kill) (INS_R {r1=r1, ...}) = (*pretty sure this is wrong*)
    (gen, add (kill, r1))
  (* The remaining instruction types don't involve any registers. *)
  | regToGK gk _ = gk


fun bbToGK (id, bb) =
    let
        val gk = List.foldl (fn (ins, gk) => regToGK gk ins)
                            (empty (), empty ()) bb
    in
        (* string  * instruction list * (set * set) * set*)
        (*bb label, bb instructions, gen/kill set, liveout set*)
        (id, bb, gk, empty ())
    end


(* Function level.*)
fun funcToGK (id, cfg) =
    (id, Cfg.map bbToGK cfg)


(* LiveOut(n) = union (for every set where m is in the successors of n) of the gen set of m union with the LiveOut set of m - the kill set of m. *)
(* n / m is a basic block. *)

(* gen(m) U (LiveOut(m) - kill(m)) *)

fun bbLiveOut1 (_, _, (gen, kill), liveOut) =
    union (gen, difference (liveOut, kill))

fun bbLiveOut succs (id, bb, gk, liveOut) =
    let
        val lo = List.foldr (fn (bb, s) => union (s, bbLiveOut1 bb))
                            (empty ()) succs
    in
        (id, bb, gk, lo)
    end


fun funcLiveOut (id, cfg) =
    (id, Cfg.apply bbLiveOut cfg)


fun regAlloc (PROGRAM {text=text, data=data}) =
    let
        val funcs = List.map funcToGK text
    in
        (*so funcs is the base, now let's write functions to compute the liveout set once*)
        List.map funcLiveOut funcs;
        ()
    end

end

structure Types =
struct
type unique = unit ref

datatype ty =
         RECORD of (Symbol.symbol * ty) list * unique
         | NIL
         | INT
         | STRING
         | ARRAY of ty * unique
	       | NAME of Symbol.symbol * ty option ref
         | VOID
	     (* | UNIT *)

exception ActualType
fun actual_ty(t : ty) : ty = (
  case t of
  NAME(sym, ref(SOME(t))) => t
  | NAME(_, ref NONE) => raise ActualType
  | _ => t
)
(* これは SMLの=と等価？ *)
fun eqv(t1 : ty, t2 : ty) : bool = (
  case (actual_ty t1, actual_ty t2) of
  (NIL, NIL) => true
  | (INT, INT) => true
  | (STRING, STRING) => true
  | (VOID, VOID) => true
  | (RECORD(_, id1), RECORD(_, id2)) => id1 = id2
  | (ARRAY(_, id1), ARRAY(_, id2)) => id1 = id2
  | _ => false
)

fun show(typ:ty):string = case typ of
  RECORD (flds, _) => "{" ^
        (foldr (fn ((sym, ty) : Symbol.symbol * ty, acc : string) => 
                 (Symbol.name sym) ^ " : " ^ (show ty) ^ ", " ^ acc)
              "" flds)
              ^ "}"
  | NIL => "nil"
  | INT => "int"
  | STRING => "string"
  | ARRAY (ty, _) => "[" ^ (show ty) ^ "]"
  | NAME _ => "NAME"
  (* | UNIT => "UNIT" *)
  | VOID => "void"
end
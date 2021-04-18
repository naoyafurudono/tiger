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

fun show(typ:ty):string = case typ of
  RECORD (flds, _) => "RECORD " ^
        (foldr (fn ((sym, ty) : Symbol.symbol * ty, acc : string) => 
                 (Symbol.name sym) ^ " : " ^ (show ty) ^ ", " ^ acc)
              "" flds)
  | NIL => "NIL"
  | INT => "INT"
  | STRING => "STRING"
  | ARRAY _ => "ARRAY"
  | NAME _ => "NAME"
  (* | UNIT => "UNIT" *)
  | VOID => "VOID"
end
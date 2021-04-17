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
	     | UNIT

fun show(typ:ty):string = case typ of
  RECORD _ => "RECORD"
  | NIL => "NIL"
  | INT => "INT"
  | STRING => "STRING"
  | ARRAY _ => "ARRAY"
  | NAME _ => "NAME"
  | UNIT => "UNIT"
end



structure Absyn =
struct

type pos = int
     and  symbol = Symbol.symbol

datatype var = SimpleVar of symbol * pos       (* variable *)
             | FieldVar of var * symbol * pos  (* record-var.field-sym *)
             | SubscriptVar of var * exp * pos (* array-var[index-exp] *)

     and exp = VarExp of var
             | NilExp
             | IntExp of int
             | StringExp of string * pos
             | CallExp of {func: symbol, args: exp list, pos: pos}
             | OpExp of {left: exp, oper: oper, right: exp, pos: pos}
             | RecordExp of {fields: (symbol * exp * pos) list, typ: symbol, pos: pos}  (* レコード宣言 *)
             | SeqExp of exp * exp * pos  (* changing the type; from (exp * pos) list *)
             | AssignExp of {var: var, exp: exp, pos: pos}
             | IfExp of {test: exp, then': exp, else': exp option, pos: pos}
             | WhileExp of {test: exp, body: exp, pos: pos}
	     | ForExp of {var: symbol, escape: bool ref, lo: exp, hi: exp, body: exp, pos: pos}
             | BreakExp of pos
             | LetExp of {decs: dec list, body: exp, pos: pos}  (* changing the type of body; from exp list *)
             | ArrayExp of {typ: symbol, size: exp, init: exp, pos: pos}  (* 配列宣言 *)

     and dec = FunctionDec of fundec list
             | VarDec of vardec
             | TypeDec of tydec list
     and ty = NameTy of symbol * pos
            | RecordTy of field list
            | ArrayTy of symbol * pos

     and oper = PlusOp | MinusOp | TimesOp | DivideOp
                | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp

withtype field = {name: symbol, escape: bool ref, typ: symbol, pos: pos}  (* typ fieldはty? *)
     and   fundec = {name: symbol,
		                 params: field list,
		                 result: (symbol * pos) option,  (* ここもty optionだと思う *)
		                 body: exp,
		                 pos: pos}
     and   vardec = {name: symbol,
		                 escape: bool ref,
		                 typ: (symbol * pos) option,  (* ここはty option ではないか？ *)
		                 init: exp,
		                 pos: pos}
     and tydec =  {name: symbol, ty: ty, pos: pos}
end


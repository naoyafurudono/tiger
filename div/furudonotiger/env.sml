structure T = Types
structure S = Symbol
structure Env :> 
sig
    (* type acces *)
    type ty
    datatype enventry = VarEntry of {ty : ty}
           |  FunEntry of {formals : ty list, result : ty}
    val base_tenv : ty Symbol.table (* name -> type e.g. int -> INT *)
    val base_venv : enventry Symbol.table (* name -> type-of-exp *)

    val enter : 'a table * Symbol.symbol * 'a -> 'a table
    val look  : 'a table * Symbol.symbol -> 'a option
end
 =
struct
    type ty = T.ty
    datatype enventry = VarEntry of {ty : ty}
           |  FunEntry of {formals : ty list, result : ty}
    val base_tenv : ty Symbol.table (* name -> type e.g. int -> INT *) =
      let 
        val emp = S.empty
        val e1  = S.enter(emp, S.symbol("int"), T.INT)
        val e2 = S.enter(e2, S.symbol("string", T.STRING))
        in e2 
      end
    val base_venv : enventry Symbol.table (* name -> type-of-exp *) =
      let 
        val emp = S.empty
        val e1 = S.enter(emp, S.symbol("/* bottom */"), FunEntry{formals=[T.INT], result=T.INT})
        val e2 = S.enter(e1, S.symbol("/* trash */"), VarEntry{ty=T.INT})
        in e2
       end

    val enter : 'a table * Symbol.symbol 'a -> 'a table
    val look  : 'a table * Symbol.symbol -> 'a option
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
      (* standard librally for Tiger *)
        val emp = S.empty
        val e1 = S.enter(emp, S.symbol("print"), FunEntry{formals=[], result=T.NIL})
        val e2 = S.enter(e1, S.symbol("flush"), FunEntry{formals=[], result=T.NIL})
        val e3 = S.enter(e2, S.symbol("getchar"), FunEntry{formals=[], result=T.STRING})
        val e4 = S.enter(e3, S.symbol("org"), FunEntry{formals=T.STRING[], result=T.INT})
        val e5 = S.enter(e4, S.symbol("chr"), FunEntry{formals=[T.INT], result=T.STRING})
        val e6 = S.enter(e4, S.symbol("size"), FunEntry{formals=[T.STRING], result=T.INT})
        val e7 = S.enter(e4, S.symbol("substring"), FunEntry{formals=[T.STRING], result=T.STRING})
        val e8 = S.enter(e4, S.symbol("concat"), FunEntry{formals=[T.STRING], result=T.STRING})
        val e9 = S.enter(e4, S.symbol("not"), FunEntry{formals=[T.INT], result=T.INT})
        val e10 = S.enter(e4, S.symbol("exit"), FunEntry{formals=[T.INT], result=T.NIL})
        in e10
       end

    val enter : 'a table * Symbol.symbol 'a -> 'a table
    val look  : 'a table * Symbol.symbol -> 'a option
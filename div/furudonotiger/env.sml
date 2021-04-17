structure Env :
sig
    (* type acces *)
    type ty
    datatype enventry = VarEntry of {ty : ty}
           |  FunEntry of {formals : ty list, result : ty}
    val base_tenv : ty Symbol.table (* name -> type e.g. int -> INT *)
    val base_venv : enventry Symbol.table (* name -> type-of-exp *)

    val enter : 'a Symbol.table * Symbol.symbol * 'a -> 'a Symbol.table
    val look  : 'a Symbol.table * Symbol.symbol -> 'a option
end
 =
struct
    type ty = Types.ty
    datatype enventry = VarEntry of {ty : ty}
           |  FunEntry of {formals : ty list, result : ty}
    val base_tenv : ty Symbol.table (* name -> type e.g. int -> INT *) =
      let 
        val emp = Symbol.empty
        val e1  = Symbol.enter(emp, Symbol.symbol("int"), Types.INT)
        val e2 = Symbol.enter(e1, Symbol.symbol("string"), Types.STRING)
        in e2 
      end
    val base_venv : enventry Symbol.table (* name -> type-of-exp *) =
      let 
      (* standard librally for Tiger *)
        val emp = Symbol.empty
        val e1 = Symbol.enter(emp, Symbol.symbol("print"), FunEntry{formals=[], result=Types.NIL})
        val e2 = Symbol.enter(e1, Symbol.symbol("flush"), FunEntry{formals=[], result=Types.NIL})
        val e3 = Symbol.enter(e2, Symbol.symbol("getchar"), FunEntry{formals=[], result=Types.STRING})
        val e4 = Symbol.enter(e3, Symbol.symbol("org"), FunEntry{formals=[Types.STRING], result=Types.INT})
        val e5 = Symbol.enter(e4, Symbol.symbol("chr"), FunEntry{formals=[Types.INT], result=Types.STRING})
        val e6 = Symbol.enter(e4, Symbol.symbol("size"), FunEntry{formals=[Types.STRING], result=Types.INT})
        val e7 = Symbol.enter(e4, Symbol.symbol("substring"), FunEntry{formals=[Types.STRING], result=Types.STRING})
        val e8 = Symbol.enter(e4, Symbol.symbol("concat"), FunEntry{formals=[Types.STRING], result=Types.STRING})
        val e9 = Symbol.enter(e4, Symbol.symbol("not"), FunEntry{formals=[Types.INT], result=Types.INT})
        val e10 = Symbol.enter(e4, Symbol.symbol("exit"), FunEntry{formals=[Types.INT], result=Types.NIL})
        in e10
       end

    val enter = Symbol.enter
    val look  = Symbol.look
  end
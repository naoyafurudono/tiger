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
datatype enventry =
         VarEntry of {ty : ty}
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
    val std = [
        ("print", [Types.STRING], Types.VOID)
        ,("flush", [], Types.VOID)
        ,("getchar", [], Types.STRING)
        ,("ord", [Types.STRING], Types.INT)
        ,("chr", [Types.INT], Types.STRING)
        ,("size", [Types.STRING], Types.INT)
        ,("substring", [Types.STRING, Types.INT, Types.INT], Types.STRING)
        ,("concat", [Types.STRING, Types.STRING], Types.STRING)
        ,("not", [Types.INT], Types.INT)
        ,("exit", [Types.INT], Types.VOID)
    ]
    in
      foldl (fn ((name, params, res), venv: enventry Symbol.table) => 
      Symbol.enter(venv, Symbol.symbol name, FunEntry{formals= params, result= res}))
            Symbol.empty std
    end

val enter = Symbol.enter val look  = Symbol.look end 
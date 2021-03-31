signature ENV =
sig
    type acces
    type ty
    datatype enventry = VarEntry of {ty : ty}
           |  FunEntry of {formals : ty list, result : ty}
    val base_tenv : ty Symbol.table (* name -> type e.g. int -> INT *)
    val base_venv : enventry Symbol.table (* name -> type-of-exp *)
end

signature ENV =
sig
    type acces
    type ty
    datatype enventry = ValEntry of {ty : ty}
                      | FunEntry of {formals : ty list, result : ty}
    val base_tenv : ty Symbol.table  (* predefined types*)
    val base_venv : enventry Synbol.table (* predefined functions / values *)
end

structure env :> ENV =
struct
type acces
end

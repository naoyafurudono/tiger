structure Main =
struct
fun main(file : name) =
let 
  val ast : Absyn.exp = Parse.parse file in
Semant.transExp (Env.basevenv, Env.tenv, ast)
end
structure Main =
struct
fun main(file : string) =
let 
  val ast : Absyn.exp = Parse.parse file 
in
  Semant.transProg ast
end
end
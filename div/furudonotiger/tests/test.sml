structure Test :
          sig
              val test : unit -> unit
              val test_ast : string list -> Absyn.exp list
              val tests : string list
          end
=
struct
val tests = 
[
    "seq.tig"
   ,"var.tig"
   ,"record.tig"
   ,"array.tig"
   ,"array_error.tig"  (* expect int, actual string*)
   ,"while.tig"
   ,"for.tig"
   ,"test.tig"
]
val prefix = "tests/"
fun test()=
    List.app 
        (fn (t) => (
             print ("------ Test : " ^ t ^ " ------\n");
             Main.main (prefix ^ t);
             print ("\n++++++++++++++++++++++++++++++++\n")
        ))
        tests
fun test_ast(tests : string list)=
    List.map
        (fn (t) => (
             print ("------ Test : " ^ t ^ " ------\n");
             Main.main (prefix ^ t)
        ))
        tests
end

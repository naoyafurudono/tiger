structure Test :
          sig
              val test : unit -> unit
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
end

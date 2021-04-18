structure Test :
          sig
              val test : unit -> unit
          end
=
struct
val tests = [
    "seq.tig"
   ,"var.tig"
   ,"record.tig"
   ,"array.tig"
   ,"array_error.tig"  (* expect int, actual string*)
        (* ,"test.tig"  *)
        (* infinite loop now... *)
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

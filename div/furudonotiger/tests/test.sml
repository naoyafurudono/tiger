structure Test :
          sig
              val test : unit -> unit
              val parse : string list -> Absyn.exp list
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
   ,"break.tig"
   ,"break_bad.tig"
   ,"mutual.tig"
   ,"single.tig"
   ,"testcases/merge.tig"
   ,"rec_ty.tig"
   ,"tmp.tig"
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
fun parse(tests : string list)=
    List.map
        (fn (t) => (
             print ("------ Parse : " ^ t ^ " ------\n");
             Parse.parse (prefix ^ t)))
        tests
end

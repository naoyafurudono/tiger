signature SEMANT =
sig
    type venv = Env.enventry Symbol.table
    type tenv = ty Symbol.table
    type expty

    val transVar : venv * tenv * Absyn.var -> expty
    val transExp : venv * tenv * Absyn.exp -> expty
    val transDecs : venv * tenv * Absyn.dec list -> {venv : venv, tenv : tenv}
    val transTy : venv * tenv * Absyn.ty -> T.ty
end

structure A = Absyn
structure E = Env
structure S = Symbol
structure T = Types
structure Semant :> SEMANT =
struct
type expty = {exp : Translate.exp, ty : T.ty}

fun transExp(venv, tenv, exp) : expty = 
   let
     fun trvar A.SimpleVar(id, pos) = case S.look (venv, id) of
       SOME(E.VarEntry{ty}) => {exp = (), ty = actual_ty ty}
       NONE => (error pos ("undefined variable " ^ S.name id))
      |  trvar A.FieldVar(v, s, pos) = ...
      |  trvar A.SubscriptVar(v, e, pos) = ...
   and
     fun trexp A.VarExp v = trvar v
     | trexp A.NilExp = 
     | trexp A.IntExp i =
     | trexp A.StringExp (str, pos) =
     | trexp A.CallExp {func = f, args = args, pos = pos} =
     | trexp A.RecordExp {fields = fields, typ = typ, pos=pos} =
     | trexp A.SeqExp exp_poss =
     | trexp A.AssignExp {var, exp, pos} =
     | trexp A.IfExp {test = test, then'=then', else'=(SOME else'), pos=pos} =
     | trexp A.IfExp {test = test, then'=then', else'=NONE, pos=pos} = 
     | trexp A.WhileExp {test, body, pos} =
     | trexp A.ForExp
     | trexp A.BreakExp
     | trexp A.LetExp {decs, body, pos} =
         let val {venv = venv', tenv = tenv'} = transDec (venv, tenv, decs)
         in transExp (venv', tenv') body
     | trexp A.ArrayExp
   (* OpExp *)
     | trexp A.OpExp{left, oper=A.PlusOp, right, pos} = (
         checkInt(trexp left, pos);
         checkInt(trexp right, pos);
         {exp = (), ty = T.INT}
     )
     | trexp A.OpExp{left, oper=A.MinusOp, right, pos} = (
         checkInt(trexp left, pos);
         checkInt(trexp right, pos);
         {exp = (), ty = T.INT}
     )
     | trexp A.OpExp{left, oper=A.TimesOp, right, pos} = (
         checkInt(trexp left, pos);
         checkInt(trexp right, pos);
         {exp = (), ty = T.INT}
     )
     | trexp A.OpExp{left, oper=A.DivideOp, right, pos} = (
         checkInt(trexp left, pos);
         checkInt(trexp right, pos);
         {exp = (), ty = T.INT}
     )
     | trexp A.OpExp{left, oper=A.EqOp, right, pos} = (
         checkInt(trexp left, pos);
         checkInt(trexp right, pos);
         {exp = (), ty = T.INT}
     )
     | trexp A.OpExp{left, oper=A.NeqOp, right, pos} = (
         checkInt(trexp left, pos);
         checkInt(trexp right, pos);
         {exp = (), ty = T.INT}
     )
     | trexp A.OpExp{left, oper=A.LtOp, right, pos} = (
         checkInt(trexp left, pos);
         checkInt(trexp right, pos);
         {exp = (), ty = T.INT}
     )
     | trexp A.OpExp{left, oper=A.LeOp, right, pos} = (
         checkInt(trexp left, pos);
         checkInt(trexp right, pos);
         {exp = (), ty = T.INT}
     )
     | trexp A.OpExp{left, oper=A.GtOp, right, pos} = (
         checkInt(trexp left, pos);
         checkInt(trexp right, pos);
         {exp = (), ty = T.INT}
     )
     | trexp A.OpExp{left, oper=A.GeOp, right, pos} = (
         checkInt(trexp left, pos);
         checkInt(trexp right, pos);
         {exp = (), ty = T.INT}
     )
   
   and
     fun trty (A.NameTy (id, pos)) =
     | trty (A.RecordTy fields) = 
     | trty (A.ArrayTy (id, pos)) =
    and
fun checkInt ({exp = e, ty = ty}, pos) = check({e,ty}, T.INT, pos, "integer expeceted!")
fun check ({exp = _, ty = ty}, expected_ty : T.ty, pos, err_msg) = if ty = expected_ty then () else error pos err_msg
in
trexp exp
end
fun transDecs (tenv : tenv, venv : venv, decs : A.dec list) : {tenv, venv} =
  let 
     fun transDec (tenv, venv, A.FunctionDec ({name, params, SOME(resty, resty_pos), body_exp, pos} :: tail)) = 
     | transDec (tenv, venv, A.FunctionDec ({name, params, NONE, body_exp, pos}::tail)) = 
     | transDec (tenv, venv, A.Vardec {name, escape, init, pos, typ=SOME(id, id_pos)}) =
     | transDec (tenv, venv, A.TypeDec ({id, ty, pos}::tail)) =
  in case decs of
      dec :: tail => 
      let 
        val {tenv = tenv', venv = venv'} = transDec (tenv, venv, dec)
        in transDecs (tenv', venv', tail) end
    | [] => {tenv = tenv, venv = venv} 
  end
    


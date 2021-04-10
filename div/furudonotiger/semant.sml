signature SEMANT =
sig
    (* venv = "今の" 値変数の名前とその型の対応。ASTではなく、識別子に型を対応させるだけ. *)
    type venv = Env.enventry Symbol.table
    (* tenv = "今の型の識別子にその抽象構文木を対応させる *)
    type tenv = ty Symbol.table
    (* 出力される中間表現とその型 *)
    type expty
    (* 変数のASTを変換する *)
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
(* *)
type expty = {exp : Translate.exp, ty : T.ty}

(* *)
fun transVar(venv:venv , tenv: tenv , var: A.var) =
    let
        fun trvar A.SimpleVar(id, pos): expty = (case S.look (venv, id) of
                                              SOME(E.VarEntry{ty}) => {exp = (), ty = actual_ty ty}
                                            | NONE => (error pos ("undefined variable " ^ S.name id))
                                         )
         |  trvar A.FieldVar(v, sym, pos) = (* vはレコード型の変数でsym フィールドを持つことを期待。そのときそのフィールドの型がこのfieldVarの型 *)
           let 
             val {exp = var_exp, ty = var_ty} = trvar v
             in (case var_ty of 
                  T.RECORD(fields, _) =>
                    let
                      val typ = (case (find (fn(label, typ)=> label = sym) fields) of
                                   NONE => error pos ("undefined field " ^ S.name sym)
                                 | SOME(typ) => typ)
                      val exp = ()
                    in
                      {exp=exp, ty=typ}
                    end
                 | _ => error pos "Expect record, but found " show var_ty)
         |  trvar A.SubscriptVar(v, e, pos) = (* vはArray型の変数であることを期待。*)
           let
             val {exp=var_exp, ty=var_ty} = trvar v
             val typ = (case var_ty of
                         T.ARRAY(typ, _) => typ
                         | _ => error pos "Expect Array, but find " ^ show var_ty)
           in
             {exp=(), ty=typ}
           end
         in trvar v
    end
fun transExp(venv:venv , tenv: tenv , exp: A.exp) : expty =
    let
        fun trexp A.VarExp v = transVar(venv, tev, v)
          | trexp A.NilExp = {exp=(), ty = T.NIL}
          | trexp A.IntExp i = {exp=(), ty = T.NIL}
          | trexp A.StringExp (str, pos) = {exp = (), ty = T.STRING}
          | trexp A.CallExp {func = f_sym, args = arg_exps, pos = pos} = 
              let 
                val f_ty = (case (look(venv, f_sym)) of
                             E.FunEntry{formals = param_tys, reset = res_ty} => 
                               let
                                 fun check
                                
                               in app check {exp=(), res_ty}
                            | E.VarEntry{ty=ty} => error pos "Expect function type variable, find normal variable of " ^ (show ty)

          | trexp A.RecordExp {fields = fields, typ = typ, pos=pos} =
          | trexp A.SeqExp exp_poss =
          | trexp A.AssignExp {var, exp, pos} =
          | trexp A.IfExp {test = test, then'=then', else'=(SOME else'), pos=pos} =
          | trexp A.IfExp {test = test, then'=then', else'=NONE, pos=pos} =
          | trexp A.WhileExp {test, body, pos} =
          | trexp A.ForExp =
          | trexp A.BreakEx =
          | trexp A.LetExp {decs, body, pos} =
            let
                val {venv = venv', tenv = tenv'} = transDec (venv, tenv, decs)
            in transExp (venv', tenv') body
             | trexp A.ArrayExp () =
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
               fun trty (A.NameTy (id, pos)) =
                 | trty (A.RecordTy fields) =
                 | trty (A.ArrayTy (id, pos)) =
            in
                trexp exp
               end
               fun checkInt ({exp = e, ty = ty}, pos) = check(ty, T.INT, pos, "integer expeceted!")
               fun check (given_ty : T.ty, expected_ty : T.ty, pos, err_msg) = if give_ty = expected_ty then () else error pos err_msg
               fun transDecs (venv : venv, tenv : tenv, decs : A.dec list) : {tenv, venv} =
                   let
                       fun transDec (venv, tenv, A.FunctionDec ({name, params, SOME(resty, resty_pos), body_exp, pos} :: tail)) =
                         | transDec (venv, tenv, A.FunctionDec ({name, params, NONE, body_exp, pos}::tail)) =
                         | transDec (venv, tenv, A.Vardec {name, escape, init, pos, typ=SOME(ty)}) =
                           let val {exp, ty} = transExp (venv, tenv, init)
                           in check{given = transTy(venv, tenv, )}{tenv = tenv, venv = S.enter (venv, name, E.VarExp{ty = ty})}
                           end
                         | transDec (venv, tenv, A.Vardec {name, escape, init, pos, typ=NONE}) =
                           let val {exp, ty} = transExp (venv, tenv, init)
                           in {tenv = tenv, venv = S.enter (venv, name, E.VarExp{ty = ty})}
                           end
                         | transDec (tenv, venv, A.TypeDec ({id, ty, pos}::tail)) =
                   in case decs of
                          dec :: tail =>
                          let
                              val {tenv = tenv', venv = venv'} = transDec (tenv, venv, dec)
                          in transDecs (tenv', venv', tail) end
                        | [] => {tenv = tenv, venv = venv}
                    end

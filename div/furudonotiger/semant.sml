signature SEMANT =
sig
    (* venv = "今の" 値変数の名前とその型の対応。ASTではなく、識別子に型を対応させるだけ. *)
    type venv = Env.enventry Symbol.table
    (* tenv = "今の型の識別子にその抽象構文木を対応させる *)
    type tenv = Types.ty Symbol.table
    (* 出力される中間表現とその型 *)
    type expty
    (* ASTの型検査をして、型エラーや宣言のない変数の使用を報告する *)
    val transProg : Absyn.exp -> unit
                                     (* val transVar : venv * tenv * Absyn.var -> expty *)
                                     (* val transExp : venv * tenv * Absyn.exp -> expty *)
                                     (* val transDecs : venv * tenv * Absyn.dec list -> {venv : venv, tenv : tenv} *)
                                     (* val transTy : tenv * Absyn.ty -> T.ty *)
end

structure A = Absyn
structure E = Env
structure T = Types
structure S = Symbol


structure Semant :> SEMANT =
struct
(* *)
type expty = {exp : Translate.exp, ty : T.ty}
type venv = Env.enventry Symbol.table
type tenv = Types.ty Symbol.table

(* *)

exception TransError of string
fun error (pos : A.pos, message : string) =
    let 
        val pretty_msg = 
            "Position : " ^ (Int.toString pos) ^"\n" ^
            message
    in
        raise (TransError pretty_msg)
    end



fun transVar(venv : venv , tenv: tenv , var: A.var): expty =
    let
        fun trvar(A.SimpleVar(id, pos)): expty = 
            (case S.look (venv, id) of
                 SOME(E.VarEntry{ty=(ty)}) => {exp = (), ty = ty}
               | _ => (error (pos, ("undefined variable " ^ S.name id)))
            )
         |  trvar (A.FieldVar(v, sym, pos)) = (* vはレコード型の変数でsym フィールドを持つことを期待。そのときそのフィールドの型がこのfieldVarの型 *)
            let 
                val {exp = var_exp, ty = var_ty} = trvar v
            in 
                (case var_ty of 
                     T.RECORD(fields, _) =>
                     let
                         val typ = (case (List.find (fn(label, typ)=> label = sym) fields) of
                                        NONE => error(pos, ("undefined field " ^ S.name sym))
                                      | SOME(lab, typ) => typ)
                         val exp = ()
                     in
                         {exp=exp, ty=typ}
                     end
                   | _ => error (pos, "Expect record, but found " ^ (T.show var_ty)))
            end
         |  trvar (A.SubscriptVar(v, e, pos)) = (* vはArray型の変数、eはintであることを期待。*)
            let
                val {exp=var_exp, ty=var_ty} = trvar v
                val typ = (case var_ty of
                               T.ARRAY(typ, _) => typ
                             | _ => error(pos, "Expect Array, but find " ^ T.show var_ty))
                val {exp=iexp, ty=ity} = transExp (venv, tenv, e)
            in
                {exp=(), ty=typ}
            end
    in 
        trvar var
    end

and transExp(venv:venv , tenv: tenv , exp: A.exp) : expty =(
    let
        fun trexp (A.VarExp v) = transVar(venv, tenv, v)
          | trexp (A.NilExp) = {exp=(), ty = T.NIL}
          | trexp (A.IntExp i) = {exp=(), ty = T.INT}
          | trexp (A.StringExp (str, pos)) = {exp = (), ty = T.STRING}
          | trexp (A.CallExp {func = f_sym, args = arg_exps, pos = pos}) = 
            let 
                val f_ty : T.ty = (case E.look(venv, f_sym) of
                                       SOME(Env.FunEntry({formals, result})) => 
                                       let
                                           fun check{exp = e, ty = t} = ()
                                       in 
                                           check {exp=(), ty = result};
                                           result
                                       end
                                     | SOME(E.VarEntry{ty=ty}) => error(pos, "Expect function type variable, find normal variable of " ^ (T.show ty))
                                     | NONE => error(pos, "unbound function"))
            in 
                {exp = (), ty = f_ty}
            end
          | trexp (A.RecordExp {fields = fields, typ = typ, pos=pos}) = 
            let
                fun fieldToTy(s : A.symbol, e : A.exp, pos) : S.symbol * T.ty = (
                    let
                        val {exp = exp, ty=ty} = trexp e
                    in
                        (s, ty)
                    end
                )
                val fieldTypeBindings = map fieldToTy fields
            in
                (* TODO Type check record!! checkRecord typ fieldTypeBindings; *)
                {exp=(), ty = T.RECORD(fieldTypeBindings, ref ())}
            end
          | trexp (A.SeqExp (e1, e2, pos)) =
            let 
                val {exp=et1, ty=tty1} = trexp e1
                val {exp=et2, ty=tty2} = trexp e2
            in
                {exp=(), ty=tty2}
            end
          | trexp (A.AssignExp {var : A.var, exp : A.exp , pos : A.pos}) =
            let 
                val {exp= tVarE, ty = tVarTy} = transVar (venv, tenv, var)
                val {exp= tExpE, ty = tExpTy} = trexp exp
            in
                check (tVarTy, tExpTy, pos);
                {exp=(), ty=T.NIL}
            end
          | trexp (A.IfExp {test = test, then'=then', else'=(SOME else'), pos=pos}) =
            let 
                val {exp = tTestExp, ty = tTestTy} = trexp test
                val {exp = tE1Exp, ty = tE1Ty} = trexp then' 
                val {exp = tE2Exp, ty = tE2Ty} = trexp else'
            in
                check(T.INT, tTestTy, pos);
                check(tE1Ty, tE2Ty, pos);
                {exp=(), ty = tE1Ty}
            end
          | trexp (A.IfExp {test = test, then'=then', else'=NONE, pos=pos}) =
            let 
                val {exp = tTestExp, ty = tTestTy} = trexp test
                val {exp = tE1Exp, ty = tE1Ty} = trexp then' 
            in
                check(T.INT, tTestTy, pos);
                check(tE1Ty, T.NIL, pos);
                {exp=(), ty=T.NIL}
            end
          | trexp (A.WhileExp {test, body, pos}) =
            let 
                val {exp = tTestExp, ty = tTestTy} = trexp test
                val {exp = tBodyExp, ty = tBodyTy} = trexp body
            in
                check(T.INT, tTestTy, pos);
                check(T.NIL, tBodyTy, pos);
                {exp=(), ty = T.NIL}
            end
          | trexp (A.ForExp {var = varSym, lo=loExp, hi=hiExp, body=bodyExp, pos=pos, ...}) =
            let 
                val {exp = tHiExp, ty = tHiTy} = trexp hiExp
                val {exp = tLoExp, ty = tLoTy} = trexp loExp
            in
                check(T.INT, tHiTy, pos);
                check(T.NIL, tLoTy, pos);
                transExp (E.enter(venv, varSym, Env.VarEntry{ty =T.INT}), tenv, bodyExp)
            end
          | trexp (A.BreakExp(pos)) = {exp=(), ty=T.NIL} (* TODO : Check the context of break is while or for *)
          | trexp (A.LetExp {decs, body, pos}) =
            let
                val {venv = venv', tenv = tenv'} = transDecs (venv, tenv, decs)
            in 
                transExp (venv', tenv', body)
            end
          | trexp (A.ArrayExp {typ = typSym, size = sizeExp, init=initExp, pos=pos}) = 
            let
                val {exp=tSizeExp, ty=tSizeTy} = trexp sizeExp
                val {exp=tInitExp, ty=tInitTy} = trexp initExp
            in
                check(T.INT, tSizeTy, pos);
                {exp=(), ty= T.ARRAY(tInitTy, ref ())}
            end
          (* OpExp *)
          | trexp (A.OpExp{left, oper, right, pos}) =
            let
                fun checkIntBinOp(left, right, pos) = (
                    checkInt(trexp left, pos);
                    checkInt(trexp right, pos);
                    {exp = (), ty = T.INT})
            in
                (case oper of
                     A.PlusOp =>    ()
                   | A.MinusOp =>   ()
                   | A.TimesOp =>   ()
                   | A.DivideOp =>  ()
                   | A.EqOp =>      ()
                   | A.NeqOp =>     ()
                   | A.LtOp =>      ()
                   | A.LeOp =>      ()
                   | A.GtOp =>      ()
                   | A.GeOp =>      ());
                   (* | _ => error(pos, "Undefined binary operation")); *)
                checkIntBinOp(left, right, pos)
            end
    in
        trexp exp
    end)
and checkInt({exp = e, ty = ty}, pos) = check(ty, T.INT, pos)
and check (given_ty : T.ty, expected_ty : T.ty, pos): bool = 
    if given_ty = expected_ty then true 
    else error(pos, ("Type error\n" ^ "  expect : " ^ (T.show expected_ty) ^ "\n  given : " ^ (T.show given_ty) ^"\n"))

and transDecs (venv:venv, tenv:tenv, decs : A.dec list) : {tenv : tenv, venv : venv} =
    let
        fun transDec (venv, tenv, A.FunctionDec fundecs) : {tenv : tenv, venv : venv} = 
            let fun transfundec(venv : venv, tenv : tenv, fundecs) : {tenv : tenv, venv : venv} =
                    (case fundecs of 
                         ({name=funNameSym, params=funParams, body=funBodyExp, pos=pos, result=funResTy} :: tail) =>
                         let
                             fun transRt rt : T.ty =
                                 (case E.look (tenv, rt) of
                                      NONE => error(pos, "function result type undefined")
                                    | SOME(tResTy) => tResTy)
                             val tResTy = (case funResTy of
                                               NONE => T.NIL
                                             | SOME(rt, posRt) => transRt rt)
                             fun transParam{name = nameSym, typ=typSym, pos=pos, escape = esc} : {name : S.symbol, ty : T.ty} = 
                                 (case E.look (tenv, typSym) of 
                                      SOME(ty) => {ty=ty, name = nameSym} 
                                    | _      => error(pos, "Parameter type is not defined"))
                             val tparams = map transParam funParams
                             val venv' = E.enter (venv, funNameSym, E.FunEntry{formals = map #ty tparams, result=tResTy})
                             fun enterParam ({name =name, ty = ty}, venv): venv = E.enter(venv, name, E.VarEntry{ty=ty})
                             val venv'' = foldl enterParam venv' tparams
                             val {exp = tBodyExp, ty = tBodyTy} = transExp (venv, tenv, funBodyExp)
                         in
                             check(tResTy, tBodyTy, pos);
                             transfundec (venv', tenv, tail)
                         end
                       | [] => {venv=venv, tenv=tenv})
            in 
                transfundec(venv, tenv, fundecs)
            end
          | transDec (venv, tenv, (A.VarDec {name, escape, init, pos, typ})) =
            let
                val {exp=tInitExp, ty=tInitTy} = transExp (venv, tenv, init)
            in 
                (case typ of
                     SOME(sym, pos) => 
                     (case Env.look(tenv, sym) of
                          SOME(ty) => (
                           check(ty, tInitTy, pos);
                           {tenv = tenv, venv = E.enter (venv, name, E.VarEntry{ty = tInitTy})}
                       )
                        | NONE => error(pos, "type not recognized"))
                   | NONE => (
                       check(tInitTy, T.NIL, pos);
                       {venv = Env.enter(venv, name, Env.VarEntry{ty = tInitTy}), tenv = tenv}
                ))
            end
          | transDec (venv, tenv, A.TypeDec tydecs)  = 
            let fun transtydec(venv, tenv, tydecs) =
                    (case tydecs of 
                         {id, ty, pos}::tail => transtydec(venv, E.enter (tenv, id, transTy(tenv, ty)), tail)
                       | [] => {venv = venv, tenv = tenv})
            in 
                case decs of
                    dec :: tail => 
                    let
                        val {tenv = tenv', venv = venv'} = transDec (venv, tenv, dec)
                    in 
                        transDecs (venv', tenv', tail) 
                    end
                  | [] => {tenv = tenv, venv = venv}
            end
    in
        foldl (fn (decl, {venv, tenv}) => transDec(venv, tenv, decl)) 
              {venv = venv, tenv = tenv} 
              decs
    end

and transTy (tenv: tenv, A.NameTy (id, pos) : A.ty) : T.ty = (case E.look (tenv, id) of SOME(ty) => ty | NONE => error(pos, "type not recognized"))
  | transTy (tenv, A.RecordTy fields) =  T.INT
  | transTy (tenv, A.ArrayTy (id, pos)) = T.INT

fun transProg e = 
    let
        val {exp = e, ty = t} = (
            transExp(Env.base_venv, Env.base_tenv, e)
            handle TransError msg => (print msg; {exp = (), ty = T.NIL}))
    in
        ()
    end

end

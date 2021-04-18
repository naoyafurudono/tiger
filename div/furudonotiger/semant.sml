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


structure Semant : SEMANT =
struct
type expty = {exp : Translate.exp, ty : T.ty}
type venv = Env.enventry Symbol.table
type tenv = Types.ty Symbol.table

(* ------------- Error in this module ----------------- *)
exception TransError of string
exception Semant of string

fun error (pos : A.pos, message : string) =
    let
        val pretty_msg =
            "Position : " ^ (Int.toString pos) ^"\n" ^ message
    in
        raise (TransError pretty_msg)
    end
val debug_flag = false
val debug_counter = ref 0
fun printD(msg : string) : unit = (
    if debug_flag
    then let
        val id = Int.toString (!debug_counter)
        val prefix = "\n---- Debug info " ^ id ^" ----\n"
        val postfix = "\n-----"^ id ^"------\n"
    in
        debug_counter := (!debug_counter) + 1;
        print(prefix ^ msg ^ postfix)
    end
    else ()
)

(* ------------- Main Definition ------------- *)
(* breakable = Break can be used within while and for *)
val breakable = ref false
fun transVar(venv : venv , tenv: tenv , var: A.var): expty =
    let
        fun trvar(A.SimpleVar(id, pos)): expty = 
            (case S.look (venv, id) of
                 SOME(E.VarEntry{ty=(ty)}) => (printD("Find simple variable "^ (T.show ty));{exp = (), ty = ty})
               | _ => (error (pos, ("undefined variable " ^ S.name id))))
         |  trvar (A.FieldVar(v, sym, pos)) = (* レコード変数vのsymフィールドへのアクセス *)
            let
                val {exp = var_exp, ty = var_ty} = trvar v
            in
                (case var_ty of
                     T.RECORD(fields, _) =>
                     let
                         val typ = (case (List.find (fn(label, typ)=> label = sym) fields) of
                                        NONE => error(pos, ("record " ^ "undefined field " ^ (S.name sym)))
                                      | SOME(lab, typ) => typ)
                         val exp = ()
                     in
                         (printD ("Find record variable" ^ (T.show var_ty));
                          {exp=exp, ty=typ})
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
                (check(T.INT, ity, pos);  (* Type of index must be INT *)
                 printD("Find array variable " ^ (T.show typ));
                 {exp=(), ty=typ})
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
                                       SOME(Env.FunEntry({formals=param_tys, result=result_ty})) => 
                                       let
                                           val arg_tys = map (fn(e) => #ty (trexp e)) arg_exps
                                           fun check_arg_ty(param_tys, arg_tys) = (ListPair.allEq (fn (x, y) => check(x, y, pos))
                                                                                                  (param_tys, arg_tys))

                                       in 
                                           if List.length param_tys = List.length arg_tys andalso
                                              check_arg_ty (param_tys, arg_tys)
                                           then ()
                                           else error(pos, "Call : arity unmatch")
                                         ;
                                           result_ty
                                       end
                                     | SOME(E.VarEntry{ty=ty}) => error(pos, "Expect function type variable, find normal variable of " ^ (T.show ty))
                                     | NONE => error(pos, "unbound function"))
            in 
                {exp = (), ty = f_ty}
            end
          | trexp (A.RecordExp {fields = fields, typ = typ_sym, pos=pos}) = 
            let
                val rec_ty : T.ty = (case Env.look(tenv, typ_sym) of 
                                         SOME(ty) => ty
                                       | _ => error(pos, "type undefined"))
                val type_field : (Symbol.symbol * T.ty) list =
                    (case rec_ty of
                         T.RECORD(expected_flds, _) => expected_flds
                       | _ => error(pos, "Record type expected!"))
                fun fieldToTy(s : A.symbol, e : A.exp, pos) : S.symbol * T.ty = (
                    let
                        val {exp = exp, ty=ty} = trexp e
                    in
                        (s, ty)
                    end
                )
                val fieldTypeBindings : (A.symbol * T.ty) list = map fieldToTy fields
                fun remove_field(fld : A.symbol, fld_lst) = (case fld_lst of
                                                                 [] => error(pos, "sorry, internal error rmeove (semant.sml // check_record_matching)")
                                                               | (fld', ty)::ys => if fld = fld' then ys else (fld', ty) :: remove_field(fld, ys)
                                                            )
                fun check_record_matching (expected, actual) = (case (expected, actual) of
                                                                    ([], []) => true
                                                                  | ((fld, ty) :: xs, lst) => (
                                                                      if not (List.exists 
                                                                                  (fn (fld', ty') => fld = fld' andalso (check(ty, ty', pos)))
                                                                                  lst)
                                                                      then error(pos, "illegal record creation! " ^  (S.name fld))
                                                                      else check_record_matching(xs, remove_field(fld, lst)))
                                                                  | _ => (
                                                                      error(pos, "Record type unmatch!")))
            in
                check_record_matching(type_field, fieldTypeBindings);
                {exp=(), ty = rec_ty}
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
                {exp=(), ty=T.VOID}
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
                check(tE1Ty, T.VOID, pos);
                {exp=(), ty=T.VOID}
            end
          | trexp (A.WhileExp {test, body, pos}) =
            
            let
                val restore_breakable = !breakable
                val {exp = tTestExp, ty = tTestTy} = trexp test
                val {exp = tBodyExp, ty = tBodyTy} = (breakable := true;trexp body)
            in
                breakable := restore_breakable;
                check(T.INT, tTestTy, pos);
                check(T.VOID, tBodyTy, pos);
                {exp=(), ty = T.VOID}
            end
          | trexp (A.ForExp {var = varSym, lo=loExp, hi=hiExp, body=bodyExp, pos=pos, ...}) =
            let 
                val restore_breakable = !breakable
                val {exp = tHiExp, ty = tHiTy} = trexp hiExp
                val {exp = tLoExp, ty = tLoTy} = trexp loExp
            in
                breakable := true;
                check(T.INT, tHiTy, pos);
                check(T.INT, tLoTy, pos);
                let 
                    val res = transExp (E.enter(venv, varSym, Env.VarEntry{ty = T.INT}), tenv, bodyExp)
                in
                    breakable := restore_breakable;
                    res
                end
            end
          | trexp (A.BreakExp(pos)) = (
              if !breakable then 
                  {exp=(), ty=T.VOID} (* TODO : Check the context of break is while or for *)
              else error(pos, "Bad break use")
          )
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
                fun checkEq(left, right, pos) = (
                       let
                         val {exp = expl, ty = tyl} = trexp left
                         val {exp = expr, ty = tyr} = trexp right
                       in
                       (check(tyl, tyr, pos);
                        {exp = (), ty = T.INT})
                       end)
            in
                (case oper of
                     A.PlusOp =>
                     checkIntBinOp(left, right, pos)
                   | A.MinusOp =>
                     checkIntBinOp(left, right, pos)
                   | A.TimesOp =>
                     checkIntBinOp(left, right, pos)
                   | A.DivideOp =>
                     checkIntBinOp(left, right, pos)
                   | A.EqOp => 
                     checkEq(left, right, pos)
                   | A.NeqOp =>
                     checkEq(left, right, pos)
                   | A.LtOp =>
                     checkIntBinOp(left, right, pos)
                   | A.LeOp =>
                     checkIntBinOp(left, right, pos)
                   | A.GtOp =>
                     checkIntBinOp(left, right, pos)
                   | A.GeOp =>
                     checkIntBinOp(left, right, pos))
                                  (* | _ => error(pos, "Undefined binary operation")); *)
                 end
                 in
                     trexp exp
                 end)
                and checkInt({exp = e, ty = ty}, pos) = check(ty, T.INT, pos)
                and check (expected_ty : T.ty, given_ty : T.ty, pos): bool = 
                    if given_ty = expected_ty then true 
                    else error(pos, ("Type error\n" ^ "  expect : " ^ (T.show expected_ty) ^ "\n  given : " ^ (T.show given_ty) ^"\n"))

                and transDecs (venv:venv, tenv:tenv, decs : A.dec list) : {tenv : tenv, venv : venv} =
                    let
                        fun transDec (venv, tenv, A.FunctionDec fundecs) : {tenv : tenv, venv : venv} = 
                            let
                                fun gather_signature(venv : venv, tenv : tenv, fundecs : A.fundec list) : {venv : venv, tenv : tenv} = (
                                    case fundecs of 
                                        ({name=funNameSym, params=funParams, body=funBodyExp, pos=pos, result=funResTy} :: tail) =>
                                        let
                                            fun transRt rt : T.ty =
                                                (case E.look (tenv, rt) of
                                                     NONE => error(pos, "function result type undefined")
                                                   | SOME(tResTy) => tResTy)
                                            val tResTy = (case funResTy of
                                                              NONE => T.VOID
                                                            | SOME(rt, posRt) => transRt rt)
                                            fun transParam{name = nameSym, typ=typSym, pos=pos, escape = esc} : {name : S.symbol, ty : T.ty} = 
                                                (case E.look (tenv, typSym) of 
                                                     SOME(ty) => {ty=ty, name = nameSym} 
                                                   | _      => error(pos, "Parameter type is not defined"))
                                            val tparams = map transParam funParams
                                            val venv' = E.enter (venv, funNameSym, E.FunEntry{formals = map #ty tparams, result=tResTy})
                                        in
                                            gather_signature(venv', tenv, tail)
                                        end
                                      | [] => {venv=venv, tenv=tenv}) 
                                fun transfundec(venv : venv, tenv : tenv, fundecs : A.fundec) : unit =
                                    (*
                                     * venvはこの宣言ブロックのシグネチャを含んでいる
                  TODO
                  * パラメータでvenvを拡張
                  * body を型検査
                  *)
                                    (case fundecs of 
                                         {name=funNameSym, params=funParams, body=funBodyExp, pos=pos, result=funResTy} =>
                                         let
                                             fun transParam{name = nameSym, typ=typSym, pos=pos, escape = esc} : {name : S.symbol, ty : T.ty} = 
                                                 (case E.look (tenv, typSym) of 
                                                      SOME(ty) => {ty=ty, name = nameSym} 
                                                    | _      => error(pos, "Parameter type is not defined"))
                                             val tparams = map transParam funParams
                                             fun enterParam ({name =name, ty = ty}, venv): venv = E.enter(venv, name, E.VarEntry{ty=ty})
                                             fun transRt rt : T.ty =
                                                 (case E.look (tenv, rt) of
                                                      NONE => error(pos, "function result type undefined")
                                                    | SOME(tResTy) => tResTy)
                                             val tResTy = (case funResTy of
                                                               NONE => T.VOID
                                                             | SOME(rt, posRt) => transRt rt)
                                             val venv' = foldl enterParam venv tparams
                                             val {exp = tBodyExp, ty = tBodyTy} = transExp (venv', tenv, funBodyExp)
                                         in
                                             (check(tResTy, tBodyTy, pos); ())
                                         end)
                                (* | _ => raise (Semant "transfundec")) *)
                                val {venv = venv', tenv = tenv'} = gather_signature(venv, tenv, fundecs)
                            in 
                                (
                                  List.app (fn (fundec) => transfundec(venv', tenv', fundec)) fundecs;
                                  {venv = venv', tenv = tenv'}
                                )
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
                                           {tenv = tenv, venv = E.enter (venv, name, E.VarEntry{ty = tInitTy})})
                                        | NONE => error(pos, "type not recognized"))
                                   | NONE => (* w/o type annotation *) 
                                     {tenv = tenv, venv = Env.enter(venv, name, Env.VarEntry{ty = tInitTy})})
                            end
                          | transDec (venv, tenv, A.TypeDec tydecs)  = 
                            let fun transtydec(venv, tenv, tydecs : Absyn.tydec list) =
                                    (case tydecs of 
                                         {name=id, ty, pos}::tail => transtydec(venv, E.enter (tenv, id, transTy(tenv, ty)), tail)
                                       | [] => {venv = venv, tenv = tenv})
                            in 
                                transtydec(venv, tenv, tydecs)
                            end
                    in
                        foldl (fn (decl, {venv, tenv}) => (
                                   (* printD("\n dec\n"); *)
                                   transDec(venv, tenv, decl)))
                              {venv = venv, tenv = tenv}
                              decs
                    end

                and transTy (tenv: tenv, A.NameTy (id, pos) : A.ty) : T.ty = (
                    case E.look (tenv, id) of SOME(ty) => ty | NONE => error(pos, "type not recognized"))
                  | transTy (tenv, A.RecordTy fields) = (
                      T.RECORD(map (fn {name = name, typ = typ, pos = pos, ...} =>(name, get_ty(typ, tenv, pos))) fields, ref ())
                  )
                  | transTy (tenv, A.ArrayTy (id, pos)) = T.ARRAY(get_ty(id, tenv, pos), ref ())
                and get_ty (typ : A.symbol, tenv : tenv, pos : A.pos) : T.ty = (
                    case E.look(tenv, typ) of
                        SOME(ty) => ty
                      | NONE => error(pos, "Type name undefined " ^ (S.name typ) ^"\n")
                )

                fun transProg e = 
                    let
                        val {exp = e, ty = t} = (
                            transExp(Env.base_venv, Env.base_tenv, e)
                            handle TransError msg => (print msg; {exp = (), ty = T.VOID}))
                    in
                        ()
                    end

            end

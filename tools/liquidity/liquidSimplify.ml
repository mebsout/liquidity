(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

let compute code to_inline =

  let rec iter exp =
    match exp.desc with
    | Const _ -> exp
    | Var (name, _loc, []) ->
       begin
         try
           let v = StringMap.find name to_inline in
           assert (not v.fail);
           iter v
         with Not_found -> exp
       end
    | Var (name, _loc, _::_) -> assert false
    | SetVar (name, _loc, _, _) -> assert false
    | Let (name, loc, v, body) ->
       let body = iter body in
       if StringMap.mem name to_inline then
         body
       else
         let v = iter v in
         begin
           try
             if StringSet.mem name (LiquidBoundVariables.bv body) then
               raise Exit;
             if not v.fail then
               body
             else
               if v.ty <> Tunit then raise Exit
               else
                 { exp with desc = Seq(v, body) }
           with Exit ->
             { exp with desc = Let(name, loc, v, body) }
         end

    | MatchOption(arg, loc, ifnone, name, ifsome) ->
       let arg = iter arg in
       let ifnone = iter ifnone in
       let ifsome = iter ifsome in
       { exp with desc = MatchOption(arg,loc,ifnone,name,ifsome) }

    | MatchNat(arg, loc, p, ifplus, m, ifminus) ->
       let arg = iter arg in
       let ifplus = iter ifplus in
       let ifminus = iter ifminus in
       { exp with desc = MatchNat(arg, loc, p, ifplus, m, ifminus) }

    | MatchList(arg, loc, head_name, tail_name, ifcons, ifnil) ->
       let arg = iter arg in
       let ifcons = iter ifcons in
       let ifnil = iter ifnil in
       { exp with desc = MatchList(arg,loc,
                                   head_name, tail_name, ifcons,
                                   ifnil) }

    | MatchVariant(arg, loc, cases) ->
       let arg = iter arg in
       let cases = List.map (fun (pat, e) -> pat, iter e) cases in
       { exp with desc = MatchVariant(arg,loc, cases) }

    | Loop(name, loc, body, arg) ->
       let body = iter body in
       let arg = iter arg in
       { exp with desc = Loop(name, loc, body, arg) }

    | Seq(e1, e2) ->
       let e1 = iter e1 in
       let e2 = iter e2 in
       { exp with desc = Seq(e1,e2) }

    | If(cond, ifthen, ifelse) ->
       let cond = iter cond in
       let ifthen = iter ifthen in
       let ifelse = iter ifelse in
       { exp with desc = If(cond, ifthen, ifelse) }

    | Apply(prim, loc, args) ->
       let args = List.map iter args in
       { exp with desc = Apply(prim, loc, args) }

    | LetTransfer (storage, result, loc,
                   contract_exp, tez_exp,
                   storage_exp, arg_exp,
                   body_exp) ->
       let contract_exp = iter contract_exp in
       let tez_exp = iter tez_exp in
       let storage_exp = iter storage_exp in
       let arg_exp = iter arg_exp in
       let body_exp = iter body_exp in
       { exp with desc = LetTransfer(storage, result, loc,
                                     contract_exp, tez_exp,
                                     storage_exp, arg_exp,
                                     body_exp) }

    | Lambda (arg_name, arg_type, loc, body_exp, res_type) ->
      let body_exp = iter body_exp in
      { exp with
        desc = Lambda (arg_name, arg_type, loc, body_exp, res_type) }

    | Closure (arg_name, arg_type, loc, call_env, body_exp, res_type) ->
      let body_exp = iter body_exp in
      let call_env = List.map (fun (name, t) -> name, iter t) call_env in
      { exp with
        desc = Closure (arg_name, arg_type, loc, call_env, body_exp, res_type) }

    | Record (loc, lab_x_exp_list) ->
       let lab_x_exp_list = List.map (fun (label, exp) -> label, iter exp)
                                     lab_x_exp_list in
       { exp with desc = Record(loc, lab_x_exp_list) }

    | And (loc, e1, e2) | Or (loc, e1, e2)
    | Implies (loc, e1, e2) | Equiv (loc, e1, e2) ->
      let e1 = iter e1 in
      let e2 = iter e2 in
      { exp with desc = match exp.desc with
            | And _ -> And (loc, e1, e2)
            | Or _ -> Or (loc, e1, e2)
            | Implies _ -> Implies (loc, e1, e2)
            | Equiv _ -> Equiv (loc, e1, e2)
            | _ -> assert false }

    | Forall (loc, vars, body) ->
      { exp with desc = Forall (loc, vars, iter body) }
    | Exists (loc, vars, body) ->
      { exp with desc = Exists (loc, vars, iter body) }

    | Constructor _ -> assert false (* never found in encoded_exp *)
  in

  iter code

let simplify_specs l to_inline =
  List.map (function
      | Requires f -> Requires (compute f to_inline)
      | Ensures f -> Ensures (compute f to_inline)
      | Fails f -> Fails (compute f to_inline)
    ) l

let simplify_contract contract to_inline =
  { contract with
    code = compute contract.code to_inline;
    spec = simplify_specs contract.spec to_inline;
  }

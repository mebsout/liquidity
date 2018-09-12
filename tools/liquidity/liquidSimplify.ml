(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

let rec compute decompile code to_inline =

  let old_to_inline = to_inline in
  let to_inline = ref (if decompile then StringMap.empty else to_inline) in

  (* Do not inline terms larger than this value when decompiling *)
  let inline_treshold_low = 100 in

  let rec size exp =
    match exp.desc with
    | Const _ | Var _ | SetField _ | Project _ -> 1

    | Failwith { arg }
    | ContractAt { arg }
    | Unpack { arg }
    | Constructor { arg } -> size arg

    | Seq (e1, e2) -> size e1 + size e2

    | Let { bnd_val } -> size bnd_val

    | Loop { arg; body } -> 30 + size arg + size body

    | If { cond = e1 ; ifthen = e2 ; ifelse = e3 }
    | MatchNat { arg = e1; ifplus = e2; ifminus = e3 }
    | MatchList { arg = e1; ifnil = e2; ifcons = e3 }
    | MatchOption { arg = e1; ifnone = e2; ifsome = e3 } ->
      40 + size e1 + size e2 + size e3

    | Map { arg; body } -> 30 + size arg + size body
    | Fold { body; arg; acc }
    | MapFold { body; arg; acc } -> 30 + size body + size arg + size acc
    | Transfer { contract; amount; arg } ->
      1 + size contract + size amount + size arg

    | Apply { args } ->
      List.fold_left (fun acc e -> acc + size e) 1 args

    | Lambda { body }
    | Closure { body } -> 70 + size body

    | Record { fields } ->
      List.fold_left (fun acc (_, e) -> acc + size e) 1 fields

    | MatchVariant { arg; cases } ->
      List.fold_left (fun acc (_, e) -> acc + size e) (size arg + 40) cases

    | CreateContract { args }  ->
      List.fold_left (fun acc e -> acc + size e) 1 args
  in


  let rec iter exp =
    match exp.desc with
    | Const _ -> exp
    | Var { name } ->
       begin
         try
           let v = StringMap.find name !to_inline in
           iter v
         with Not_found -> exp
       end
    | SetField { record; loc; field; set_val } ->
      let record = iter record in
      let set_val = iter set_val in
      begin match record.desc, set_val.desc with
        | SetField s, Project p
          when s.field <> field && p.field = field &&
               eq_syntax_exp s.record p.record ->
          (* (s.f1 <- v1).f2 <- s.f2  ==>  s.f1 <- v1 *)
          record
        | _, Project p
          when p.field = field && eq_syntax_exp p.record record ->
          (* s.f <- s.f  ==>  s *)
          record
        | _, _ -> { exp with desc = SetField { record; loc; field; set_val } }
      end
    | Project { loc; field; record } ->
      let record = iter record in
      { exp with desc = Project { loc; field; record } }
    | Let { bnd_val } when bnd_val.ty = Tfail ->
      iter bnd_val
    | Let { bnd_var; bnd_val; body = { desc = Var { name } }}
      when name = bnd_var -> (* special case for let x = e in x *)
      iter bnd_val
    | Let { bnd_var; bnd_val = ({ ty = Ttuple tys} as v);
            body = { desc = Apply { prim = Prim_tuple; args = tuple } }}
      when
        let len, ok =
          List.fold_left (fun (i, ok) t -> match t.desc with
              | Apply { prim = Prim_tuple_get;
                        args = [
                          { desc = Var { name } };
                          { desc = Const { const = CInt n | CNat n } }] } ->
              let ok = ok && name = bnd_var &&
                       LiquidPrinter.int_of_integer n = i in
              (i + 1, ok)
            | _ -> (i + 1, false)
            ) (0, true) tuple in
        ok && List.length tys = len
      ->
      (* special case for let x = v in (x.(0), x.(1)) *)
      iter v
    | Let { bnd_var; inline; loc; bnd_val; body } ->
      if decompile && bnd_val.name = None &&
         size bnd_val <= inline_treshold_low &&
         (StringMap.mem bnd_var old_to_inline ||
          match bnd_val.desc with
          | Var _ | Apply { prim = Prim_tuple_get } -> true
          | _ -> false)
      then
        to_inline := StringMap.add bnd_var bnd_val !to_inline;
      (* let obody = body in *)
      let body = iter body in
      (* if body <> obody then iter { exp with desc = Let (name, loc, v, body) } else *)
      if StringMap.mem bnd_var !to_inline then
        body
      else
        let bnd_val = iter bnd_val in
        begin
          try
            if StringSet.mem bnd_var (LiquidBoundVariables.bv body) then
              raise Exit;
            if not bnd_val.fail then
              body
            else
            if bnd_val.ty <> Tunit then raise Exit
            else
              { exp with desc = Seq (bnd_val, body); name = None }
          with Exit ->
            { exp with desc = Let { bnd_var; inline; loc; bnd_val; body } }
        end

    | MatchOption { arg; loc; ifnone; some_name; ifsome } ->
       let arg = iter arg in
       let ifnone = iter ifnone in
       let ifsome = iter ifsome in
       { exp with desc = MatchOption { arg; loc; ifnone; some_name; ifsome } }

    | MatchNat { arg; loc; plus_name; ifplus; minus_name; ifminus } ->
       let arg = iter arg in
       let ifplus = iter ifplus in
       let ifminus = iter ifminus in
       { exp with
         desc = MatchNat { arg; loc; plus_name; ifplus; minus_name; ifminus } }

    | MatchList { arg; loc; head_name; tail_name; ifcons; ifnil } ->
       let arg = iter arg in
       let ifcons = iter ifcons in
       let ifnil = iter ifnil in
       { exp with
         desc = MatchList { arg; loc; head_name; tail_name; ifcons; ifnil } }

    | MatchVariant { arg; loc; cases } ->
       let arg = iter arg in
       let cases = List.map (fun (pat, e) -> pat, iter e) cases in
       { exp with desc = MatchVariant { arg; loc; cases } }

    | Loop { arg_name; loc; body; arg } ->
       let body = iter body in
       let arg = iter arg in
       { exp with desc = Loop { arg_name; loc; body; arg } }

    | Fold { prim; arg_name; loc; body; arg; acc } ->
       let body = iter body in
       let arg = iter arg in
       let acc = iter acc in
       { exp with desc = Fold { prim; arg_name; loc; body; arg; acc } }

    | Map { prim; arg_name; loc; body; arg } ->
       let body = iter body in
       let arg = iter arg in
       { exp with desc = Map { prim; arg_name; loc; body; arg } }

    | MapFold { prim; arg_name; loc; body; arg; acc } ->
       let body = iter body in
       let arg = iter arg in
       let acc = iter acc in
       { exp with desc = MapFold { prim; arg_name; loc; body; arg; acc } }

    | Seq(e1, e2) ->
       let e1 = iter e1 in
       let e2 = iter e2 in
       if e1.ty = Tfail (* e1 always fails *)
       then e1
       else if not e1.fail && not e1.transfer (* no side-effects *)
       then e2
       else { exp with desc = Seq(e1,e2) }

    | If { cond; ifthen; ifelse } ->
       let cond = iter cond in
       let ifthen = iter ifthen in
       let ifelse = iter ifelse in
       { exp with desc = If { cond; ifthen; ifelse } }

    | Apply { prim = Prim_exec; loc; args =  [x; f] } ->
       (* inline body of lambda *)
       let x = iter x in
       let f = iter f in
       begin match f.desc with
         | Lambda { arg_name; body } ->
           iter { exp with
                  desc = Let { bnd_var = arg_name; inline = false;
                               loc; bnd_val = x;  body }
                }
         | _ ->
           { exp with desc = Apply { prim = Prim_exec; loc; args = [x; f] } }
       end

    | Apply { prim; loc; args } ->
       let args = List.map iter args in
       { exp with desc = Apply { prim; loc; args } }

    | Transfer { loc; contract; amount; entry; arg } ->
       let contract = iter contract in
       let amount = iter amount in
       let arg = iter arg in
       { exp with desc = Transfer { loc; contract; amount; entry; arg } }

    | Lambda { arg_name; arg_ty; loc; body; ret_ty } ->
      let body = iter body in
      { exp with
        desc = Lambda { arg_name; arg_ty; loc; body; ret_ty } }

    | Closure { arg_name; arg_ty; call_env; loc; body; ret_ty } ->
      let body = iter body in
      let call_env = List.map (fun (name, t) -> name, iter t) call_env in
      { exp with
        desc = Closure { arg_name; arg_ty; call_env; loc; body; ret_ty } }

    | Record { loc; fields } ->
       let fields = List.map (fun (label, exp) -> label, iter exp) fields in
       { exp with desc = Record { loc; fields } }

    | Failwith { arg; loc } ->
      { exp with desc = Failwith { arg = iter arg; loc } }

    | CreateContract { loc; args; contract } ->
      let args = List.map iter args in
      (* contract is already simplified *)
      { exp with desc = CreateContract { loc; args; contract } }

    | ContractAt { loc; arg; c_sig } ->
      let arg = iter arg in
      { exp with desc = ContractAt { loc; arg; c_sig } }

    | Unpack { loc; arg; ty } ->
      let arg = iter arg in
      { exp with desc = Unpack { loc; arg; ty } }

    | Constructor { loc; constr; arg } ->
      let arg = iter arg in
      { exp with desc = Constructor { loc; constr; arg } }
  in

  let rec fixpoint code =
    let c = iter code in
    if c <> code then fixpoint c else c
  in

  fixpoint code

  (* iter code *)

and simplify_contract ?(decompile_annoted=false) contract to_inline =
  match contract.entries with
  | [{ entry_sig = { entry_name = "main" };
       code } as entry ] ->
    { contract with
      entries = [{ entry with code = compute decompile_annoted code to_inline }]
    }
  | _ -> assert false

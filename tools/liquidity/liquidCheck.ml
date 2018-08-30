(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* TODO: we should check that the keys of maps and sets are comparable,
  in particular to avoid using `key` in maps instead of `key_hash`. *)

(* TODO: we don't handle correctly all the occurrences of Tfail, i.e.
   when an error occurs in a sub-part of a type and another type is expected,
   we should probably unify.
 *)

(*
  Typecheck an AST.
  The following actions are also performed:
  * variables are replaced by uniq identifiers: STRING/INTEGER
  * Var(var,_loc,labels) -> Var(var,_loc,[]) with accesses thought "get"
  * SetVar(var,_loc,labels) -> Var(var,_loc,[]) with accesses thought "get"
                               and modification thought "set"
  * Lambda(_,_,_,Tunit) -> Lambda(_,_,_, body.ty)
  * Record(_,_) -> Apply("tuple", _)
  * Constructor(_) -> Apply("Left"|"Right", [arg; unused ty])

 *)

open LiquidTypes

let noloc env = LiquidLoc.loc_in_file env.env.filename

let error loc msg =
  LiquidLoc.raise_error ~loc ("Type error:  " ^^ msg ^^ "%!")

let comparable_ty ty1 ty2 =
  comparable_type ty1 && ty1 = ty2

let error_not_comparable loc prim ty1 ty2 =
  error loc "arguments of %s not comparable: %s\nwith\n%s\n"
    (LiquidTypes.string_of_primitive prim)
    (LiquidPrinter.Liquid.string_of_type ty1)
    (LiquidPrinter.Liquid.string_of_type ty2)

let new_binding env name ?(fail=false) ty =
  let count = ref 0 in
  let env = { env with
              vars = StringMap.add name (name, ty, fail) env.vars;
              vars_counts = StringMap.add name count env.vars_counts;
            } in
  (env, count)

let check_used env name loc count =
  if env.warnings && !count = 0 && name.[0] <> '_' then begin
      LiquidLoc.warn loc (Unused name)
  end

let check_used_in_env env name loc =
  try
    let count = StringMap.find name env.vars_counts in
    check_used env name loc count;
  with Not_found ->
  match env.clos_env with
  | None -> check_used env name loc (ref 0)
  | Some ce ->
    try
      let _, (count, _) = StringMap.find name ce.env_bindings in
      check_used env name loc count;
    with Not_found ->
      check_used env name loc (ref 0)

(* Find variable name in either the global environment *)
let find_var ?(count_used=true) env loc name =
  try
    let (name, ty, fail) = StringMap.find name env.vars in
    let count = StringMap.find name env.vars_counts in
    if count_used then incr count;
    { (mk (Var (name, loc)) ty) with fail }
  with Not_found ->
    error loc "unbound variable %S" name

let type_error loc msg actual expected =
  error loc "%s.\nExpected type:\n  %s\nActual type:\n  %s"
    msg
    (LiquidPrinter.Liquid.string_of_type expected)
    (LiquidPrinter.Liquid.string_of_type actual)


let error_prim loc prim args expected_args =
  let prim = LiquidTypes.string_of_primitive prim in
  let nargs = List.length args in
  let nexpected = List.length expected_args in
  if nargs <> nexpected then
    error loc "Prim %S: %d args provided, %d args expected"
          prim nargs nexpected
  else
    let args = List.map (fun { ty } -> ty) args in
    List.iteri (fun i (arg, expected) ->
        if arg <> expected then
          error loc
                "Primitive %s, argument %d:\nExpected type:%s\nProvided type:%s"
                prim (i+1)
                (LiquidPrinter.Liquid.string_of_type expected)
                (LiquidPrinter.Liquid.string_of_type arg)

      ) (List.combine args expected_args);
    Printf.eprintf "Fatal error on typechecking primitive %S\n%!" prim;
    assert false


  (* approximate location *)
let rec loc_exp e = match e.desc with
  | Const (loc, _, _)
  | Var (_, loc)
  | SetField (_, loc, _, _)
  | Project (loc, _, _)
  | Apply (_, loc, _)
  | Transfer (loc, _, _, _, _)
  | MatchOption (_, loc, _, _, _)
  | MatchNat (_, loc, _, _, _, _)
  | MatchList (_, loc, _, _, _, _)
  | Loop (_, loc, _, _)
  | Fold (_, _, loc, _, _, _)
  | Map (_, _, loc, _, _)
  | MapFold (_, _, loc, _, _, _)
  | Lambda (_, _, loc, _, _)
  | Closure (_, _, loc, _, _, _)
  | Record (loc, _)
  | Constructor (loc, _, _)
  | MatchVariant (_, loc, _)
  | Failwith (_, loc)
  | CreateContract (loc, _, _)
  | ContractAt (loc, _, _)
  | Unpack (loc, _, _) -> loc

  | Let (_, _, _, _, e) -> loc_exp e

  | If (e1, _, e2)
  | Seq (e1, e2) ->
    match loc_exp e1, loc_exp e2 with
    | ({ loc_pos = Some ( loc_begin , _ ) } as loc),
      { loc_pos = Some ( _, loc_end ) } ->
      { loc with loc_pos = Some (loc_begin, loc_end) }
    | loc, _ -> loc

(* Merge nested matches to recover encoding for pattern matching over
   sum type *)
let rec merge_matches acc loc cases constrs =
  match cases, constrs with
  | [ CConstr ("Left", l), case_l; CConstr ("Right", r), case_r ],
    [ c1, ty1; c2, ty2 ] ->
    List.rev @@ (CConstr (c2, r), case_r) ::
                (CConstr (c1, l), case_l) :: acc

  | [ CConstr ("Left", l), case_l;
      CConstr ("Right", [x]), { desc = Let (v, _, _, case_r,
                                            { desc = Var (v', _) }) } ],
    _ :: _
    when v = v'
    ->
    merge_matches acc loc [ CConstr ("Left", l), case_l;
                            CConstr ("Right", [x]), case_r ] constrs

  | [ CConstr ("Left", l), case_l; CConstr ("Right", [x]), case_r ],
    (c1, ty1) :: constrs ->
    begin match case_r.desc with
      | MatchVariant ( { desc = Var (x', _) }, loc, cases) when x = x' ->
        (* match arg with
           | Left l -> case_l
           | Right x -> match x with
                        | Left -> ...*)

        merge_matches ((CConstr (c1, l), case_l) :: acc) loc cases constrs
      | _ -> raise Exit
    end
  | _ -> raise Exit

(* (\* Flatten nested records to recover encodings *\)
 * let flatten_record env exp = match exp.desc with
 *   | Record (loc, [lx, x; ly, y]) ->
 *     let ty_rx = StringMap.find lx env.env.labels in
 *     let ty_ry = StringMap.find ly env.env.labels in
 *     if ty_rx <> ty_ry then exp
 *     else
 *       let nb_fields = match ty_rx with
 *         | Trecord (_, labels) -> List.length labels
 *         | _ -> assert false in
 *       let rec aux nb exp =
 *         if nb = 0 then exp
 *         else match exp.desc with
 *           | Record (loc, [lx, x; ly, y]) ->
 *             (lx, x) :: aux (nb - 1)
 *           | _ -> exp
 *       in
 *       aux nb_fields exp
 *   | _ -> exp *)

  (* this function returns a triple with
   * the expression annotated with its type
   * whether the expression can fail
   * whether the expression performs a TRANSFER_TOKENS
   *)
let rec typecheck env ( exp : syntax_exp ) : typed_exp =
  match exp.desc with

  | Const (loc, ty, cst) ->
    mk ?name:exp.name (Const (loc, ty, cst)) (ty:datatype)

  | Let (name, inline, loc, exp, body) ->
     let exp = typecheck env exp in
     if exp.ty = Tfail then
       match exp.desc with
       | Failwith _ -> exp
       | _ ->
         mk (Failwith (mk (Const (loc, Tunit, CUnit)) Tunit, loc)) Tfail
     else
       let (env, count) = new_binding env name ~fail:exp.fail exp.ty in
       let body = typecheck env body in
       let desc = Let (name, inline, loc, exp, body ) in
       check_used env name loc count;
       mk ?name:exp.name desc body.ty

  | Var (name, loc) -> find_var env loc name

  | Project (loc, label, arg) ->
    let arg = typecheck env arg in
    let ty = match arg.ty with
      | Trecord (record_name, ltys) ->
        begin
          try List.assoc label ltys
          with Not_found ->
            error loc "label %s does not belong to type %s"
              label record_name;
        end
      | _ -> error loc "not a record type: %s"
               (LiquidPrinter.Liquid.string_of_type arg.ty)
    in
    mk ?name:exp.name (Project (loc, label, arg)) ty

  | SetField (arg, loc, label, e) ->
    let arg = typecheck env arg in
    let exp_ty =
      let ty_name, _, ty =
        try StringMap.find label env.env.labels
        with Not_found -> error loc "unbound label %S" label
      in
      let record_ty = StringMap.find ty_name env.env.types in
      if arg.ty <> record_ty then
        error loc "label %s does not belong to type %s" label
          (LiquidPrinter.Liquid.string_of_type arg.ty);
      ty
    in
    let e = typecheck_expected "field update" env exp_ty e in
    mk ?name:exp.name (SetField (arg, loc, label, e)) arg.ty

  | Seq (exp1, exp2) ->
    let exp1 = typecheck_expected "sequence" env Tunit exp1 in
    let exp2 = typecheck env exp2 in
    let desc = Seq (exp1, exp2) in
    (* TODO: if not fail1 then remove exp1 *)
    mk ?name:exp.name desc exp2.ty

  | If (cond, ifthen, ifelse) ->
     let cond =
       typecheck_expected "if condition" env Tbool cond in
     let ifthen = typecheck env ifthen in
     let ifelse, ty =
       if ifthen.ty = Tfail then
         let ifelse = typecheck env ifelse in
         ifelse, ifelse.ty
       else
         let ifelse =
           typecheck_expected "else branch" env ifthen.ty ifelse in
         ifelse, ifthen.ty
     in
     let desc = If(cond, ifthen, ifelse) in
     mk ?name:exp.name desc ty

  | Transfer (loc, contract_exp, tez_exp, entry, arg_exp) ->
     let tez_exp = typecheck_expected "call amount" env Ttez tez_exp in
     let contract_exp = typecheck env contract_exp in
     let entry' = match entry with None -> "main" | Some e -> e in
     begin
       match contract_exp.ty with
       | Tcontract contract_sig ->
         begin try
             let { parameter = arg_ty } =
               List.find (fun { entry_name } -> entry_name = entry')
                 contract_sig.entries_sig in
             let arg_exp = typecheck_expected "call argument" env arg_ty arg_exp in
             if tez_exp.transfer || contract_exp.transfer || arg_exp.transfer then
               error loc "transfer within transfer arguments";
             let desc = Transfer(loc, contract_exp, tez_exp, entry, arg_exp) in
             mk ?name:exp.name desc Toperation
           with Not_found ->
             error loc "contract has no entry point %s" entry';
         end
       | ty ->
         error (loc_exp contract_exp)
           "Bad contract type.\nExpected type:\n  'a contract\n\
            Actual type:\n  %s"
           (LiquidPrinter.Liquid.string_of_type ty)
     end

  | Apply (Prim_unknown, loc,
           ({ desc = Var (name, varloc)} as f) :: ((_ :: _) as r))
       when StringMap.mem name env.vars ->
     let exp = List.fold_left (fun f x ->
                   { exp with desc = Apply (Prim_exec, loc, [x; f]) }
                 ) f r
     in
     typecheck env exp

  | Apply (Prim_unknown, loc,
           ({ desc = Var ("Contract.call", varloc)} :: args)) ->
    let nb_args = List.length args in
    if nb_args <> 3 then
      error loc
        "Contract.call expects 3 arguments, it was given %d arguments."
        nb_args
    else
      error loc "Bad syntax for Contract.call."

  | Apply (Prim_unknown, loc,
           ({ desc = Var (name, varloc)} ::args)) ->
     let prim =
       try
         LiquidTypes.primitive_of_string name
       with Not_found ->
         error loc "Unknown identifier %S" name
     in
     typecheck env { exp with
                     desc = Apply(prim, loc, args) }

  | Apply (Prim_unknown, loc, [f ; x ]) ->
     typecheck env { exp with
                     desc = Apply(Prim_exec, loc, [x; f]) }

  | Apply (prim, loc, args) -> typecheck_apply ?name:exp.name env prim loc args

  | Failwith (err, loc) ->
    let err = typecheck env err in
    mk (Failwith (err, loc)) Tfail (* no name *)

  | MatchOption (arg, loc, ifnone, name, ifsome) ->
     let arg = typecheck env arg in
     let arg_ty = match arg.ty with
       | Tfail -> error loc "cannot match failure"
       | Toption ty -> ty
       | _ -> error loc "not an option type"
     in
     let ifnone = typecheck env ifnone in
     let (env, count) = new_binding env name arg_ty in
     let ifsome = typecheck env ifsome in
     check_used env name loc count;
     let desc = MatchOption (arg, loc, ifnone, name, ifsome ) in
     let ty =
       match ifnone.ty, ifsome.ty with
       | ty, Tfail | Tfail, ty -> ty
       | ty1, ty2 ->
         if ty1 <> ty2 then
           type_error loc "branches of match have different types" ty2 ty1;
         ty1
     in
     mk ?name:exp.name desc ty

  | MatchNat (arg, loc, plus_name, ifplus, minus_name, ifminus) ->
     let arg = typecheck_expected "match%nat" env Tint arg in
     let (env2, count_p) = new_binding env plus_name Tnat in
     let ifplus = typecheck env2 ifplus in
     let (env3, count_m) = new_binding env minus_name Tnat in
     let ifminus = typecheck env3 ifminus in
     check_used env plus_name loc count_p;
     check_used env minus_name loc count_m;
     let desc = MatchNat (arg, loc, plus_name, ifplus, minus_name, ifminus) in
     let ty =
       match ifplus.ty, ifminus.ty with
       | ty, Tfail | Tfail, ty -> ty
       | ty1, ty2 ->
         if ty1 <> ty2 then
           type_error loc "branches of match%nat must have the same type"
             ty2 ty1;
         ty1
     in
     mk ?name:exp.name desc ty

  | Loop (name, loc, body, arg) ->
     let arg = typecheck env arg in
     if arg.ty = Tfail then error loc "loop arg is a failure";
     let (env, count) = new_binding env name arg.ty in
     let body =
       typecheck_expected "loop body" env (Ttuple [Tbool; arg.ty]) body in
     check_used env name loc count;
     mk ?name:exp.name (Loop (name, loc, body, arg)) arg.ty

  | Fold (prim, name, loc, body, arg, acc) ->
    let arg = typecheck env arg in
    let acc = typecheck env acc in
    let prim, name_ty = match prim, arg.ty, acc.ty with
      | (Prim_coll_iter|Prim_map_iter), Tmap (k_ty, v_ty), Tunit ->
        Prim_map_iter, Ttuple [k_ty; v_ty]
      | (Prim_coll_iter|Prim_set_iter), Tset elt_ty, Tunit ->
        Prim_set_iter, elt_ty
      | (Prim_coll_iter|Prim_list_iter), Tlist elt_ty, Tunit ->
        Prim_list_iter, elt_ty

      | (Prim_map_fold|Prim_coll_fold), Tmap (k_ty, v_ty), acc_ty ->
        Prim_map_fold, Ttuple[Ttuple [k_ty; v_ty]; acc_ty]
      | (Prim_set_fold|Prim_coll_fold), Tset elt_ty, acc_ty ->
        Prim_set_fold, Ttuple[elt_ty; acc_ty]
      | (Prim_list_fold|Prim_coll_fold), Tlist elt_ty, acc_ty ->
        Prim_list_fold, Ttuple[elt_ty; acc_ty]

      | _ ->
        error (loc_exp arg) "%s expects a collection, got %s"
          (LiquidTypes.string_of_fold_primitive prim)
          (LiquidPrinter.Liquid.string_of_type arg.ty)
    in
    let (env, count) = new_binding env name name_ty in
    let body = typecheck_expected
        (LiquidTypes.string_of_fold_primitive prim ^" body") env acc.ty body in
    check_used env name loc count;
    mk ?name:exp.name (Fold (prim, name, loc, body, arg, acc)) acc.ty

  | Map (prim, name, loc, body, arg) ->
    let arg = typecheck env arg in
    let prim, name_ty = match prim, arg.ty with
      | (Prim_map_map|Prim_coll_map), Tmap (k_ty, v_ty) ->
        Prim_map_map, Ttuple [k_ty; v_ty]
      | (Prim_set_map|Prim_coll_map), Tset elt_ty ->
        Prim_set_map, elt_ty
      | (Prim_list_map|Prim_coll_map), Tlist elt_ty ->
        Prim_list_map, elt_ty
      | _ ->
        error (loc_exp arg) "%s expects a collection, got %s"
          (LiquidTypes.string_of_map_primitive prim)
          (LiquidPrinter.Liquid.string_of_type arg.ty)
    in
    let (env, count) = new_binding env name name_ty in
    let body = typecheck env body in
    let ret_ty = match arg.ty with
      | Tmap (k_ty, _) -> Tmap (k_ty, body.ty)
      | Tset _ -> Tset body.ty
      | Tlist _ -> Tlist body.ty
      | _ -> assert false
    in
    check_used env name loc count;
    mk ?name:exp.name (Map (prim, name, loc, body, arg)) ret_ty

  | MapFold (prim, name, loc, body, arg, acc) ->
    let arg = typecheck env arg in
    let acc = typecheck env acc in
    let prim, name_ty = match prim, arg.ty, acc.ty with
      | (Prim_map_map_fold|Prim_coll_map_fold), Tmap (k_ty, v_ty), acc_ty ->
        Prim_map_map_fold, Ttuple[Ttuple [k_ty; v_ty]; acc_ty]
      | (Prim_set_map_fold|Prim_coll_map_fold), Tset elt_ty, acc_ty ->
        Prim_set_map_fold, Ttuple[elt_ty; acc_ty]
      | (Prim_list_map_fold|Prim_coll_map_fold), Tlist elt_ty, acc_ty ->
        Prim_list_map_fold, Ttuple[elt_ty; acc_ty]
      | _ ->
        error (loc_exp arg) "%s expects a collection, got %s"
          (LiquidTypes.string_of_map_fold_primitive prim)
          (LiquidPrinter.Liquid.string_of_type arg.ty)
    in
    let (env, count) = new_binding env name name_ty in
    let body = typecheck env body in
    let body_r = match body.ty with
      | Ttuple [r; baccty] when baccty = acc.ty -> r
      | _ ->
        error (loc_exp body)
          "body of %s must be of type 'a * %s, but has type %s"
          (LiquidTypes.string_of_map_fold_primitive prim)
          (LiquidPrinter.Liquid.string_of_type acc.ty)
          (LiquidPrinter.Liquid.string_of_type body.ty)
    in
    let ret_ty = match arg.ty with
      | Tmap (k_ty, _) -> Tmap (k_ty, body_r)
      | Tset _ -> Tset body_r
      | Tlist _ -> Tlist body_r
      | _ -> assert false
    in
    check_used env name loc count;
    mk ?name:exp.name (MapFold (prim, name, loc, body, arg, acc))
      (Ttuple [ret_ty; acc.ty])

  | MatchList (arg, loc, head_name, tail_name, ifcons, ifnil) ->
     let arg  = typecheck env arg in
     let arg_ty = match arg.ty with
       | Tfail -> error loc "cannot match failure"
       | Tlist ty -> ty
       | _ -> error loc "not a list type"
     in
     let ifnil = typecheck env ifnil in
     let (env, count_head) = new_binding env head_name arg_ty in
     let (env, count_tail) = new_binding env tail_name (Tlist arg_ty) in
     let ifcons = typecheck env ifcons in
     check_used env head_name loc count_head;
     check_used env tail_name loc count_tail;
     let desc = MatchList (arg, loc, head_name, tail_name, ifcons, ifnil) in
     let ty =
       match ifnil.ty, ifcons.ty with
       | ty, Tfail | Tfail, ty -> ty
       | ty1, ty2 ->
         if ty1 <> ty2 then
           type_error loc "branches of match must have the same type"
             ty2 ty1;
          ty1
     in
     mk ?name:exp.name desc ty

  | Lambda (arg_name, arg_type, loc, body, res_type) ->
    let lambda_arg_type = arg_type in
    let lambda_arg_name = arg_name in
    let lambda_body = body in
    assert (res_type = Tunit);
    (* allow closures at typechecking, do not reset env *)
    let (env, arg_count) = new_binding env lambda_arg_name lambda_arg_type in
    let body = typecheck env lambda_body in
    check_used env lambda_arg_name loc arg_count;
    let desc = Lambda (arg_name, lambda_arg_type, loc, body, body.ty) in
    let ty = Tlambda (lambda_arg_type, body.ty) in
    mk ?name:exp.name desc ty

  | Closure _ -> assert false

  | Record (_loc, []) -> assert false
  | Record (loc, (( (label, _) :: _ ) as lab_x_exp_list)) ->
     let ty_name, _, _ =
       try StringMap.find label env.env.labels
       with Not_found -> error loc "unbound label %S" label
     in
     let record_ty = StringMap.find ty_name env.env.types in
     let remaining_labels = match record_ty with
       | Trecord (_, rtys) -> List.map fst rtys |> StringSet.of_list |> ref
       | _ -> assert false in
     let lab_exp = List.map (fun (label, exp) ->
         let ty_name', _, ty = try
             StringMap.find label env.env.labels
           with Not_found -> error loc "unbound label %S" label
         in
         if ty_name <> ty_name' then error loc "inconsistent list of labels";
         let exp = typecheck_expected ("label "^ label) env ty exp in
         remaining_labels := StringSet.remove label !remaining_labels;
         (label, exp)
       ) lab_x_exp_list in
     if not (StringSet.is_empty !remaining_labels) then
       error loc "label %s is not defined" (StringSet.choose !remaining_labels);
     mk ?name:exp.name (Record (loc, lab_exp)) record_ty

  | Constructor(loc, Constr constr, arg) ->
     let ty_name, arg_ty = StringMap.find constr env.env.constrs in
     let arg = typecheck_expected "construtor argument" env arg_ty arg in
     let constr_ty = StringMap.find ty_name env.env.types in
     mk ?name:exp.name (Constructor(loc, Constr constr, arg)) constr_ty

  | Constructor(loc, Left right_ty, arg) ->
     let arg = typecheck env arg in
     let ty = Tor (arg.ty, right_ty) in
     mk ?name:exp.name (Constructor(loc, Left right_ty, arg)) ty

  | Constructor(loc, Right left_ty, arg) ->
     let arg = typecheck env arg in
     let ty = Tor (left_ty, arg.ty) in
     mk ?name:exp.name (Constructor(loc, Right left_ty, arg)) ty

  | MatchVariant (arg, loc, cases) ->
    let untyped_arg = arg in
    let arg = typecheck env arg in
    let decoded = match arg.ty, env.decompiling with
      | Tsum (_, constrs), true ->
        (* allow loose typing when decompiling *)
        begin try
            let cases = merge_matches [] loc cases constrs in
            Some (typecheck env
                    { exp with desc = MatchVariant (untyped_arg, loc, cases) })
          with Exit -> None
        end
      | _ -> None in
    begin match decoded with
      | Some exp -> exp
      | None ->
        let constrs, is_left_right =
          try
            match arg.ty with
            | Tfail ->
              error loc "cannot match failure"
            | Tsum (_, constrs) ->
              (List.map fst constrs, None)
            | Tor (left_ty, right_ty) ->
              (* Left, Right pattern matching *)
              (["Left"; "Right"], Some (left_ty, right_ty))
            | _ -> raise Not_found
          with Not_found ->
            error loc "not a variant type: %s"
              (LiquidPrinter.Liquid.string_of_type arg.ty)
        in
        let expected_type = ref None in
        let cases_extra_constrs =
          List.fold_left (fun acc -> function
              | CAny, _ -> acc
              | CConstr (c, _), _ -> StringSet.add c acc
            ) StringSet.empty cases
          |> ref
        in
        let cases = List.map (fun constr ->
            let cve = find_case loc env constr cases in
            cases_extra_constrs := StringSet.remove constr !cases_extra_constrs;
            cve
          ) constrs in

        if not (StringSet.is_empty !cases_extra_constrs) then
          error loc "constructors %s do not belong to type %s"
            (String.concat ", " (StringSet.elements !cases_extra_constrs))
            (LiquidPrinter.Liquid.string_of_type arg.ty);

        let cases = List.map (fun (constr, vars, e) ->
            let var_ty = match is_left_right, constr with
              | Some (left_ty, _), "Left" -> left_ty
              | Some (_, right_ty), "Right" -> right_ty
              | Some _, _ -> error loc "unknown constructor %S" constr
              | None, _ ->
                let ty_name', var_ty =
                  try StringMap.find constr env.env.constrs
                  with Not_found -> error loc "unknown constructor %S" constr
                in
                (* if ty_name <> ty_name' then error loc "inconsistent constructors"; *)
                var_ty
            in
            let env, count_opt =
              match vars with
              | [] -> env, None
              | [ var ] ->
                let (env, count) = new_binding env var var_ty in
                env, Some count
              | _ ->
                error loc "cannot deconstruct constructor args"
            in
            let e =
              match !expected_type with
              | Some expected_type ->
                typecheck_expected "pattern matching branch" env expected_type e
              | None ->
                let e = typecheck env e in
                begin match e.ty with
                  | Tfail -> ()
                  | _ -> expected_type := Some e.ty
                end;
                e
            in
            begin match vars, count_opt with
              | [name], Some count -> check_used env name loc count
              | _ -> ()
            end;
            (CConstr (constr, vars), e)
          ) cases
        in

        let desc = MatchVariant (arg, loc, cases) in
        let ty = match !expected_type with
          | None -> Tfail
          | Some ty -> ty
        in
        mk ?name:exp.name desc ty
    end

  | Unpack (loc, e, ty) ->
    let e = typecheck_expected "Bytes.unpack argument" env Tbytes e in
    let desc = Unpack (loc, e, ty) in
    mk ?name:exp.name desc (Toption ty)

  | ContractAt (loc, addr, ty) ->
    let addr = typecheck_expected "Contract.at argument" env Taddress addr in
    let desc = ContractAt (loc, addr, ty) in
    mk ?name:exp.name desc (Toption ty)

  | CreateContract (loc, args, contract) ->
    let contract = typecheck_contract ~warnings:env.warnings
        ~decompiling:env.decompiling env.env contract in
    match args with
    | [manager; delegate; spendable; delegatable; init_balance; init_storage] ->
      let manager = typecheck_expected "manager" env Tkey_hash manager in
      let delegate =
        typecheck_expected "delegate" env (Toption Tkey_hash) delegate in
      let spendable = typecheck_expected "spendable" env Tbool spendable in
      let delegatable =
        typecheck_expected "delegatable" env Tbool delegatable in
      let init_balance =
        typecheck_expected "initial balance" env Ttez init_balance in
      let init_storage = typecheck_expected "initial storage"
          env contract.storage init_storage in
      let desc = CreateContract (loc, [manager; delegate; spendable;
                                       delegatable; init_balance; init_storage],
                                 contract) in
      mk ?name:exp.name desc (Ttuple [Toperation; Taddress])
    | _ ->
      error loc "Contract.create expects 7 arguments, was given %d"
        (List.length args)

and find_case loc env constr cases =
  match List.find_all (function
      | CAny, _ -> true
      | CConstr (cname, _), _ -> cname = constr
    ) cases
  with
  | [] ->
    error loc "non-exhaustive pattern. Constructor %s is not matched." constr
  | m :: unused ->
    List.iter (fun (_, e) ->
        LiquidLoc.warn (loc_exp e) (UnusedMatched constr)
      ) unused;
    match m with
    | CAny, e -> constr, [], e
    | CConstr (_, vars), e -> constr, vars, e

and typecheck_prim1 env prim loc args =
  match prim, args with
  | Prim_tuple_get, [ { ty = tuple_ty };
                      { desc = Const (loc, _, (CInt n | CNat n)) }] ->
    let tuple = match tuple_ty with
      | Ttuple tuple -> tuple
      | Trecord (_, rtys) -> List.map snd rtys
      | _ -> error loc "get takes a tuple as first argument, got:\n%s"
               (LiquidPrinter.Liquid.string_of_type tuple_ty)
    in
    let n = LiquidPrinter.int_of_integer n in
    let size = List.length tuple in
    if size <= n then error loc "get outside tuple";
    let ty = List.nth tuple n in
    prim, ty

  | Prim_tuple_set, [ { ty = tuple_ty };
                      { desc = Const (loc, _, (CInt n | CNat n)) };
                      { ty } ] ->
    let tuple = match tuple_ty with
      | Ttuple tuple -> tuple
      | Trecord (_, rtys) -> List.map snd rtys
      | _ -> error loc "set takes a tuple as first argument, got:\n%s"
               (LiquidPrinter.Liquid.string_of_type tuple_ty)
    in
    let n = LiquidPrinter.int_of_integer n in
    let expected_ty = List.nth tuple n in
    let size = List.length tuple in
    if size <= n then error loc "set outside tuple";
    let ty = if not (ty = expected_ty || ty = Tfail) then
        error loc "prim set bad type"
      else tuple_ty
    in
    prim, ty

  | _ ->
     let prim =
       (* Unqualified versions of primitives. They should not be used,
         but they can be generated by decompiling Michelson. *)
       match prim, args with
       | Prim_coll_update, [ _; _; { ty = Tset _ }] -> Prim_set_update
       | Prim_coll_update, [ _; _; { ty = (Tmap _ | Tbigmap _) }] ->
         Prim_map_update
       | Prim_coll_mem, [ _; { ty = Tset _ } ] -> Prim_set_mem
       | Prim_coll_mem, [ _; { ty = (Tmap _ | Tbigmap _) } ] -> Prim_map_mem
       | Prim_coll_find, [ _; { ty = (Tmap _ | Tbigmap _) } ] -> Prim_map_find
       | Prim_coll_size, [{ ty = Tlist _ } ] -> Prim_list_size
       | Prim_coll_size, [{ ty = Tset _ } ] -> Prim_set_size
       | Prim_coll_size, [{ ty = Tmap _ } ] -> Prim_map_size
       | Prim_coll_size, [{ ty = Tstring } ] -> Prim_string_size
       | Prim_coll_size, [{ ty = Tbytes } ] -> Prim_bytes_size
       | Prim_slice, [ _; _; { ty = Tstring } ] -> Prim_string_sub
       | Prim_slice, [ _; _; { ty = Tbytes } ] -> Prim_bytes_sub
       | Prim_concat, [{ ty = Tlist Tstring } ] -> Prim_string_concat
       | Prim_concat, [{ ty = Tlist Tbytes } ] -> Prim_bytes_concat
       | _ -> prim
     in
     prim, typecheck_prim2 env prim loc args

and typecheck_prim2 env prim loc args =
  match prim, List.map (fun a -> a.ty) args with
  | ( Prim_neq | Prim_lt | Prim_gt | Prim_eq | Prim_le | Prim_ge ),
    [ ty1; ty2 ] ->
     if comparable_ty ty1 ty2 then Tbool
     else error_not_comparable loc prim ty1 ty2
  | Prim_compare,
    [ ty1; ty2 ] ->
     if comparable_ty ty1 ty2 then Tint
     else error_not_comparable loc prim ty1 ty2

  | Prim_neg, [( Tint | Tnat )] -> Tint

  | (Prim_add | Prim_sub) , [ Ttez; Ttez ] -> Ttez
  | Prim_mul, ([ Tnat; Ttez ] | [ Ttez; Tnat ]) -> Ttez

  | (Prim_add|Prim_mul), [ Tnat; Tnat ] -> Tnat
  | (Prim_add|Prim_sub|Prim_mul), [ (Tint|Tnat);
                                    (Tint|Tnat) ] -> Tint

  | Prim_add, [ Ttimestamp; Tint|Tnat ] -> Ttimestamp
  | Prim_add, [ Tint|Tnat; Ttimestamp ] -> Ttimestamp
  | Prim_sub, [ Ttimestamp; Tint|Tnat ] -> Ttimestamp
  | Prim_sub, [ Ttimestamp; Ttimestamp ] -> Tint

  (* TODO: improve types of ediv in Michelson ! *)
  | Prim_ediv, [ Tnat; Tnat ] ->
     Toption (Ttuple [Tnat; Tnat])
  | Prim_ediv, [ Tint|Tnat; Tint|Tnat ] ->
     Toption (Ttuple [Tint; Tnat])
  | Prim_ediv, [ Ttez; Tnat ] ->
     Toption (Ttuple [Ttez; Ttez])
  | Prim_ediv, [ Ttez; Ttez ] ->
     Toption (Ttuple [Tnat; Ttez])

  | Prim_xor, [ Tbool; Tbool ] -> Tbool
  | Prim_or, [ Tbool; Tbool ] -> Tbool
  | Prim_and, [ Tbool; Tbool ] -> Tbool
  | Prim_not, [ Tbool ] -> Tbool

  | Prim_xor, [ Tnat; Tnat ] -> Tnat
  | Prim_or, [ Tnat; Tnat ] -> Tnat
  | Prim_and, [ Tint|Tnat; Tnat ] -> Tnat
  | Prim_not, [ Tint|Tnat ] -> Tint

  | Prim_abs, [ Tint ] -> Tint
  | Prim_is_nat, [ Tint ] -> Toption Tnat
  | Prim_int, [ Tnat ] -> Tint
  | Prim_sub, [ Tint|Tnat ] -> Tint

  | (Prim_lsl|Prim_lsr), [ Tnat ; Tnat ] -> Tnat



  | Prim_tuple, ty_args -> Ttuple (List.map (fun e -> e.ty) args)

  | Prim_map_find,
    [ key_ty;
      (Tmap (expected_key_ty, value_ty) | Tbigmap (expected_key_ty, value_ty)) ]
    ->
     if expected_key_ty <> key_ty then
       error loc "bad Map.find key type";
     Toption value_ty
  | Prim_map_update,
    [ key_ty;
      Toption value_ty;
      ( Tmap (expected_key_ty, expected_value_ty)
      | Tbigmap (expected_key_ty, expected_value_ty)) as m]
    ->
     if expected_key_ty <> key_ty then
       error loc "bad Map.update key type";
     if expected_value_ty <> value_ty then
       error loc "bad Map.update value type";
     begin match m with
       | Tmap _ -> Tmap (key_ty, value_ty)
       | Tbigmap _ -> Tbigmap (key_ty, value_ty)
       |  _ -> assert false
     end
  | Prim_map_add,
    [ key_ty;
      value_ty;
      ( Tmap (expected_key_ty, expected_value_ty)
      | Tbigmap (expected_key_ty, expected_value_ty)) as m]
    ->
     if expected_key_ty <> key_ty then
       error loc "bad Map.add key type";
     if expected_value_ty <> value_ty then
       error loc "bad Map.add value type";
     begin match m with
       | Tmap _ -> Tmap (key_ty, value_ty)
       | Tbigmap _ -> Tbigmap (key_ty, value_ty)
       |  _ -> assert false
     end
  | Prim_map_remove,
    [ key_ty;
      ( Tmap (expected_key_ty, value_ty)
      | Tbigmap (expected_key_ty, value_ty)) as m]
    ->
     if expected_key_ty <> key_ty then
       error loc "bad Map.remove key type";
     begin match m with
       | Tmap _ -> Tmap (key_ty, value_ty)
       | Tbigmap _ -> Tbigmap (key_ty, value_ty)
       |  _ -> assert false
     end

  | Prim_map_mem,
    [ key_ty;
      (Tmap (expected_key_ty,_) | Tbigmap (expected_key_ty,_)) ]
    ->
     if expected_key_ty <> key_ty then
       error loc "bad Mem.mem key type";
     Tbool

  | Prim_set_mem,[ key_ty; Tset expected_key_ty]
    ->
     if expected_key_ty <> key_ty then
       error loc "bad Set.mem key type";
     Tbool

  | Prim_list_size, [ Tlist _]  ->  Tnat
  | Prim_set_size, [ Tset _]  ->  Tnat
  | Prim_map_size, [ Tmap _]  ->  Tnat

  | Prim_set_update, [ key_ty; Tbool; Tset expected_key_ty]
    ->
     if expected_key_ty <> key_ty then
       error loc "bad Set.update key type";
     Tset key_ty
  | Prim_set_add, [ key_ty; Tset expected_key_ty]
    ->
     if expected_key_ty <> key_ty then
       error loc "bad Set.add key type";
     Tset key_ty
  | Prim_set_remove, [ key_ty; Tset expected_key_ty]
    ->
     if expected_key_ty <> key_ty then
       error loc "bad Set.remove key type";
     Tset key_ty

  | Prim_Some, [ ty ] -> Toption ty
  | Prim_self, [ Tunit ] -> Tcontract env.t_contract_sig
  | Prim_now, [ Tunit ] -> Ttimestamp
  | Prim_balance, [ Tunit ] -> Ttez
  | Prim_source, [ Tunit ] -> Taddress
  | Prim_sender, [ Tunit ] -> Taddress
  | Prim_amount, [ Tunit ] -> Ttez
  | Prim_gas, [ Tunit ] -> Tnat
  | Prim_pack, [ _ ] -> Tbytes
  | Prim_blake2b, [ Tbytes ] -> Tbytes
  | Prim_sha256, [ Tbytes ] -> Tbytes
  | Prim_sha512, [ Tbytes ] -> Tbytes
  | Prim_hash_key, [ Tkey ] -> Tkey_hash
  | Prim_check, [ Tkey; Tsignature; Tbytes ] ->
     Tbool
  | Prim_check, _ ->
     error_prim loc Prim_check args [Tkey; Tsignature; Tbytes]

  | Prim_address, [ Tcontract _ ] ->
     Taddress

  | Prim_create_account, [ Tkey_hash; Toption Tkey_hash; Tbool; Ttez ] ->
     Ttuple [Toperation; Taddress]
  | Prim_create_account, _ ->
     error_prim loc Prim_create_account args
                [ Tkey_hash; Toption Tkey_hash; Tbool; Ttez ]

  | Prim_default_account, [ Tkey_hash ] ->
     Tcontract unit_contract_sig

  | Prim_set_delegate, [ Toption Tkey_hash ] ->
    Toperation

  | Prim_exec, [ ty;
                 ( Tlambda(from_ty, to_ty)
                 | Tclosure((from_ty, _), to_ty))] ->
     if ty <> from_ty then
       type_error loc "Bad argument type in function application" ty from_ty;
     to_ty

  | Prim_list_rev, [ Tlist ty ] -> Tlist ty

  | Prim_concat_two, [ Tstring; Tstring ] -> Tstring
  | Prim_concat_two, [ Tbytes; Tbytes ] -> Tbytes
  | Prim_string_concat, [ Tlist Tstring ] -> Tstring
  | Prim_bytes_concat, [ Tlist Tbytes ] -> Tbytes

  | Prim_Cons, [ head_ty; Tunit ] ->
     Tlist head_ty
  | Prim_Cons, [ head_ty; Tlist tail_ty ] ->
     if head_ty <> tail_ty then
       type_error loc "Bad types for list" head_ty tail_ty;
     Tlist tail_ty

  | Prim_string_size, [ Tstring ] -> Tnat
  | Prim_bytes_size, [ Tbytes ] -> Tnat

  | Prim_string_sub, [ Tnat; Tnat; Tstring ] -> Toption Tstring
  | Prim_bytes_sub, [ Tnat; Tnat; Tbytes ] -> Toption Tbytes

  | prim, _ ->
     error loc "Bad %d args for primitive %S:\n    %s\n" (List.length args)
           (LiquidTypes.string_of_primitive prim)
           (String.concat "\n    "
                          (List.map
                             (fun arg ->
                               LiquidPrinter.Liquid.string_of_type arg.ty)
                             args))
    ;

and typecheck_expected info env expected_ty exp =
  let exp = typecheck env exp in
  if exp.ty <> expected_ty && exp.ty <> Tfail then
    type_error (loc_exp exp)
               ("Unexpected type for "^info) exp.ty expected_ty;
  exp

and typecheck_apply ?name env prim loc args =
  let args = List.map (typecheck env)args in
  let prim, ty = typecheck_prim1 env prim loc args in
  mk ?name (Apply (prim, loc, args)) ty


and typecheck_entry env entry =
  (* let env = { env with clos_env = None } in *)
  let (env, _) =
    new_binding env entry.entry_sig.storage_name env.t_contract_storage in
  let (env, _) =
    new_binding env entry.entry_sig.parameter_name entry.entry_sig.parameter in
  let expected_ty = Ttuple [Tlist Toperation; env.t_contract_storage] in
  let code =
    typecheck_expected "return value" env expected_ty entry.code in
  { entry with code }

and typecheck_contract ~warnings ~decompiling env contract =
  let env =
    {
      warnings;
      annot = false;
      decompiling;
      counter = ref 0;
      vars = StringMap.empty;
      vars_counts = StringMap.empty;
      to_inline = ref StringMap.empty;
      force_inline = ref StringMap.empty;
      env = env;
      clos_env = None;
      t_contract_sig = sig_of_contract contract;
      t_contract_storage = contract.storage;
    } in

  let env, values, counts =
    List.fold_left (fun (env, values, counts) (name, inline, exp) ->
        let exp = typecheck env exp in
        let (env, count) = new_binding env name ~fail:exp.fail exp.ty in
        env, ((name, inline, exp) :: values), ((name, count) :: counts)
      ) (env, [], []) contract.values in
  let entries = List.map (typecheck_entry env) contract.entries in
  List.iter (fun (name, count) ->
      check_used env name (noloc env) (* TODO *) count
    ) counts;
  { contract with
    values = List.rev values;
    entries }

let typecheck_code env ?expected_ty code =
  match expected_ty with
  | Some expected_ty -> typecheck_expected "value" env expected_ty code
  | None -> typecheck env code


(* XXX just for printing, do not use *)
let rec type_of_const = function
  | CUnit -> Tunit
  | CBool _ -> Tbool
  | CInt _ -> Tint
  | CNat _ -> Tnat
  | CTez _ -> Ttez
  | CTimestamp _ -> Ttimestamp
  | CString _ -> Tstring
  | CBytes _ -> Tbytes
  | CKey _ -> Tkey
  | CSignature _ -> Tsignature
  | CAddress _ -> Taddress
  | CTuple l ->
    Ttuple (List.map type_of_const l)
  | CNone -> Toption Tunit
  | CSome c -> Toption (type_of_const c)
  | CMap [] -> Tmap (Tint, Tunit)
  | CMap ((k,e) :: _) -> Tmap (type_of_const k, type_of_const e)

  | CBigMap [] -> Tbigmap (Tint, Tunit)
  | CBigMap ((k,e) :: _) -> Tbigmap (type_of_const k, type_of_const e)

  | CList [] -> Tlist (Tunit)
  | CList (e :: _) -> Tlist (type_of_const e)

  | CSet [] -> Tset (Tint)
  | CSet (e :: _) -> Tset (type_of_const e)

  | CLeft c -> Tor (type_of_const c, Tunit)
  | CRight c -> Tor (Tunit, type_of_const c)

  | CKey_hash _ -> Tkey_hash
  | CContract _ -> Tcontract unit_contract_sig

  (* XXX just for printing *)
  | CRecord _ -> Trecord ("<record>", [])
  | CConstr _ -> Tsum ("<sum>", [])


let check_const_type ?(from_mic=false) ~to_tez loc ty cst =
  let top_ty, top_cst = ty, cst in
  let rec check_const_type ty cst =
    match ty, cst with
    | Tunit, CUnit -> CUnit
    | Tbool, CBool b -> CBool b

    | Tint, CInt s
    | Tint, CNat s -> CInt s

    | Tnat, CInt s
    | Tnat, CNat s -> CNat s

    | Tstring, CString s -> CString s

    | Tbytes, CBytes s -> CBytes s

    | Ttez, CTez s -> CTez s

    | Tkey, CKey s -> CKey s
    | Tkey, CBytes s -> CKey s

    | Tkey_hash, CKey_hash s -> CKey_hash s
    | Tkey_hash, CBytes s -> CKey_hash s

    | Tcontract _, CContract s -> CContract s
    | Tcontract _, CAddress s -> CAddress s
    | Tcontract { entries_sig = [{ parameter= Tunit }] } , CKey_hash s ->
      CKey_hash s
    | Tcontract _, CBytes s -> CContract s

    | Taddress, CAddress s -> CAddress s
    | Taddress, CContract s -> CContract s
    | Taddress, CKey_hash s -> CKey_hash s
    | Taddress, CBytes s -> CContract s

    | Ttimestamp, CTimestamp s -> CTimestamp s

    | Tsignature, CSignature s -> CSignature s
    | Tsignature, CBytes s -> CSignature s

    | Ttuple tys, CTuple csts ->
       begin
         try
           CTuple (List.map2 check_const_type tys csts)
         with Invalid_argument _ ->
           error loc "constant type mismatch (tuple length differs from type)"
       end

    | Toption _, CNone -> CNone
    | Toption ty, CSome cst -> CSome (check_const_type ty cst)

    | Tor (left_ty, _), CLeft cst ->
      CLeft (check_const_type left_ty cst)

    | Tor (_, right_ty), CRight cst ->
      CRight (check_const_type right_ty cst)

    | Tmap (ty1, ty2), CMap csts ->
       CMap (List.map (fun (cst1, cst2) ->
          check_const_type ty1 cst1,
          check_const_type ty2 cst2) csts)

    | Tbigmap (ty1, ty2), (CMap csts | CBigMap csts) -> (* allow map *)
       CBigMap (List.map (fun (cst1, cst2) ->
          check_const_type ty1 cst1,
          check_const_type ty2 cst2) csts)

    | Tlist ty, CList csts ->
       CList (List.map (check_const_type ty) csts)

    | Tset ty, CSet csts ->
       CSet (List.map (check_const_type ty) csts)

    | Trecord (rname, labels), CRecord fields ->
      CRecord (List.map (fun (f, cst) ->
          try
            let ty = List.assoc f labels in
            f, check_const_type ty cst
          with Not_found ->
            error loc "Record field %s is not in type %s" f rname
        ) fields)

    | Tsum (sname, constrs), CConstr (c, cst) ->
      CConstr (c,
               try
                 let ty = List.assoc c constrs in
                 check_const_type ty cst
               with Not_found ->
                 error loc "Constructor %s does not belong to type %s" c sname
              )

    | _ ->
      if from_mic then
        match ty, cst with
        | Ttimestamp, CString s ->
          begin (* approximation of correct tezos timestamp *)
            try Scanf.sscanf s "%_d-%_d-%_dT%_d:%_d:%_dZ%!" ()
            with _ ->
            try Scanf.sscanf s "%_d-%_d-%_d %_d:%_d:%_dZ%!" ()
            with _ ->
            try Scanf.sscanf s "%_d-%_d-%_dT%_d:%_d:%_d-%_d:%_d%!" ()
            with _ ->
            try Scanf.sscanf s "%_d-%_d-%_dT%_d:%_d:%_d+%_d:%_d%!" ()
            with _ ->
            try Scanf.sscanf s "%_d-%_d-%_d %_d:%_d:%_d-%_d:%_d%!" ()
            with _ ->
            try Scanf.sscanf s "%_d-%_d-%_d %_d:%_d:%_d+%_d:%_d%!" ()
            with _ ->
              error loc "Bad format for timestamp"
          end;
          CTimestamp s

         | Ttez, CString s -> CTez (to_tez s)
         | Tkey_hash, CString s -> CKey_hash s
         | Tcontract _, CString s -> CContract s
         | Tkey, CString s -> CKey s
         | Tsignature, CString s -> CSignature s

         | _ ->
           error loc "constant type mismatch, expected %s, got %s"
             (LiquidPrinter.Liquid.string_of_type top_ty)
             (LiquidPrinter.Liquid.string_of_type (type_of_const top_cst))
       else
         error loc "constant type mismatch, expected %s, got %s"
           (LiquidPrinter.Liquid.string_of_type top_ty)
           (LiquidPrinter.Liquid.string_of_type (type_of_const top_cst))

  in
  check_const_type ty cst

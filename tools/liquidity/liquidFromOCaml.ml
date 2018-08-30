(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* We use '\231' as suffix for integers and floats to encode the "tz"
    suffix.  *)

open LiquidTypes
open LiquidOCamlParser

type 'a ast_elt =
  | Syn_value of string * bool (* inline *) * 'a
  | Syn_contract of 'a contract
  | Syn_entry of 'a entry
  | Syn_init of syntax_init

let () =
  LiquidOCamlLexer.define_keywords
 [
    "and", AND;
    "as", AS;
    "assert", ASSERT;
    "begin", BEGIN;
    (*    "class", CLASS; *)
    (*    "constraint", CONSTRAINT; *)
    "do", DO;
    "done", DONE;
    "downto", DOWNTO;
    "else", ELSE;
    "end", END;
    (* "exception", EXCEPTION; *)
    (* "external", EXTERNAL; *)
    "false", FALSE;
    "for", FOR;
    "fun", FUN;
    "function", FUNCTION;
    (* "functor", FUNCTOR; *)
    "if", IF;
    "in", IN;
    (* "include", INCLUDE; *)
    (* "inherit", INHERIT; *)
    (* "initializer", INITIALIZER; *)
    (* "lazy", LAZY; *)
    "let", LET;
    "match", MATCH;
    (* "entry", METHOD; *)
    "contract", MODULE;
    (* "mutable", MUTABLE; *)
    (* "new", NEW; *)
    (* "nonrec", NONREC; *)
    (* "object", OBJECT; *)
    "of", OF;
    (* "open", OPEN; *)
    "or", OR;
    (*  "parser", PARSER; *)
    (* "private", PRIVATE; *)
    "rec", REC;
    "sig", SIG;
    "struct", STRUCT;
    "then", THEN;
    "to", TO;
    "true", TRUE;
    (* "try", TRY; *)
    "type", TYPE;
    "val", VAL;
    (* "virtual", VIRTUAL; *)

    (* "when", WHEN; *)
    "while", WHILE;
    "with", WITH;

    "lor", INFIXOP3("lor"); (* Should be INFIXOP2 *)
    "lxor", INFIXOP3("lxor"); (* Should be INFIXOP2 *)
    "mod", INFIXOP3("mod");
    "land", INFIXOP3("land");
    "lsl", INFIXOP4("lsl");
    "lsr", INFIXOP4("lsr");
    "xor", INFIXOP3("xor"); (* Should be INFIXOP2 *)
    "asr", INFIXOP4("asr")
]

let ident_counter = ref 0

(* The minimal version of liquidity files that are accepted by this compiler *)
let minimal_version = 0.3

(* The maximal version of liquidity files that are accepted by this compiler *)
let maximal_version = 0.36


open Asttypes
open Longident
open Parsetree
open LiquidTypes

let loc_of_loc loc =
  let open Lexing in
  {
    loc_file =
      loc.Location.loc_start.pos_fname;
    loc_pos = Some (
        (loc.Location.loc_start.pos_lnum,
         loc.Location.loc_start.pos_cnum - loc.Location.loc_start.pos_bol),
        (loc.Location.loc_end.pos_lnum,
         loc.Location.loc_end.pos_cnum - loc.Location.loc_end.pos_bol)
      )
  }


let ppf = Format.err_formatter

let default_args = [
    "storage", Tunit;
    "parameter", Tunit;
  ]

let error_loc loc msg =
  let loc = loc_of_loc loc in
  LiquidLoc.raise_error ~loc "Unexpected syntax: %s%!" msg

let error_version loc msg =
  let loc = loc_of_loc loc in
  LiquidLoc.raise_error ~loc "version mismatch %s%!" msg

let unbound_type loc ty =
  let loc = loc_of_loc loc in
  LiquidLoc.raise_error ~loc "Unbound type %S%!" ty

let unbound_contract_type loc ty =
  let loc = loc_of_loc loc in
  LiquidLoc.raise_error ~loc "Unbound contract type %S%!" ty

let unbound_contract loc ty =
  let loc = loc_of_loc loc in
  LiquidLoc.raise_error ~loc "Unbound contract %S%!" ty

let error_arg loc arg =
  let loc = loc_of_loc loc in
  LiquidLoc.raise_error ~loc "Unexpected argument %S%!" arg

let todo_loc loc msg =
  let loc = loc_of_loc loc in
  LiquidLoc.raise_error ~loc "Syntax %S not yet implemented%!" msg

let rec translate_type env ?expected typ =
  match typ with
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "unit" }, []) } -> Tunit
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "bool" }, []) } -> Tbool
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "int" }, []) } -> Tint
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "nat" }, []) } -> Tnat
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "tez" }, []) } -> Ttez
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "string" }, []) } -> Tstring
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "bytes" }, []) } -> Tbytes
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "timestamp" }, []) } -> Ttimestamp
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "key" }, []) } -> Tkey
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "key_hash" }, []) } -> Tkey_hash
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "signature" }, []) } -> Tsignature
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "operation" }, []) } -> Toperation
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "address" }, []) } -> Taddress

  | { ptyp_desc = Ptyp_constr ({ txt = Lident "option" }, [param_type]) } ->
    let expected = match expected with
      | Some (Toption ty) -> Some ty
      | _ -> None
    in
    Toption (translate_type env ?expected param_type)
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "list" }, [param_type]) } ->
    let expected = match expected with
      | Some (Tlist ty) -> Some ty
      | _ -> None
    in
    Tlist (translate_type env ?expected param_type)
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "set" }, [param_type]);
      ptyp_loc } ->
    let expected = match expected with
      | Some (Tset ty) -> Some ty
      | _ -> None
    in
    let elt_type = translate_type env ?expected param_type in
    if not @@ comparable_type elt_type then
      error_loc ptyp_loc @@ Printf.sprintf
        "type %S is not comparable, it cannot be used as element of a set"
        (LiquidPrinter.Liquid.string_of_type elt_type);
    Tset elt_type

  | { ptyp_desc = Ptyp_package ({ txt = Lident contract_type_name }, []) } ->
     begin
       try Tcontract (StringMap.find contract_type_name env.contract_types)
       with Not_found ->
         unbound_contract_type typ.ptyp_loc contract_type_name
     end
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "variant" },
                               [left_type; right_type]) } ->
    let expected1, expected2 = match expected with
      | Some (Tor (ty1, ty2)) -> Some ty1, Some ty2
      | _ -> None, None
    in
    Tor (translate_type env ?expected:expected1 left_type,
         translate_type env ?expected:expected2 right_type)

  | { ptyp_desc = Ptyp_constr ({ txt = Lident "map" },
                               [key_type; val_type]);
      ptyp_loc } ->
    let expected1, expected2 = match expected with
      | Some (Tmap (ty1, ty2)) -> Some ty1, Some ty2
      | _ -> None, None
    in
    let key_type = translate_type env ?expected:expected1 key_type in
    if not @@ comparable_type key_type then
      error_loc ptyp_loc @@ Printf.sprintf
        "type %S is not comparable, it cannot be used as key in a map"
        (LiquidPrinter.Liquid.string_of_type key_type);
    let val_type = translate_type env ?expected:expected2 val_type in
    Tmap (key_type, val_type)

  | { ptyp_desc = Ptyp_constr ({ txt = Lident "big_map" },
                               [key_type; val_type]);
      ptyp_loc } ->
    let expected1, expected2 = match expected with
      | Some (Tbigmap (ty1, ty2)) -> Some ty1, Some ty2
      | _ -> None, None
    in
    let key_type = translate_type env ?expected:expected1 key_type in
    if not @@ comparable_type key_type then
      error_loc ptyp_loc @@ Printf.sprintf
        "type %S is not comparable, it cannot be used as key in a big map"
        (LiquidPrinter.Liquid.string_of_type key_type);
    let val_type = translate_type env ?expected:expected2 val_type in
    Tbigmap (key_type, val_type)

  | { ptyp_desc = Ptyp_arrow (_, parameter_type, return_type) } ->
    (* TODO ? *)
    (* let expected = match expected with
     *   | Some (Tcontract ty) -> Some ty
     *   | _ -> None
     * in *)
    Tlambda (translate_type env (* ?expected:expected *) parameter_type,
             translate_type env return_type)

  | { ptyp_desc = Ptyp_tuple types } ->
    let expecteds = match expected with
      | Some (Ttuple tys) when List.length types = List.length tys ->
        List.map (fun ty -> Some ty) tys
      | _ -> List.map (fun _ -> None) types
    in
    Ttuple (List.map2 (fun ty expected -> translate_type env ?expected ty)
              types expecteds)

  | { ptyp_desc = Ptyp_constr ({ txt = Lident ty_name }, []) } ->
     begin
       try StringMap.find ty_name env.types
       with Not_found ->
         unbound_type typ.ptyp_loc ty_name
     end

  | { ptyp_desc = Ptyp_any; ptyp_loc } ->
    begin match expected with
      | Some ty -> ty
      | None -> error_loc ptyp_loc "cannot infer type"
    end

  | { ptyp_loc } -> error_loc ptyp_loc "in type"


exception NotAConstant

(* Translate an expression, expecting a constant. Fails with [NotAConstant]
 if the expression is not a constant. *)

let rec translate_const env exp =
  match exp with
  | { pexp_desc = Pexp_construct ( { txt = Lident "()" }, None ) } ->
     CUnit, Some Tunit
  | { pexp_desc = Pexp_construct ( { txt = Lident "true" }, None ) } ->
     CBool true, Some Tbool
  | { pexp_desc = Pexp_construct ( { txt = Lident "false" }, None ) } ->
     CBool false, Some Tbool
  | { pexp_desc = Pexp_construct ( { txt = Lident "None" }, None ) } ->
     CNone, None
  | { pexp_desc = Pexp_constant (Pconst_integer (s,None)) } ->
     CInt (LiquidPrinter.integer_of_liq s), Some Tint
  | { pexp_desc = Pexp_constant (Pconst_integer (s, Some 'p')) } ->
     CNat (LiquidPrinter.integer_of_liq s), Some Tnat

  | { pexp_desc = Pexp_constant (Pconst_integer (s, Some '\231')) } ->
     CTez (LiquidPrinter.tez_of_liq s), Some Ttez
  | { pexp_desc = Pexp_constant (Pconst_float (s, Some '\231')) } ->
     CTez (LiquidPrinter.tez_of_liq s), Some Ttez

  (* Timestamps *)
  | { pexp_desc = Pexp_constant (Pconst_integer (s, Some '\232')) } ->
     CTimestamp (ISO8601.of_string s), Some Ttimestamp

  (* Key_hash *)
  | { pexp_desc = Pexp_constant (Pconst_integer (s, Some '\233')) } ->
     CKey_hash s, Some Tkey_hash

  (* Address *)
  | { pexp_desc = Pexp_constant (Pconst_integer (s, Some '\236')) } ->
     CAddress s, Some Taddress

  (* Key *)
  | { pexp_desc = Pexp_constant (Pconst_integer (s, Some '\234')) } ->
     CKey s, Some Tkey

  (* Signature *)
  | { pexp_desc = Pexp_constant (Pconst_integer (s, Some '\235')) } ->
     CSignature s, Some Tsignature

  (* Bytes *)
  | { pexp_desc = Pexp_constant (Pconst_integer (s, Some '\237')) } ->
     CBytes s, Some Tbytes

  | { pexp_desc = Pexp_constant (Pconst_string (s, None)) } ->
     CString s, Some Tstring

  | { pexp_desc = Pexp_tuple exps } ->
     let csts, tys = List.split (List.map (translate_const env) exps) in
     let tys =
       try
         Some (Ttuple (List.map (function
                                 | None -> raise Exit
                                 | Some ty -> ty) tys))
       with Exit -> None
     in
     CTuple csts, tys

  | { pexp_desc = Pexp_construct (
                      { txt = Lident "[]" }, None) } ->
     CList [], None

  | { pexp_desc = Pexp_construct (
                      { txt = Lident "::" },
                      Some { pexp_desc = Pexp_tuple [head; tail] }) } ->
     let head, head_ty = translate_const env head in
     let tail, tail_ty = translate_const env tail in
     begin
       match tail with
       | CList tail ->
          let cst = CList (head :: tail) in
          let ty =
            match head_ty, tail_ty with
            | Some head_ty, Some (Tlist tail_ty) ->
               if head_ty <> tail_ty then
                 error_loc exp.pexp_loc "inconsistent types in list";
               Some (Tlist head_ty)
            | Some head_ty, None ->
               Some (Tlist head_ty)
            | _ ->
               error_loc exp.pexp_loc "inconsistent types in list"
          in
          cst, ty
       | _ ->
          error_loc exp.pexp_loc "inconsistent types in list"
     end

  | { pexp_desc = Pexp_construct ({ txt = Lident "Map" }, None) } ->
     CMap [], None

  | { pexp_desc = Pexp_construct ({ txt = Lident "BigMap" }, None) } ->
     CBigMap [], None

  | { pexp_desc = Pexp_construct (
      { txt = Lident ("Map" | "BigMap" as map_kind) },
      Some pair_list) } ->
     let pair_list = translate_list pair_list in
     let pair_list = List.map translate_pair pair_list in
     let pair_list = List.map (fun (e1,e2) ->
                         let cst1, ty1 = translate_const env e1 in
                         let cst2, ty2 = translate_const env e2 in
                         (cst1, cst2), (ty1, ty2)
                       ) pair_list in
     let pair_list = List.sort compare pair_list in
     let csts, tys = List.split pair_list in
     let tys = match tys with
       | (Some ty1, Some ty2) :: tail ->

          List.iter (function
                       (Some ty1', Some ty2') when ty1' = ty1 && ty2' = ty2
                                                   -> ()
                     | _ -> error_loc exp.pexp_loc
                              "inconsistent map types"
                    )
                     tail;
          begin match map_kind with
            | "Map" -> Some (Tmap (ty1, ty2))
            | "BigMap" -> Some (Tbigmap (ty1, ty2))
            | _ -> assert false
          end
       | _ -> None
     in
     begin match map_kind with
       | "Map" -> CMap csts, tys
       | "BigMap" -> CBigMap csts, tys
       | _ -> assert false
     end

  | { pexp_desc = Pexp_construct (
                      { txt = Lident "Set" }, None) } ->
     CSet [], None

  | { pexp_desc = Pexp_construct (
                      { txt = Lident "Set" }, Some pair_list) } ->
     let list = translate_list pair_list in
     let list = List.map (fun e1 ->
                    let cst1, ty1 = translate_const env e1 in
                    cst1, ty1
                  ) list in
     let list = List.sort compare list in
     let csts, tys = List.split list in
     let tys = match tys with
       | Some ty1 :: tail ->
          List.iter (function
                       Some ty1' when ty1' = ty1 -> ()
                     | _ -> error_loc exp.pexp_loc
                              "inconsistent set types"
                              )
                    tail;
          Some (Tset ty1)
       | _ -> None
     in
     CSet csts, tys


  | { pexp_desc = Pexp_construct (
                      { txt = Lident "Some" }, Some arg) } ->
     let arg, ty = translate_const env arg in
     let ty = match ty with
       | None -> None
       | Some ty -> Some (Toption ty)
     in
     CSome arg, ty

  | { pexp_desc = Pexp_construct (
                      { txt = Lident "Left" }, Some arg) } ->
    let arg, ty = translate_const env arg in
    let ty = match ty with
      | None -> None
      | Some ty -> Some (Tor (ty, Tunit (* dummy *)))
    in
    CLeft arg, ty

  | { pexp_desc = Pexp_construct (
                      { txt = Lident "Right" }, Some arg) } ->
    let arg, ty = translate_const env arg in
    let ty = match ty with
      | None -> None
      | Some ty -> Some (Tor (Tunit (* dummy *), ty))
    in
    CRight arg, ty

  | { pexp_desc = Pexp_construct ({ txt = Lident lid }, args) } ->
    begin
      try
        let ty_name, tya = StringMap.find lid env.constrs in
        let c =
          match args with
          | None -> CUnit
          | Some args ->
            let c, ty_opt = translate_const env args in
            let loc = loc_of_loc args.pexp_loc in
            LiquidCheck.check_const_type ~to_tez:LiquidPrinter.tez_of_liq
              loc tya c
            (* begin match ty_opt with
             *   | None -> ()
             *   | Some ty ->
             *     if ty <> tya then
             *       error_loc exp.pexp_loc
             *         ("wrong type for argument of constructor "^lid)
             * end;
             * c *)
        in
        let ty = StringMap.find ty_name env.types in
        CConstr (lid, c), Some ty
      with Not_found -> raise NotAConstant
    end

  | { pexp_desc = Pexp_record (lab_x_exp_list, None) } ->
    let lab_x_exp_list =
      List.map (function
            ({ txt = Lident label; loc }, exp) ->
            let loc = loc_of_loc exp.pexp_loc in
            let _, _, ty' = StringMap.find label env.labels in
            let c, ty_opt = translate_const env exp in
            (* begin match ty_opt with
             * | None -> ()
             * | Some ty ->
             *   if ty <> ty' then
             *     error_loc loc ("wrong type for label "^label)
             * end; *)
            let c =
              LiquidCheck.check_const_type ~to_tez:LiquidPrinter.tez_of_liq
                loc ty' c
            in
            label, c
          | ( { loc }, _) ->
            error_loc loc "label expected"
        ) lab_x_exp_list in
    let ty = match lab_x_exp_list with
      | [] -> error_loc exp.pexp_loc "empty record"
      | (label, _) :: _ ->
        try
          let ty_name, _, _ = StringMap.find label env.labels in
          StringMap.find ty_name env.types
        with Not_found -> error_loc exp.pexp_loc ("unknown label " ^ label)
    in
    CRecord lab_x_exp_list, Some ty

  | { pexp_desc = Pexp_constraint (cst, ty) } ->
     let cst, tyo = translate_const env cst in
     let ty = translate_type env ?expected:tyo ty in
     begin
       let loc = loc_of_loc exp.pexp_loc in
       match tyo with
       | None ->
          LiquidCheck.check_const_type ~to_tez:LiquidPrinter.tez_of_liq loc ty cst, Some ty
       | Some ty_infer ->
          LiquidCheck.check_const_type ~to_tez:LiquidPrinter.tez_of_liq loc ty cst, Some ty
     end

  | _ -> raise NotAConstant

and translate_list exp =
  match exp.pexp_desc with
  | Pexp_construct({ txt = Lident "[]" }, None) -> []

  | Pexp_construct({ txt = Lident "::" },
                   Some { pexp_desc = Pexp_tuple [e1; e2] }) ->
     e1 :: translate_list e2

  | _ -> error_loc exp.pexp_loc "list expected"

and translate_pair exp =
  match exp.pexp_desc with
  | Pexp_tuple [e1; e2] -> (e1, e2)
  | _ -> error_loc exp.pexp_loc "pair expected"


let mk desc = mk desc ()

let vars_info_pat env pat =
  let rec vars_info_pat_aux acc indexes = function
    | { ppat_desc = Ppat_constraint (pat, ty) } ->
      let acc, _ = vars_info_pat_aux acc indexes pat in
      acc, translate_type env ty

    | { ppat_desc = Ppat_var { txt = var; loc } } ->
      (var, loc_of_loc loc, indexes) :: acc, Tunit (* Dummy type value *)

    | { ppat_desc = Ppat_any; ppat_loc } ->
      ("_", loc_of_loc ppat_loc, indexes) :: acc, Tunit (* Dummy type value *)

    | { ppat_desc = Ppat_tuple pats } ->
      let _, acc, tys =
        List.fold_left (fun (i, acc, tys) pat ->
            let acc, ty = vars_info_pat_aux acc (i :: indexes) pat in
            i + 1, acc, ty :: tys
          ) (0, acc, []) pats
      in
      acc, Ttuple (List.rev tys)

    | { ppat_loc } ->
      error_loc ppat_loc "cannot deconstruct this pattern"
  in
  vars_info_pat_aux [] [] pat

let access_of_deconstruct var_name loc indexes =
  let a = mk (Var (var_name, loc, [])) in
  List.fold_right (fun i a ->
      mk (Apply (Prim_tuple_get, loc, [
          a;
          mk (Const (loc, Tnat, CNat (LiquidPrinter.integer_of_int i)))
        ]))
    ) indexes a

let deconstruct_pat env pat e =
  let vars_infos, ty = vars_info_pat env pat in
  match vars_infos with
  | [] -> assert false
  | [v, _loc, []] -> v, ty, e
  | _ ->
    let var_name =
      String.concat "_" ( "" :: (List.rev_map (fun (v,_,_) -> v) vars_infos)) in
    let e =
      List.fold_left (fun e (v, loc, indexes) ->
          let access = access_of_deconstruct var_name loc indexes in
          mk (Let (v, false, loc, access, e))
        ) e vars_infos
    in
    var_name, ty, e

let rec translate_code contracts env exp =
  let loc = loc_of_loc exp.pexp_loc in
  let desc =
    match exp with
    | { pexp_desc = Pexp_ident ( { txt = Lident var } ) } ->
       Var (var, loc, [])
    | { pexp_desc = Pexp_ident ( { txt = Ldot(Lident m, var) } ) } ->
       Var (m ^ "." ^ var, loc, [])

    | { pexp_desc = Pexp_field (exp, { txt = Lident label }) } ->
       begin
         let e = translate_code contracts env exp
         in
         match e.desc with
         | Var (name, loc, labels) ->
            Var (name, loc, labels @ [label])
         | _ -> error_loc exp.pexp_loc "variable expected"
       end
    | { pexp_desc = Pexp_setfield (exp, { txt = Lident label }, arg) } ->
       begin
         let e = translate_code contracts env exp in
         let arg = translate_code contracts env arg
         in
         match e.desc with
         | Var (name, loc, labels) ->
            SetVar (name, loc, labels @ [label], arg)
         | _ -> error_loc exp.pexp_loc "variable expected"
       end


    | { pexp_desc = Pexp_ifthenelse (e1, e2, None) } ->
       If (translate_code contracts env e1,
           translate_code contracts env e2,
           mk (Const (loc, Tunit, CUnit)))
    | { pexp_desc = Pexp_ifthenelse (e1, e2, Some e3) } ->
       If (translate_code contracts env e1, translate_code contracts env e2, translate_code contracts env e3)
    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident
                  { txt = Ldot(Lident "Contract", "call") } },
            [
              Nolabel, contract_exp;
              Nolabel, tez_exp;
              Nolabel, arg_exp;
            ]) } ->
      Transfer (loc,
                translate_code contracts env contract_exp,
                translate_code contracts env tez_exp,
                None,
                translate_code contracts env arg_exp)
    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident
                  { txt = Ldot(Lident "Contract", "create") } },
            [
              Nolabel, delegate_exp;
              Nolabel, manager_exp;
              Nolabel, delegatable_exp;
              Nolabel, spendable_exp;
              Nolabel, amout_exp;
              Nolabel, storage_exp;
              Nolabel,
              { pexp_desc =
                  Pexp_pack { pmod_desc =
                                Pmod_ident { txt = Lident contract_name };
                              pmod_loc } }
            ]) } ->
      CreateContract
        (loc,
         [translate_code contracts env delegate_exp;
          translate_code contracts env manager_exp;
          translate_code contracts env delegatable_exp;
          translate_code contracts env spendable_exp;
          translate_code contracts env amout_exp;
          translate_code contracts env storage_exp;
         ],
         try StringMap.find contract_name contracts
         with Not_found ->
           unbound_contract pmod_loc contract_name
        )
    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident
                  { txt = Ldot(Lident "Contract", "create") } }, _);
        pexp_loc } ->
      error_loc pexp_loc
        "Wrong number or order of arguements for Contract.create.\n\
         Expected syntax is : Contract.create <delegate> <manager> \
         <delegatable> <spendable> <amount> <initial_storage> \
         (fun (parameter : <parameter_type>) (storage : <storage_type>) -> \
         <contract code>)"

    | { pexp_desc =
          Pexp_constraint (
            { pexp_desc =
                Pexp_apply (
                  { pexp_desc = Pexp_ident
                        { txt = Ldot(Lident "Contract", "at") } },
                  [
                    Nolabel, addr_exp;
                  ]) },
            pty) } ->

      let ty = match translate_type env pty with
        | Toption ((Tcontract _) as ty) -> ty
        | _ -> error_loc pty.ptyp_loc
                 "Contract.at type must be 'a contract option for some 'a"
      in
      ContractAt (loc, translate_code contracts env addr_exp, ty)

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident
                  { txt = Ldot(Lident "Contract", "at") } },
            [
              Nolabel, addr_exp;
            ]);
          pexp_loc } ->
      error_loc pexp_loc
        "Contract.at must be annotated by the resulting contract type (option)"

    | { pexp_desc =
          Pexp_constraint (
            { pexp_desc =
                Pexp_apply (
                  { pexp_desc = Pexp_ident
                        { txt = Ldot(Lident "Bytes", "unpack") } },
                  [
                    Nolabel, exp;
                  ]) },
            pty) } ->

      let ty = match translate_type env pty with
        | Toption p -> p
        | _ -> error_loc pty.ptyp_loc
                 "Bytes.unpack type must be 'a option for some 'a"
      in
      Unpack (loc, translate_code contracts env exp, ty)

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident
                  { txt = Ldot(Lident "Bytes", "unpack") } },
            [
              Nolabel, addr_exp;
            ]);
          pexp_loc } ->
      error_loc pexp_loc
        "Bytes.unpack must be annotated by the resulting type (option)"

    | { pexp_desc = Pexp_let (Nonrecursive, [ {
        pvb_pat = pat;
        pvb_expr = var_exp;
        pvb_attributes = attrs;
      } ], body) } ->
      let exp, body = translate_code contracts env var_exp, translate_code contracts env body in
      let var_name, _, body = deconstruct_pat env pat body in
      let inline = match attrs with
        | [ { txt = "inline"} , PStr [] ] -> true
        | _ -> false in
      Let (var_name, inline, loc_of_loc pat.ppat_loc, exp, body)

    | { pexp_desc = Pexp_sequence (exp1, exp2) } ->
       Seq (translate_code contracts env exp1, translate_code contracts env exp2)

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident
                  ( { txt = (Lident "failwith"
                            | Ldot(Lident "Current", "failwith")) })},
            [
              Nolabel, err
            ]);
        pexp_loc
      } ->
      let err = translate_code contracts env err in
      Failwith (err, loc_of_loc pexp_loc)

    | { pexp_desc =
          Pexp_apply (
              { pexp_desc = Pexp_ident ( { txt = Ldot(Lident "Loop",
                                                      "loop");
                                           loc } ) },
              [
                Nolabel, { pexp_desc =
                             Pexp_fun (Nolabel,None,
                                       pat,
                                       body
                         ) };
                Nolabel, arg
      ]) } ->
      let name, body =
        match pat.ppat_desc with
        | Ppat_var { txt = name } -> name, body
        | _ ->
          incr ident_counter;
          let name = Printf.sprintf "tmp#%d" !ident_counter in
          let body =
            { exp with
              pexp_desc =
                Pexp_let(Nonrecursive,
                         [{ pvb_pat = pat;
                            pvb_expr = { exp with
                                         pexp_desc = Pexp_ident(
                                             { txt = Lident name; loc })};
                            pvb_attributes = [];
                            pvb_loc = exp.pexp_loc;
                          }],
                         body) } in
          name, body
      in
      let body = translate_code contracts env body in
      let arg = translate_code contracts env arg in
      Loop (name, loc_of_loc exp.pexp_loc, body, arg)

    | { pexp_desc =
          Pexp_apply (
              { pexp_desc = Pexp_ident ( { txt = Ldot(Lident iter_coll,
                                                      "iter") } ) },
              [
                Nolabel, { pexp_desc = Pexp_fun (Nolabel,None, pat, body) };
                Nolabel, arg
              ]) } ->
       let body = translate_code contracts env body in
       let arg = translate_code contracts env arg in
       let var_name, _, body = deconstruct_pat env pat body in
       let prim = LiquidTypes.fold_primitive_of_string (iter_coll^".iter") in
       let acc = mk (Const (loc, Tunit, CUnit)) in
       Fold (prim, var_name, loc, body, arg, acc)

    | { pexp_desc =
          Pexp_apply (
              { pexp_desc = Pexp_ident ( { txt = Ldot(Lident iter_coll,
                                                      "fold") } ) },
              [ Nolabel, { pexp_desc = Pexp_fun (Nolabel,None, pat, body) };
                Nolabel, arg;
                Nolabel, acc;
              ]) } ->
       let arg = translate_code contracts env arg in
       let acc = translate_code contracts env acc in
       let body = translate_code contracts env body in
       let var_name, _, body = deconstruct_pat env pat body in
       let prim = LiquidTypes.fold_primitive_of_string (iter_coll^".fold") in
       Fold (prim, var_name, loc, body, arg, acc)

    | { pexp_desc =
          Pexp_apply (
              { pexp_desc = Pexp_ident ( { txt = Ldot(Lident map_coll,
                                                      "map") } ) },
              [ Nolabel, { pexp_desc = Pexp_fun (Nolabel,None, pat, body) };
                Nolabel, arg;
              ]) } ->
       let arg = translate_code contracts env arg in
       let body = translate_code contracts env body in
       let var_name, _, body = deconstruct_pat env pat body in
       let prim = LiquidTypes.map_primitive_of_string (map_coll^".map") in
       Map (prim, var_name, loc, body, arg)

    | { pexp_desc =
          Pexp_apply (
              { pexp_desc = Pexp_ident ( { txt = Ldot(Lident map_fold_coll,
                                                      "map_fold") } ) },
              [ Nolabel, { pexp_desc = Pexp_fun (Nolabel,None, pat, body) };
                Nolabel, arg;
                Nolabel, acc;
              ]) } ->
       let arg = translate_code contracts env arg in
       let acc = translate_code contracts env acc in
       let body = translate_code contracts env body in
       let var_name, _, body = deconstruct_pat env pat body in
       let prim = LiquidTypes.map_fold_primitive_of_string (map_fold_coll^".map_fold") in
       MapFold (prim, var_name, loc, body, arg, acc)

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident ( { txt = Ldot(Lident iter_coll,
                                                    "iter");
                                         loc = vloc } ) },
            [
              Nolabel, f_exp;
              Nolabel, arg
            ]) } ->
      let f = translate_code contracts env f_exp in
      let name = "_iter_arg" in
      let name_var = mk (Var(name, loc_of_loc vloc, [])) in
      let body =
        mk (Apply(Prim_exec, loc, [name_var; f])) in
      let arg = translate_code contracts env arg in
      let prim = LiquidTypes.fold_primitive_of_string (iter_coll^".iter") in
      let acc = mk (Const (loc, Tunit, CUnit)) in
      Fold (prim, name, loc, body, arg, acc)

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident ( { txt = Ldot(Lident iter_coll,
                                                    "fold");
                                         loc = vloc } ) },
            [
              Nolabel, f_exp;
              Nolabel, arg;
              Nolabel, acc;
            ]) } ->
      let f = translate_code contracts env f_exp in
      let name = "_fold_arg" in
      let name_var = mk (Var(name, loc_of_loc vloc, [])) in
      let body =
        mk (Apply(Prim_exec, loc, [name_var; f])) in
      let arg = translate_code contracts env arg in
      let acc = translate_code contracts env acc in
      let prim = LiquidTypes.fold_primitive_of_string (iter_coll^".fold") in
      Fold (prim, name, loc, body, arg, acc)

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident ( { txt = Ldot(Lident map_coll,
                                                    "map");
                                         loc = vloc } ) },
            [
              Nolabel, f_exp;
              Nolabel, arg;
            ]) } ->
      let f = translate_code contracts env f_exp in
      let name = "_map_arg" in
      let name_var = mk (Var(name, loc_of_loc vloc, [])) in
      let body =
        mk (Apply(Prim_exec, loc, [name_var; f])) in
      let arg = translate_code contracts env arg in
      let prim = LiquidTypes.map_primitive_of_string (map_coll^".map") in
      Map (prim, name, loc, body, arg)

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident ( { txt = Ldot(Lident map_fold_coll,
                                                    "map_fold");
                                         loc = vloc } ) },
            [
              Nolabel, f_exp;
              Nolabel, arg;
              Nolabel, acc;
            ]) } ->
      let f = translate_code contracts env f_exp in
      let name = "_map_fold_arg" in
      let name_var = mk (Var(name, loc_of_loc vloc, [])) in
      let body =
        mk (Apply(Prim_exec, loc, [name_var; f])) in
      let arg = translate_code contracts env arg in
      let acc = translate_code contracts env acc in
      let prim =
        LiquidTypes.map_fold_primitive_of_string (map_fold_coll^".map_fold") in
      MapFold (prim, name, loc, body, arg, acc)


    | { pexp_desc = Pexp_apply (exp, args) } ->
       let exp = translate_code contracts env exp in
       Apply(Prim_unknown, loc, exp :: List.map (
                                         function (Nolabel, exp) ->
                                                  translate_code contracts env exp
                                                | (_, { pexp_loc }) ->
                                                   error_loc pexp_loc "in arg"
                                       ) args)

    | { pexp_desc = Pexp_extension (
        { txt = "nat" },
        PStr [{ pstr_desc = Pstr_eval (
            { pexp_desc = Pexp_match (e, cases); pexp_loc },
            [])
          }]
      )} ->
      let e = translate_code contracts env e in
      let cases = List.map (translate_case contracts env) cases in
      begin
        match cases with
        | [ CConstr ("Plus", [p]), ifplus; CConstr ("Minus", [m]), ifminus ]
        | [ CConstr ("Minus", [m]), ifminus; CConstr ("Plus", [p]), ifplus ] ->
          MatchNat(e, loc_of_loc pexp_loc, p, ifplus, m, ifminus)
        | [ CConstr ("Plus", [p]), ifplus; CAny, ifminus ] ->
          MatchNat(e, loc_of_loc pexp_loc, p, ifplus, "_", ifminus)
        | [ CConstr ("Minus", [m]), ifminus; CAny, ifplus ] ->
          MatchNat(e, loc_of_loc pexp_loc, "_", ifplus, m, ifminus)
        | _ -> error_loc pexp_loc "match%nat patterns are Plus _, Minus _"
      end

    | { pexp_desc = Pexp_match (e, cases) } ->
       let e = translate_code contracts env e in
       let cases = List.map (translate_case contracts env) cases in
       begin
         match cases with
         | [ CConstr ("None", []), ifnone; CConstr ("Some", [arg]), ifsome ]
         | [ CConstr ("Some", [arg]), ifsome; CConstr ("None", []), ifnone ]
         | [ CConstr ("Some", [arg]), ifsome; CAny, ifnone ] ->
           MatchOption(e, loc, ifnone, arg, ifsome)
         | [ CConstr ("None", []), ifnone; CAny, ifsome ] ->
           MatchOption(e, loc, ifnone, "_", ifsome)

         | [ CConstr ("[]", []), ifnil; CConstr ("::", [head; tail]), ifcons ]
         | [ CConstr ("::", [head; tail]), ifcons; CConstr ("[]", []), ifnil ]
         | [ CConstr ("::", [head; tail]), ifcons; CAny, ifnil ] ->
           MatchList(e, loc, head, tail, ifcons, ifnil)
         | [ CConstr ("[]", []), ifnil; CAny, ifcons ] ->
           MatchList(e, loc, "_head", "_tail", ifcons, ifnil)

         | args ->
           MatchVariant(e, loc, args)
       end

    | { pexp_desc = Pexp_fun (Nolabel, None, pat, body_exp) } ->
       let body_exp = translate_code contracts env body_exp in
       let arg_name, arg_type, body_exp = deconstruct_pat env pat body_exp in
       Lambda (arg_name, arg_type, loc, body_exp,
               Tunit) (* not yet inferred *)

    | { pexp_desc = Pexp_record (lab_x_exp_list, None) } ->
       let lab_x_exp_list =
         List.map (function
                     ({ txt = Lident label }, exp) ->
                     label, translate_code contracts env exp
                   | ( { loc }, _) ->
                      error_loc loc "label expected"
                  ) lab_x_exp_list in
       Record (loc, lab_x_exp_list)

    | exp ->
       match translate_const env exp with
       | _, None -> error_loc exp.pexp_loc "constant needs a type annotation"
       | cst, Some ty -> Const (loc, ty, cst)
       | exception NotAConstant ->
          match exp with
          | { pexp_desc = Pexp_construct (
                              { txt = Lident "Some" }, Some args) } ->
             Apply(Prim_Some, loc, [translate_code contracts env args])

          (* Ok, this is a very special case. We accept
not knowing the full type of the empty list because it will be infered
from the head element. We use unit for that type. *)
          | { pexp_desc =
                Pexp_construct (
                    { txt = Lident "::" },
                    Some { pexp_desc =
                             Pexp_tuple
                               [a;
                                { pexp_desc =
                                    Pexp_construct(
                                        { txt = Lident "[]" }, None) }]}) }
            ->
             Apply(Prim_Cons, loc,
                   [translate_code contracts env a;
                    mk (Const (loc, Tunit, CList [])) (* XXX ? *)
                  ])

          | { pexp_desc = Pexp_construct (
                              { txt = Lident "::" },
                              Some { pexp_desc = Pexp_tuple [a;b]}) } ->
             Apply(Prim_Cons, loc,
                   [translate_code contracts env a;
                    translate_code contracts env b])




          | { pexp_desc = Pexp_construct (
                              { txt = Lident lid }, args) } ->
             begin
               try
                 let (_ty_name, _ty) = StringMap.find lid env.constrs in
                 Constructor( loc, Constr lid,
                              match args with
                              | None -> mk (Const (loc, Tunit, CUnit))
                              | Some arg -> translate_code contracts env arg )
               with Not_found -> match lid with
                 | "Map" | "Set" | "BigMap" ->
                   error_loc exp.pexp_loc @@ Printf.sprintf
                     "constructor %s can only be used with a constant argument"
                     lid
                 | _ ->
                   error_loc exp.pexp_loc ("unknown constructor : " ^ lid)
             end

          | { pexp_desc =
                Pexp_constraint (
                    { pexp_desc =
                        Pexp_construct (
                          { txt = Lident "Left" }, args) },
                    pty
                  ) } ->
            let right_ty = match pty with
              | { ptyp_desc = Ptyp_constr (
                  { txt = Lident "variant" },
                  [ left_ty; right_ty ])} ->
                translate_type env right_ty
              | _ -> match translate_type env pty with
                | Tor (_, right_ty) -> right_ty
                | _ -> error_loc pty.ptyp_loc
                         "Type of Left must be a variant"
            in
            Constructor( loc,
                         Left right_ty,
                         match args with
                         | None -> mk (Const (loc, Tunit, CUnit))
                         | Some arg -> translate_code contracts env arg )

          | { pexp_desc =
                Pexp_constraint (
                  { pexp_desc =
                      Pexp_construct (
                        { txt = Lident "Right" }, args) },
                  pty
                ) } ->
            let left_ty = match pty with
              | { ptyp_desc = Ptyp_constr (
                  { txt = Lident "variant" },
                  [ left_ty; right_ty ])} ->
                translate_type env left_ty
              | _ -> match translate_type env pty with
                | Tor (left_ty, _) -> left_ty
                | _ -> error_loc pty.ptyp_loc
                         "Type of Right must be a variant"
            in
            Constructor( loc,
                         Right left_ty,
                         match args with
                         | None -> mk (Const (loc, Tunit, CUnit))
                         | Some arg -> translate_code contracts env arg )

          (* TODO *)
          | { pexp_desc = Pexp_tuple exps } ->
            let exps = List.map (translate_code contracts env) exps in
            begin
              try
                let tys, csts = List.split (
                    List.map (
                      function
                      | { desc = Const (_loc, ty, cst) } -> (ty, cst)
                      | _ -> raise Exit
                    ) exps)
                in
                Const (loc, Ttuple tys, CTuple csts)
              with Exit ->
                Apply(Prim_tuple, loc_of_loc exp.pexp_loc, exps)
            end

          | { pexp_loc } ->
             error_loc pexp_loc
                       ("in expression " ^
                          LiquidOCamlPrinter.string_of_expression exp
                       (*    LiquidOCamlPrinter.exp_ast exp *)
                       )


  (*
    | { pexp_desc = Pexp_construct ({ txt = Lident id }, None) } ->
       todo_loc ("constructor " ^ id) exp.pexp_loc
   *)
  in
  mk desc

and translate_case contracts env case =
  match case.pc_guard with
  | Some e  ->
     error_loc e.pexp_loc "when forbidden"
  | None ->
     let e = case.pc_rhs in
     let e = translate_code contracts env e in
     match case.pc_lhs with
     | { ppat_desc = Ppat_any } ->
        (CAny, e)
     | { ppat_desc =
           Ppat_construct (
             { txt = Lident ("Some" | "::" | "Plus" | "Minus" as c) },
             None);
         ppat_loc }  ->
       error_loc ppat_loc
         (Printf.sprintf "Constructor %S takes arguments, was given 0" c)
     | { ppat_desc =
           Ppat_construct ( { txt = Lident ("None" | "[]" as c) }, Some _);
         ppat_loc }  ->
       error_loc ppat_loc (Printf.sprintf "Constructor %S takes no arguments" c)
     | { ppat_desc = Ppat_construct ( { txt = Lident name } , None) }  ->
        (CConstr (name, []), e)
     | { ppat_desc =
           Ppat_construct (
               { txt = Lident "::" } ,
               Some { ppat_desc = Ppat_tuple [ p1; p2 ]}
       ) }  ->
       (CConstr ("::", [var_of_pat p1; var_of_pat p2]), e)

     | { ppat_desc = Ppat_construct ({ txt = Lident name } , Some pat) }  ->
       let var_name, _, e = deconstruct_pat env pat e in
       (CConstr (name, [var_name]), e)

     | { ppat_loc } ->
        error_loc ppat_loc "bad pattern"

and var_of_pat = function
    | { ppat_desc = Ppat_var { txt = var } } -> var
    | { ppat_desc = Ppat_any } -> "_"
    | { ppat_desc = Ppat_construct _; ppat_loc } ->
      error_loc ppat_loc "cannot match deep"
    | { ppat_loc } ->
      error_loc ppat_loc "bad pattern"

let rec inline_funs exp = function
  | [] -> exp
  | (f_pvb, f_loc) :: funs ->
    let f_in_exp = {
      pexp_loc = f_loc;
      pexp_desc = Pexp_let (Nonrecursive, [f_pvb], exp);
      pexp_attributes = []; (* dummy value *)
    } in
    inline_funs f_in_exp funs


let rec translate_entry name env contracts head_exp parameter storage_name =
  match head_exp with
  | { pexp_desc =
        Pexp_fun (
            Nolabel, None,
            { ppat_desc =
                Ppat_constraint(
                    { ppat_desc =
                        Ppat_var { txt = parameter_name } },
                    arg_type)
            },
            head_exp) } when parameter = None ->
    translate_entry name env contracts head_exp
      (Some (parameter_name, translate_type env arg_type))
      storage_name

  | { pexp_desc =
        Pexp_fun (
            Nolabel, None,
            { ppat_desc =
                Ppat_constraint(
                    { ppat_desc =
                        Ppat_var { txt = sto_name } },
                    arg_type)
            },
            head_exp);
      pexp_loc }
    when storage_name = None && parameter <> None ->
    let storage_ty = translate_type env arg_type in
    begin match StringMap.find_opt "storage" env.types with
      | None ->
        error_loc pexp_loc "type storage is required but not provided"
      | Some s ->
        if s <> storage_ty then
          LiquidLoc.raise_error ~loc:(loc_of_loc pexp_loc)
            "storage argument %s for entry point %s must be the same type \
             as contract storage" sto_name name;
        translate_entry name env contracts head_exp parameter (Some sto_name)
    end

  | { pexp_desc =
        Pexp_fun (
          Nolabel, None,
          { ppat_desc = Ppat_var { txt = sto_name } },
          head_exp);
      pexp_loc }
    when storage_name = None && parameter <> None ->
    begin try StringMap.find "storage" env.types |> ignore
      with Not_found ->
        error_loc pexp_loc "type storage is required but not provided"
    end;
    translate_entry name env contracts head_exp parameter (Some sto_name)

  | { pexp_desc = Pexp_fun (Nolabel, None, { ppat_desc = _ }, _);
      pexp_loc }  ->
    LiquidLoc.raise_error ~loc:(loc_of_loc pexp_loc)
      "entry point %s accepts two arguments" name

  | { pexp_loc } when parameter = None || storage_name = None ->
    LiquidLoc.raise_error ~loc:(loc_of_loc pexp_loc)
      "entry point %s needs two arguments" name

  | { pexp_desc = Pexp_constraint (head_exp, return_type); pexp_loc } ->
    begin match translate_type env return_type with
      | Ttuple [ ret_ty; sto_ty ] ->
        let storage = StringMap.find "storage" env.types in
        if sto_ty <> storage then
          error_loc pexp_loc
            "Second component of return type must be identical to storage type";
        if ret_ty <> Tlist Toperation then
          error_loc pexp_loc
            "First component of return type must be an operation list"
      | _ ->
        error_loc pexp_loc
          "return type must be a product of \"operation list\" \
           and the storage type"
    end;
    translate_entry name env contracts head_exp parameter storage_name

(*
  | { pexp_desc =
        Pexp_fun (
            Nolabel, None,
            { ppat_desc =
                Ppat_extension ({ txt = "invariant"},
                                PStr [{ pstr_desc = Pstr_eval (exp, [])}])
            },
            head_exp) } ->
    Format.eprintf "invariant@.";
    translate_entry name env contracts head_exp parameter storage_name

  | { pexp_desc =
        Pexp_fun (
            Nolabel, None,
            { ppat_desc =
                Ppat_constraint(
                    { ppat_desc =
                        Ppat_var { txt } },
                    arg_type)
            },
            head_exp);
      pexp_loc } ->
     error_arg pexp_loc txt
*)

  | exp ->
    let code = translate_code contracts env exp in
    let parameter_name, parameter = match parameter with
      | Some p -> p
      | None -> assert false in
    let storage_name = match storage_name with
      | Some s -> s
      | None -> assert false in
     {
       entry_sig = {
         entry_name = name;
         parameter;
         parameter_name;
         storage_name;
       };
       code;
     }

let rec translate_initial_storage env contracts exp args =
  match exp with
  | { pexp_desc =
        Pexp_fun (
            Nolabel, None,
            { ppat_desc =
                Ppat_constraint(
                    { ppat_desc = Ppat_var { txt = arg; loc } },
                    arg_type)
            },
            exp) } ->
    translate_initial_storage env contracts exp
      ((arg, loc_of_loc loc, translate_type env arg_type) :: args)

  | _ ->
    let init_code = translate_code contracts env exp in
    (List.rev args, init_code)

let translate_record ty_name labels env =
  let rtys = List.mapi
      (fun i pld ->
         let label = pld.pld_name.txt in
         if StringMap.mem label env.labels then
           error_loc pld.pld_loc "label already defined";

         let ty = translate_type env pld.pld_type in
         env.labels <- StringMap.add label (ty_name, i, ty) env.labels;
         label, ty) labels
  in
  let ty = Trecord (ty_name, rtys) in
  env.types <- StringMap.add ty_name ty env.types

let translate_variant ty_name constrs env =
  let constrs = List.map
      (fun pcd ->
         let constr = pcd.pcd_name.txt in
         if StringMap.mem constr env.constrs then
           error_loc pcd.pcd_loc "constructor already defined";

         let ty = match pcd.pcd_args with
           | Pcstr_tuple [ ty ] -> translate_type env ty
           | Pcstr_tuple [] -> Tunit
           | Pcstr_tuple tys -> Ttuple (List.map (translate_type env) tys)
           | Pcstr_record _ -> error_loc pcd.pcd_loc "syntax error"
         in
         env.constrs <- StringMap.add constr (ty_name, ty) env.constrs;
         constr, ty) constrs
  in
  let ty = Tsum (ty_name, constrs) in
  env.types <- StringMap.add ty_name ty env.types

let check_version = function
  | { pexp_desc = Pexp_constant (Pconst_float (s, None)); pexp_loc } ->
    let req_version = float_of_string s in
    if req_version < minimal_version then
      Printf.kprintf (error_version pexp_loc)
        "(requires %F while compiler has minimal %F )"
        req_version minimal_version;
    if req_version > maximal_version then
      Printf.kprintf (error_loc pexp_loc)
        "(requires %F while compiler has maximal %F )"
        req_version maximal_version;
  | { pexp_loc } -> error_loc pexp_loc "version must be a floating point number"

let filter_contracts acc =
  List.fold_left (fun acc -> function
      | Syn_contract c -> StringMap.add c.contract_name c acc
      | _ -> acc) StringMap.empty acc

let rec translate_structure env acc ast =
  match ast with
  | { pstr_desc =
        Pstr_extension
            (({ Asttypes.txt = "version" },
             PStr [{ pstr_desc = Pstr_eval (exp,[])}]),[])
    } :: ast ->
    check_version exp;
    translate_structure env acc ast

  | { pstr_desc =
        Pstr_extension
          (({ txt = "init" },
            PStr
              [{ pstr_desc =
                   Pstr_value (
                     Nonrecursive,
                     [ {
                       pvb_pat = { ppat_desc = Ppat_var { txt = "storage" } };
                       pvb_expr = sto_exp;
                     }
                     ]) } ]
           ), []);
      pstr_loc } :: ast
    ->
    if List.exists (function Syn_init _ -> true | _ -> false) acc then
      error_loc pstr_loc "Initial storage already defined";
    let init =
      Syn_init (translate_initial_storage env (filter_contracts acc) sto_exp [])
    in
    translate_structure env (init :: acc) ast

  | { pstr_desc =
        Pstr_extension
          (({ txt = "entry" },
           PStr
             [{ pstr_desc =
                  Pstr_value (
                      Nonrecursive,
                      [ {
                          pvb_pat = { ppat_desc = Ppat_var { txt = name } };
                          pvb_expr = head_exp;
                        }
             ]) } ]
           ), []) } :: ast (* _ast TODO *)
    ->
    let entry =
      Syn_entry (translate_entry name env
                   (filter_contracts acc) head_exp None None) in
    translate_structure env (entry :: acc) ast

  | { pstr_desc = (
      Pstr_value (
        Nonrecursive,
        [ {
          pvb_pat = { ppat_desc = Ppat_var { txt = var_name } };
          pvb_expr = var_exp;
          pvb_attributes = attrs;
        }
        ])); pstr_loc = f_loc } :: ast ->
    let exp = translate_code (filter_contracts acc) env var_exp in
    let inline = match attrs with
      | [ { txt = "inline"} , PStr [] ] -> true
      | _ -> false in
    if List.exists (function
        | Syn_value (n, _, _) -> n = var_name
        | _ -> false) acc
    then
      error_loc f_loc
        (Printf.sprintf "Top-level value %s already defined" var_name);
    let value = Syn_value (var_name, inline, exp) in
    translate_structure env (value :: acc) ast

  | { pstr_desc = Pstr_type (Recursive,
                             [
                               { ptype_name = { txt = ty_name };
                                 ptype_params = [];
                                 ptype_cstrs = [];
                                 ptype_private = Public;
                                 ptype_manifest = None;
                                 ptype_attributes = [];
                                 ptype_loc;
                                 ptype_kind; (* Ptype_record labels;*)
                               }
    ]) } :: ast ->
     if StringMap.mem ty_name env.types then
       error_loc ptype_loc "type already defined";
     begin match ptype_kind with
     | Ptype_record labels ->
        if List.length labels < 2 then begin
            error_loc ptype_loc "record must have at least two fields"
          end;
        translate_record ty_name labels env;
     | Ptype_abstract
       | Ptype_open -> error_loc ptype_loc "bad type definition"
     | Ptype_variant constrs ->
        if List.length constrs < 2 then begin
            error_loc ptype_loc "variant type must have at least two constructors"
          end;
        translate_variant ty_name constrs env;
     end;
     translate_structure env acc ast

  (* type alias *)
  | { pstr_desc = Pstr_type (Recursive,
                             [
                               { ptype_name = { txt = ty_name };
                                 ptype_params = [];
                                 ptype_cstrs = [];
                                 ptype_private = Public;
                                 ptype_manifest = Some ct;
                                 ptype_attributes = [];
                                 ptype_loc;
                                 ptype_kind;
                               }
                             ]) } :: ast ->
    let ty = translate_type env ct in
    env.types <- StringMap.add ty_name ty env.types;
    translate_structure env acc ast

  (* contracts *)
  | { pstr_desc = Pstr_module {
      pmb_name = { txt = contract_name };
      pmb_expr = {
        pmod_desc = Pmod_structure contract_struct;
      }
    }
    } :: ast ->
    let env = { env with filename = env.filename } in (* copy *)
    let sub_acc =
      List.filter (function Syn_contract _ -> true | _ -> false) acc in
    let contract, _ = translate_structure env sub_acc contract_struct in
    let contract = Syn_contract { contract with contract_name } in
    translate_structure env (contract :: acc) ast

  | { pstr_desc = Pstr_module {
      pmb_name = { txt = contract_name };
      pmb_expr = {
        pmod_desc = Pmod_ident { txt = Lident c_name; loc };
      }
    }
    } :: ast ->
    begin try
        let contract = StringMap.find c_name (filter_contracts acc) in
        let contract = Syn_contract { contract with contract_name } in
        translate_structure env (contract :: acc) ast
      with Not_found ->
        unbound_contract loc c_name
    end

  | [] -> pack_contract env (List.rev acc)

  | { pstr_loc = loc } as ast :: _ ->
    Format.eprintf "\n%a\b@."
      (Printast.structure 0) [ast];
    error_loc loc "at toplevel"


and pack_contract env toplevels =
  let storage =
    try StringMap.find "storage" env.types
    with Not_found ->
      LiquidLoc.raise_error
        ~loc:(LiquidLoc.loc_in_file env.filename)
        "type storage is required but not provided"
  in
  let contract_name = String.capitalize_ascii env.filename in
  if not (List.exists (function Syn_entry _ -> true | _ -> false) toplevels)
  then Location.raise_errorf "No entry point found in file %S%!" env.filename;
  let rec partition (contracts, values, entries, init) = function
    | Syn_value (v,i,e) :: r ->
      partition (contracts, (v,i,e) :: values, entries, init) r
    | Syn_contract c :: r -> partition (c :: contracts, values, entries, init) r
    | Syn_entry e :: r -> partition (contracts, values, e :: entries, init) r
    | Syn_init i :: r -> partition (contracts, values, entries, Some i) r
    | [] -> (List.rev contracts, values, List.rev entries, init) in
  let contracts, values, entries, init =
    partition ([], [], [], None) toplevels in
  { contract_name; storage; values; entries }, init


let predefined_constructors =
  List.fold_left (fun acc (constr, info) ->
      StringMap.add constr info acc) StringMap.empty
                 (* These constructors are never looked-up in this
                 map, hence we can provide wrong info. However, they
                 need to be there to prevent the user from overriding
                 them. *)
                 [
                   "Some", ("'a option", Tunit);
                   "None", ("'a option", Tunit);
                   "::", ("'a list", Tunit);
                   "[]", ("'a list", Tunit);
                   "Left", ("('a, 'b) variant", Tunit);
                   "Right", ("('a, 'b) variant", Tunit);
                 ]

let predefined_types =
  List.fold_left (fun acc (constr, info) ->
      StringMap.add constr info acc) StringMap.empty
                 (* Enter predefined types with dummy-info to prevent
 the user from overriding them *)
                 [
                   "int", Tunit;
                   "unit", Tunit;
                   "bool", Tunit;
                   "nat", Tunit;
                   "tez", Tunit;
                   "string", Tunit;
                   "bytes", Tunit;
                   "timestamp", Tunit;
                   "key", Tunit;
                   "key_hash", Tunit;
                   "signature", Tunit;
                   "operation", Tunit;
                   "address", Tunit;
                   "option", Tunit;
                   "list", Tunit;
                   "map", Tunit;
                   "set", Tunit;
                   "big_map", Tunit;
                   "variant", Tunit;
                   "contract", Tunit;
                 ]

let initial_env filename =
  {
    types = predefined_types;
    contract_types = StringMap.empty;
    labels = StringMap.empty;
    constrs = predefined_constructors;
    filename;
  }

let translate_exn exn =
  match exn with
  | Location.Error err ->
     let loc = loc_of_loc Location.(err.loc) in
     LiquidLoc.raise_error ~loc "%s"  Location.(err.msg)

  (* Syntax errors *)
  | Syntaxerr.Error err  ->
     let (loc, msg) =
       match err with
       | Syntaxerr.Unclosed(opening_loc, opening, closing_loc, closing) ->
          closing_loc,
          Printf.sprintf "Syntax error: '%s' expected" closing
       | Syntaxerr.Expecting (loc, nonterm) ->
          loc,
          Printf.sprintf "Syntax error: %s expected." nonterm
       | Syntaxerr.Not_expecting (loc, nonterm) ->
          loc,
          Printf.sprintf "Syntax error: %s not expected." nonterm
       | Syntaxerr.Applicative_path loc ->
          loc,
          "Syntax error: applicative paths of the form F(X).t \
           are not supported when the option -no-app-func is set."
       | Syntaxerr.Variable_in_scope (loc, var) ->
          loc,
          Printf.sprintf "In this scoped type, variable '%s \
                             is reserved for the local type %s."
                            var var
       | Syntaxerr.Other loc ->
          loc, "Syntax error"
       | Syntaxerr.Ill_formed_ast (loc, s) ->
          loc,
          Printf.sprintf "broken invariant in parsetree: %s" s
       | Syntaxerr.Invalid_package_type (loc, s) ->
          loc,
          Printf.sprintf "invalid package type: %s" s
     in
     let loc = loc_of_loc loc in
     LiquidLoc.raise_error ~loc "%s" msg

  | LiquidOCamlLexer.Error (error, oloc) ->
     let loc = loc_of_loc oloc in
     LiquidLoc.raise_error ~loc "%s"
                           (Location.error_of_printer
                              oloc
                              LiquidOCamlLexer.report_error error).Location.msg
     LiquidOCamlLexer.report_error ppf error
  | _ -> raise exn


let translate ~filename ast =
  let env = initial_env filename in
  try
    let contract, init = translate_structure env [] ast in
    contract, init, env
  with exn -> translate_exn exn

let ocaml_of_file parser file =
  let ic = open_in file in
  try
    Location.input_name := file;
    let lexbuf = Lexing.from_channel ic in
    Location.init lexbuf file;
    let ast = parser lexbuf in
    close_in ic;
    (* Format.eprintf "\n%a\b@."
     *   Printast.implementation ast; *)
    ast
  with exn ->
    close_in ic;
    translate_exn exn

let read_file filename =
  try
    ocaml_of_file LiquidOCamlParse.implementation filename
  with exn -> translate_exn exn

let translate_expression env expression =
  try
    translate_code StringMap.empty env expression
  with exn -> translate_exn exn

let ocaml_of_string ?(filename = "buffer") parser content =
  try
    Location.input_name := filename;
    let lexbuf = Lexing.from_string content in
    Location.init lexbuf filename;
    parser lexbuf
  with exn ->
    translate_exn exn

let structure_of_string ?filename impl =
  ocaml_of_string ?filename LiquidOCamlParse.implementation impl
let expression_of_string ?filename s =
  ocaml_of_string ?filename LiquidOCamlParse.expression s

let translate_type env ty = translate_type env ty

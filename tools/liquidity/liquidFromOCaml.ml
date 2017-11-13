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
    "exists", EXISTS;
    (* "external", EXTERNAL; *)
    "false", FALSE;
    "for", FOR;
    "forall", FORALL;
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
    (* "method", METHOD; *)
    (* "module", MODULE; *)
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
    (* "sig", SIG; *)
    (* "struct", STRUCT; *)
    "then", THEN;
    "to", TO;
    "true", TRUE;
    (* "try", TRY; *)
    "type", TYPE;
    (* "val", VAL; *)
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
    "asr", INFIXOP4("asr")
]



(* The minimal version of liquidity files that are accepted by this compiler *)
let minimal_version = 0.13

(* The maximal version of liquidity files that are accepted by this compiler *)
let maximal_version = 0.13


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
    "return", Tunit;
    "storage", Tunit;
    "parameter", Tunit;
  ]

let error_loc loc msg =
  let loc = loc_of_loc loc in
  LiquidLoc.raise_error ~loc "Unexpected syntax %s%!" msg

let error_version loc msg =
  let loc = loc_of_loc loc in
  LiquidLoc.raise_error ~loc "version mismatch %s%!" msg

let unbound_type loc ty =
  let loc = loc_of_loc loc in
  LiquidLoc.raise_error ~loc "Unbound type %S%!" ty

let error_arg loc arg =
  let loc = loc_of_loc loc in
  LiquidLoc.raise_error ~loc "Unexpected argument %S%!" arg

let todo_loc loc msg =
  let loc = loc_of_loc loc in
  LiquidLoc.raise_error ~loc "Syntax %S not yet implemented%!" msg

let rec translate_type env typ =
  match typ with
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "unit" }, []) } -> Tunit
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "bool" }, []) } -> Tbool
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "int" }, []) } -> Tint
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "nat" }, []) } -> Tnat
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "tez" }, []) } -> Ttez
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "string" }, []) } -> Tstring
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "timestamp" }, []) } -> Ttimestamp
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "key" }, []) } -> Tkey
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "key_hash" }, []) } -> Tkey_hash
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "signature" }, []) } -> Tsignature



  | { ptyp_desc = Ptyp_constr ({ txt = Lident "option" }, [param_type]) } ->
     Toption (translate_type env param_type)
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "list" }, [param_type]) } ->
     Tlist (translate_type env param_type)
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "set" }, [param_type]) } ->
     Tset (translate_type env param_type)


  | { ptyp_desc = Ptyp_constr ({ txt = Lident "contract" },
                               [parameter_type; return_type]) } ->
     Tcontract (translate_type env parameter_type, translate_type env return_type)
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "variant" },
                               [parameter_type; return_type]) } ->
     Tor (translate_type env parameter_type, translate_type env return_type)
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "map" },
                               [parameter_type; return_type]) } ->
     Tmap (translate_type env parameter_type, translate_type env return_type)

  | { ptyp_desc = Ptyp_arrow (_, parameter_type, return_type) } ->
     Tlambda (translate_type env parameter_type, translate_type env return_type)

  | { ptyp_desc = Ptyp_tuple types } ->
     Ttuple (List.map (translate_type env) types)

  | { ptyp_desc = Ptyp_constr ({ txt = Lident ty_name }, []) } ->
     begin
       try
         Ttype (ty_name, StringMap.find ty_name env.types)
       with Not_found ->
         unbound_type typ.ptyp_loc ty_name
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

  (* Key *)
  | { pexp_desc = Pexp_constant (Pconst_integer (s, Some '\234')) } ->
     CKey s, Some Tkey

  (* Signature *)
  | { pexp_desc = Pexp_constant (Pconst_integer (s, Some '\235')) } ->
     CSignature s, Some Tkey

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

  | { pexp_desc = Pexp_construct (
                      { txt = Lident "Map" }, None) } ->
     CMap [], None

  | { pexp_desc = Pexp_construct (
                      { txt = Lident "Map" }, Some pair_list) } ->
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
       | [] -> None
       | (Some ty1, Some ty2) :: tail ->

          List.iter (function
                       (Some ty1', Some ty2') when ty1' = ty1 && ty2' = ty2
                                                   -> ()
                     | _ -> error_loc exp.pexp_loc
                              "inconsistent map types"
                    )
                    tail;
          Some (Tmap (ty1, ty2))
       | _ ->
          error_loc exp.pexp_loc
            "underspecified map types"
     in
     CMap csts, tys


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
       | [] -> None
       | Some ty1 :: tail ->
          List.iter (function
                       Some ty1' when ty1' = ty1 -> ()
                     | _ -> error_loc exp.pexp_loc
                              "inconsistent set types"
                              )
                    tail;
          Some (Tset ty1)
       | _ ->
          error_loc exp.pexp_loc
            "underspecified set types"
     in
     CSet csts, tys


  | { pexp_desc = Pexp_construct (
                      { txt = Lident "Some" }, Some arg) } ->
     let arg, ty = translate_const env arg in
     begin
       match ty with
       | None -> error_loc exp.pexp_loc "No type for Some arg"
       | Some ty ->
          CSome arg, Some (Toption ty)
     end

  | { pexp_desc = Pexp_constraint (cst, ty) } ->
     let cst, tyo = translate_const env cst in
     let ty = translate_type env ty in
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
          mk (Const (Tnat, CNat (LiquidPrinter.integer_of_int i)))
        ]))
    ) indexes a

let deconstruct_pat env pat e =
  let vars_infos, ty = vars_info_pat env pat in
  match vars_infos with
  | [] -> assert false
  | [v, _loc, []] -> v, ty, e
  | _ ->
    let var_name =
      String.concat "_" (List.rev_map (fun (v,_,_) -> v) vars_infos) in
    let e =
      List.fold_left (fun e (v, loc, indexes) ->
          let access = access_of_deconstruct var_name loc indexes in
          mk (Let (v, loc, access, e))
        ) e vars_infos
    in
    var_name, ty, e

let rec translate_code env exp =
  let desc =
    match exp with
    | { pexp_desc = Pexp_ident ( { txt = Lident var } ) } ->
       Var (var, loc_of_loc exp.pexp_loc, [])
    | { pexp_desc = Pexp_ident ( { txt = Ldot(Lident m, var) } ) } ->
       Var (m ^ "." ^ var, loc_of_loc exp.pexp_loc, [])

    | { pexp_desc = Pexp_field (exp, { txt = Lident label }) } ->
       begin
         let e = translate_code env exp
         in
         match e.desc with
         | Var (name, loc, labels) ->
            Var (name, loc, labels @ [label])
         | _ -> error_loc exp.pexp_loc "variable expected"
       end
    | { pexp_desc = Pexp_setfield (exp, { txt = Lident label }, arg) } ->
       begin
         let e = translate_code env exp in
         let arg = translate_code env arg
         in
         match e.desc with
         | Var (name, loc, labels) ->
            SetVar (name, loc, labels @ [label], arg)
         | _ -> error_loc exp.pexp_loc "variable expected"
       end


    | { pexp_desc = Pexp_ifthenelse (e1, e2, None) } ->
       If (translate_code env e1,
           translate_code env e2,
           mk (Const (Tunit, CUnit)))
    | { pexp_desc = Pexp_ifthenelse (e1, e2, Some e3) } ->
       If (translate_code env e1, translate_code env e2, translate_code env e3)
    | { pexp_desc =
          Pexp_let (
              Nonrecursive,
              [
                {
                  pvb_pat =
                    {
                      ppat_desc =
                        Ppat_tuple [
                          { ppat_desc = (Ppat_any | Ppat_var _) as res};
                          { ppat_desc = (Ppat_any | Ppat_var _) as sto};
                        ]
                    };
                  pvb_expr =
                    { pexp_desc =
                        Pexp_apply (
                            { pexp_desc = Pexp_ident
                                            { txt =
                                                Ldot(Lident "Contract",
                                                     "call") } },
                            [
                              Nolabel, contract_exp;
                              Nolabel, tez_exp;
                              Nolabel, storage_exp;
                              Nolabel, arg_exp;
                    ]) }
                }
              ], body) } ->
       let result = match res with
         | Ppat_any -> "_"
         | Ppat_var { txt = result } -> result
         | _ -> assert false
       in
       let storage_name = match sto with
         | Ppat_any -> "_"
         | Ppat_var { txt = storage_name } -> storage_name
         | _ -> assert false
       in
       LetTransfer (storage_name, result,
                    loc_of_loc exp.pexp_loc,
                    translate_code env contract_exp,
                    translate_code env tez_exp,
                    translate_code env storage_exp,
                    translate_code env arg_exp,
                    translate_code env body)

    | { pexp_desc = Pexp_let (Nonrecursive, [ {
        pvb_pat = pat;
        pvb_expr = var_exp;
      } ], body) } ->
      let exp, body = translate_code env var_exp, translate_code env body in
      let var_name, _, body = deconstruct_pat env pat body in
      Let (var_name, loc_of_loc pat.ppat_loc, exp, body)

    | { pexp_desc = Pexp_sequence (exp1, exp2) } ->
       Seq (translate_code env exp1, translate_code env exp2)

    | { pexp_desc =
          Pexp_apply (
              { pexp_desc = Pexp_ident ( { txt = Ldot(Lident "Loop",
                                                      "loop");
                                           loc } ) },
              [
                Nolabel, { pexp_desc =
                             Pexp_fun (Nolabel,None,
                                       { ppat_desc = Ppat_var { txt = name } },
                                       body
                         ) };
                Nolabel, arg
      ]) } ->
       let body = translate_code env body in
       let arg = translate_code env arg in
       Loop (name, loc_of_loc exp.pexp_loc, body, arg)

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident ({ txt = Lident "/\\"}) },
            [Nolabel, e1; Nolabel, e2]); pexp_loc = loc } ->
      let e1, e2 = translate_code env e1, translate_code env e2 in
      And(loc_of_loc loc, e1, e2)

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident ({ txt = Lident "\\/"}) },
            [Nolabel, e1; Nolabel, e2]); pexp_loc = loc } ->
      let e1, e2 = translate_code env e1, translate_code env e2 in
      Or(loc_of_loc loc, e1, e2)

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident ({ txt = Lident "=>"}) },
            [Nolabel, e1; Nolabel, e2]); pexp_loc = loc } ->
      let e1, e2 = translate_code env e1, translate_code env e2 in
      Implies(loc_of_loc loc, e1, e2)

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident ({ txt = Lident "<=>"}) },
            [Nolabel, e1; Nolabel, e2]); pexp_loc = loc } ->
      let e1, e2 = translate_code env e1, translate_code env e2 in
      Equiv(loc_of_loc loc, e1, e2)

    | { pexp_desc =
          Pexp_apply (
            exp,
            args); pexp_loc = loc } ->
      let exp = translate_code env exp in
      (* Format.eprintf "aply>> %s@." (LiquidPrinter.Liquid.string_of_code exp); *)
      Apply(Prim_unknown, loc_of_loc loc, exp :: List.map (
          function (Nolabel, exp) ->
            translate_code env exp
                 | (_, { pexp_loc }) ->
                   error_loc pexp_loc "in arg"
        ) args)

    | { pexp_desc = Pexp_extension (
        { txt = ("forall"| "exists") as q },
        PPat ({ppat_desc = Ppat_tuple vars_pat}, Some exp)
      ); pexp_loc = loc } ->
      let vars = List.map (function
          | { ppat_desc = Ppat_constraint (
              { ppat_desc = Ppat_var { txt = v } }, ty) } ->
            v, translate_type env ty
          | { ppat_loc } ->
            error_loc ppat_loc
              "quantified variable must be of the form \"(v : ty)\""
        ) vars_pat
      in
      let e = translate_code env exp in
      begin match q with
        | "forall" -> Forall (loc_of_loc loc, vars, e)
        | "exists" -> Exists (loc_of_loc loc, vars, e)
        | _ -> assert false
      end

    | { pexp_desc = Pexp_extension (
        { txt = "nat" },
        PStr [{ pstr_desc = Pstr_eval (
            { pexp_desc = Pexp_match (e, cases); pexp_loc },
            [])
          }]
      )} ->
      let e = translate_code env e in
      let cases = List.map (translate_case env) cases in
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

    | { pexp_desc = Pexp_match (e, cases); pexp_loc } ->
       let e = translate_code env e in
       let cases = List.map (translate_case env) cases in
       begin
         match cases with
         | [ CConstr ("None", []), ifnone; CConstr ("Some", [arg]), ifsome ]
         | [ CConstr ("Some", [arg]), ifsome; CConstr ("None", []), ifnone ]
         | [ CConstr ("Some", [arg]), ifsome; CAny, ifnone ] ->
           MatchOption(e, loc_of_loc pexp_loc, ifnone, arg, ifsome)
         | [ CConstr ("None", []), ifnone; CAny, ifsome ] ->
           MatchOption(e, loc_of_loc pexp_loc, ifnone, "_", ifsome)

         | [ CConstr ("[]", []), ifnil; CConstr ("::", [head; tail]), ifcons ]
         | [ CConstr ("::", [head; tail]), ifcons; CConstr ("[]", []), ifnil ]
         | [ CConstr ("::", [head; tail]), ifcons; CAny, ifnil ] ->
           MatchList(e, loc_of_loc pexp_loc, head, tail, ifcons, ifnil)
         | [ CConstr ("[]", []), ifnil; CAny, ifcons ] ->
           MatchList(e, loc_of_loc pexp_loc, "_head", "_tail", ifcons, ifnil)

         | args ->
           MatchVariant(e, loc_of_loc pexp_loc, args)
       end

    | { pexp_desc = Pexp_fun (Nolabel, None, pat, body_exp) } ->
       let body_exp = translate_code env body_exp in
       let arg_name, arg_type, body_exp = deconstruct_pat env pat body_exp in
       Lambda (arg_name, arg_type, loc_of_loc exp.pexp_loc,
               body_exp,
               Tunit) (* not yet inferred *)

    | { pexp_desc = Pexp_record (lab_x_exp_list, None) } ->
       let lab_x_exp_list =
         List.map (function
                     ({ txt = Lident label; loc }, exp) ->
                     label, translate_code env exp
                   | ( { loc }, _) ->
                      error_loc loc "label expected"
                  ) lab_x_exp_list in
       Record (loc_of_loc exp.pexp_loc, lab_x_exp_list)

    | exp ->
       match translate_const env exp with
       | _, None -> error_loc exp.pexp_loc "constant needs a type"
       | cst, Some ty -> Const (ty, cst)
       | exception NotAConstant ->
          match exp with
          | { pexp_desc = Pexp_construct (
                              { txt = Lident "Some" }, Some args) } ->
             Apply(Prim_Some, loc_of_loc exp.pexp_loc,
                   [translate_code env args])

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
             Apply(Prim_Cons, loc_of_loc exp.pexp_loc,
                   [translate_code env a;
                    mk ( Const (Tunit, CUnit))
                  ])

          | { pexp_desc = Pexp_construct (
                              { txt = Lident "::" },
                              Some { pexp_desc = Pexp_tuple [a;b]}) } ->
             Apply(Prim_Cons, loc_of_loc exp.pexp_loc,
                   [translate_code env a;
                    translate_code env b])




          | { pexp_desc = Pexp_construct (
                              { txt = Lident lid }, args) } ->
             begin
               try
                 let (_ty_name, _ty) = StringMap.find lid env.constrs in
                 Constructor( loc_of_loc exp.pexp_loc, Constr lid,
                              match args with
                              | None -> mk (Const (Tunit, CUnit))
                              | Some arg -> translate_code env arg )
               with Not_found ->
                 error_loc exp.pexp_loc "unknown constructor"
             end

          | { pexp_desc =
                Pexp_constraint (
                    { pexp_desc =
                        Pexp_construct (
                            { txt = Lident "Left" }, args) },
                    { ptyp_desc = Ptyp_constr (
                                      { txt = Lident "variant" },
                                      [ left_ty; right_ty ]
            )}) } ->
             Constructor( loc_of_loc exp.pexp_loc,
                          Left (translate_type env right_ty),
                          match args with
                          | None -> mk (Const (Tunit, CUnit))
                          | Some arg -> translate_code env arg )

          | { pexp_desc =
                Pexp_constraint (
                    { pexp_desc =
                        Pexp_construct (
                            { txt = Lident "Right" }, args) },
                    { ptyp_desc = Ptyp_constr (
                                      { txt = Lident "variant" },
                                      [ left_ty; right_ty ]
            )}) } ->
             Constructor( loc_of_loc exp.pexp_loc,
                          Right (translate_type env left_ty),
                          match args with
                          | None -> mk (Const (Tunit, CUnit))
                          | Some arg -> translate_code env arg )


          | { pexp_desc =
                Pexp_constraint (
                    { pexp_desc =
                        Pexp_construct (
                            { txt = Lident "Source" }, None) },
                    { ptyp_desc = Ptyp_constr (
                                      { txt = Lident "contract" },
                                      [ from_ty; to_ty ]
            )}) } ->
             Constructor( loc_of_loc exp.pexp_loc,
                          Source (translate_type env from_ty,
                                  translate_type env to_ty
                                 ), mk (Const (Tunit, CUnit)) )

          (* TODO *)
          | { pexp_desc = Pexp_tuple exps } ->
             let exps = List.map (translate_code env) exps in
             begin
               try
                 let tys, csts = List.split (
                                     List.map (
                                         function
                                         | { desc = Const (ty, cst) } -> (ty, cst)
                                         | _ -> raise Exit
                                       ) exps)
                 in
                 Const (Ttuple tys, CTuple csts)
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

and translate_case env case =
  match case.pc_guard with
  | Some e  ->
     error_loc e.pexp_loc "when forbidden"
  | None ->
     let e = case.pc_rhs in
     let e = translate_code env e in
     match case.pc_lhs with
     | { ppat_desc = Ppat_any } ->
        (CAny, e)
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

let translate_specs env l =
  List.map (function
      | { txt = "requires" }, PStr [{ pstr_desc = Pstr_eval (exp, [])}] ->
        Requires (translate_code env exp)
      | { txt = "ensures" }, PStr [{ pstr_desc = Pstr_eval (exp, [])}] ->
        Ensures (translate_code env exp)
      | { txt = "fails" }, PStr [{ pstr_desc = Pstr_eval (exp, [])}] ->
        Fails (translate_code env exp)
      | { txt; loc }, _ ->
        error_loc loc ("bad specification: " ^ txt)
    ) l

let rec translate_head env ext_funs head_exp args specs =
  match head_exp with
  | { pexp_desc =
        Pexp_fun (
            Nolabel, None,
            { ppat_desc =
                Ppat_constraint(
                    { ppat_desc =
                        Ppat_var { txt =
                                     (   "parameter"
                                       | "storage"
                                     (*  | "return" *)
                                     ) as arg} },
                    arg_type)
            },
            head_exp) } ->
     translate_head env ext_funs head_exp
       ((arg, translate_type env arg_type) :: args) specs

  | { pexp_desc = Pexp_constraint (head_exp, return_type); pexp_loc } ->
    let return = match translate_type env return_type with
      | Ttuple [ ret_ty; sto_ty ] ->
        let storage = List.assoc "storage" args in
        if not (eq_types sto_ty storage) then
          error_loc pexp_loc
            "Second component of return type must be identical to storage type";
        ret_ty
      | _ ->
        error_loc pexp_loc
          "return type must be a product of some type and the storage type"
    in
    let nspecs = translate_specs env return_type.ptyp_attributes in
    translate_head env ext_funs head_exp (("return", return) :: args)
      (specs @ nspecs)

  | { pexp_desc =
        Pexp_fun (
            Nolabel, None,
            { ppat_desc =
                Ppat_extension ({ txt = "return"}, PTyp arg_type)
            },
            head_exp) } ->
     translate_head env ext_funs head_exp
       (("return", translate_type env arg_type) :: args) specs

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

  | exp ->
     let code = translate_code env (inline_funs exp ext_funs) in
     {
       code;
       spec = specs;
       parameter = List.assoc "parameter" args;
       storage = List.assoc "storage" args;
       return = List.assoc "return" args;
     }

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
  let ty = Trecord rtys in
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
  let ty = Tsum constrs in
  env.types <- StringMap.add ty_name ty env.types

let check_version = function
  | { pexp_desc = Pexp_constant (Pconst_float (s, None)); pexp_loc } ->
    let req_version = float_of_string s in
    if req_version < minimal_version then
      Printf.kprintf (error_version pexp_loc)
                     "(requires %.2f while compiler has minimal %.2f )" req_version minimal_version;
    if req_version > maximal_version then
      Printf.kprintf (error_loc pexp_loc)
                     "(requires %.2f while compiler has maximal %.2f )" req_version maximal_version;
  | { pexp_loc } -> error_loc pexp_loc "version must be a floating point number"

let rec translate_structure funs env ast =
  match ast with
  | { pstr_desc =
        Pstr_extension
            (({ Asttypes.txt = "version" },
             PStr [{ pstr_desc = Pstr_eval (exp,[])}]),[])
    } :: ast ->
    check_version exp;
    translate_structure funs env ast

  | { pstr_desc =
        Pstr_extension
          (({ txt = "entry" },
           PStr
             [{ pstr_desc =
                  Pstr_value (
                      Nonrecursive,
                      [ {
                          pvb_pat = { ppat_desc = Ppat_var { txt = "main" } };
                          pvb_expr = head_exp;
                        }
             ]) } ]
           ), []) } :: _ast  (* TODO *)
    ->
     translate_head env funs head_exp default_args []

  | { pstr_desc =
         Pstr_value (
             Nonrecursive,
             [ {
                 pvb_pat = { ppat_desc = Ppat_var _ };
                 pvb_expr = { pexp_desc = Pexp_fun _ };
               } as f_pvb
    ]); pstr_loc = f_loc } :: ast ->
    translate_structure ((f_pvb, f_loc) :: funs) env ast

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
     translate_structure funs env ast

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
    translate_structure funs env ast

  | [] ->
    Location.raise_errorf "No entry point found in file %S%!" env.filename

  | { pstr_loc = loc } :: _ -> error_loc loc "at toplevel"

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
                   "Source", ("('a, 'b) contract", Tunit);
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
                   "key", Tunit;
                   "signature", Tunit;
                   "option", Tunit;
                   "list", Tunit;
                   "map", Tunit;
                   "set", Tunit;
                   "variant", Tunit;
                   "contract", Tunit;
                 ]

let initial_env filename =
  {
    types = predefined_types;
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
    translate_structure [] env ast, env
  with exn -> translate_exn exn

let ocaml_of_file parser file =
  let ic = open_in file in
  try
    Location.input_name := file;
    let lexbuf = Lexing.from_channel ic in
    Location.init lexbuf file;
    let ast = parser lexbuf in
    close_in ic;
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
    translate_code env expression
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

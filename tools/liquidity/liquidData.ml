(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* TODO: We could use a simplify pass to propagate the no-effect
   defining expression of once-used variables to their uniq use
   site. It could dramatically decrease the size of the stack.  *)


open LiquidTypes


let rec default_const = function
  | Tunit -> CUnit
  | Tbool -> CBool false
  | Tint -> CInt (LiquidPrinter.integer_of_int 0)
  | Tnat -> CNat (LiquidPrinter.integer_of_int 0)
  | Ttez -> CTez (LiquidPrinter.tez_of_liq "0")
  | Tstring -> CString ""
  | Tbytes -> CBytes "0x"
  | Ttimestamp -> CTimestamp "1970-01-01T00:00:00Z"
  | Tkey -> CKey "edpkuit3FiCUhd6pmqf9ztUTdUs1isMTbF9RBGfwKk1ZrdTmeP9ypN"
  | Tkey_hash -> CKey_hash "tz1YLtLqD1fWHthSVHPD116oYvsd4PTAHUoc"
  | Tsignature ->
    CSignature
      "edsigthTzJ8X7MPmNeEwybRAvdxS1pupqcM5Mk4uCuyZAe7uEk\
       68YpuGDeViW8wSXMrCi5CwoNgqs8V2w8ayB5dMJzrYCHhD8C7"
  | Tcontract _ -> CContract "KT1GE2AZhazRxGsAjRVkQccHcB2pvANXQWd7"
  | Taddress -> CAddress "KT1GE2AZhazRxGsAjRVkQccHcB2pvANXQWd7"
  | Ttuple l ->
    CTuple (List.map default_const l)
  | Toption ty -> CSome (default_const ty)
  | Tlist ty -> CList [default_const ty]
  | Tset ty -> CSet [default_const ty]
  | Tmap (ty1, ty2) ->
    CMap [default_const ty1, default_const ty2]
  | Tbigmap  (ty1, ty2) ->
    CBigMap [default_const ty1, default_const ty2]
  | Tor (ty, _) -> CLeft (default_const ty)
  | Trecord (_, fields) ->
    CRecord (
      List.map (fun (name, ty) ->
          name, default_const ty) fields
    )
  | Tsum (_, (c, ty) :: _) ->
    CConstr (c, default_const ty)

  | Tsum (_, [])
  | Tfail
  | Tclosure _
  | Tlambda _
  | Toperation -> raise Not_found

let rec default_empty_const = function
  | Tunit -> CUnit
  | Tbool -> CBool false
  | Tint -> CInt (LiquidPrinter.integer_of_int 0)
  | Tnat -> CNat (LiquidPrinter.integer_of_int 0)
  | Ttez -> CTez (LiquidPrinter.tez_of_liq "0")
  | Tstring -> CString ""
  | Tbytes -> CBytes "0x"
  | Ttimestamp -> CTimestamp "1970-01-01T00:00:00Z"
  | Tkey -> CKey "edpkuit3FiCUhd6pmqf9ztUTdUs1isMTbF9RBGfwKk1ZrdTmeP9ypN"
  | Tkey_hash -> CKey_hash "tz1YLtLqD1fWHthSVHPD116oYvsd4PTAHUoc"
  | Tsignature ->
    CSignature
      "edsigthTzJ8X7MPmNeEwybRAvdxS1pupqcM5Mk4uCuyZAe7uEk\
       68YpuGDeViW8wSXMrCi5CwoNgqs8V2w8ayB5dMJzrYCHhD8C7"
  | Tcontract _ -> CContract "KT1GE2AZhazRxGsAjRVkQccHcB2pvANXQWd7"
  | Taddress -> CAddress "KT1GE2AZhazRxGsAjRVkQccHcB2pvANXQWd7"
  | Ttuple l ->
    CTuple (List.map default_empty_const l)
  | Toption ty -> CNone
  | Tlist ty -> CList []
  | Tset ty -> CSet []
  | Tmap (ty1, ty2) -> CMap []
  | Tbigmap  (ty1, ty2) -> CBigMap []
  | Tor (ty, _) -> CLeft (default_empty_const ty)
  | Trecord (_, fields) ->
    CRecord (
      List.map (fun (name, ty) ->
          name, default_empty_const ty) fields
    )
  | Tsum (_, (c, ty) :: _) ->
    CConstr (c, default_empty_const ty)

  | Tsum (_, [])
  | Tfail
  | Tclosure _
  | Tlambda _
  | Toperation -> raise Not_found

let rec translate_const_exp (exp : encoded_exp) =
  let loc = exp.loc in
  match exp.desc with
  | Let _ ->
    LiquidLoc.raise_error ~loc "'let' forbidden in constant"
  | Const { const } -> const

  | Record fields ->
    CRecord (List.map (fun (f, e) -> (f, translate_const_exp e)) fields)

  | Constructor { constr = Constr c; arg } ->
    CConstr (c, translate_const_exp arg)
  | Constructor { constr = Left _; arg } -> CLeft (translate_const_exp arg)
  | Constructor { constr = Right _; arg } -> CRight (translate_const_exp arg)

  | Apply { prim = Prim_Left; args = [x] } -> CLeft (translate_const_exp x)
  | Apply { prim = Prim_Right; args = [x] } -> CRight (translate_const_exp  x)
  | Apply { prim = Prim_Some; args = [x] } -> CSome (translate_const_exp x)
  | Apply { prim = Prim_Cons; args } ->
    CList (List.map translate_const_exp args)
  | Apply { prim = Prim_tuple; args } ->
    CTuple (List.map translate_const_exp args)

  | TypeAnnot { e } -> translate_const_exp e

  | Apply _
  | Var _
  | SetField _
  | Project _
  | If _
  | Seq _
  | Transfer _
  | Call _
  | MatchOption _
  | MatchNat _
  | MatchList _
  | Loop _
  | LoopLeft _
  | Fold _
  | Map _
  | MapFold _
  | Lambda _
  | Closure _
  | MatchVariant _
  | Failwith _
  | CreateContract _
  | ContractAt _
  | Unpack _
    ->
    LiquidLoc.raise_error ~loc "non-constant expression"


let translate env contract_sig s ty =
  let ml_exp =
    LiquidFromOCaml.expression_of_string ~filename:env.filename s in
  (* hackish: add type annotation for constants *)
  let ml_ty = LiquidToOCaml.convert_type ~abbrev:false ty in
  let ml_exp = Ast_helper.Exp.constraint_
      ~loc:(Location.in_file env.filename) ml_exp ml_ty in
  let sy_exp = LiquidFromOCaml.translate_expression env ml_exp in
  let tenv = empty_typecheck_env ~warnings:true contract_sig env in
  let ty_exp = LiquidCheck.typecheck_code tenv ~expected_ty:ty sy_exp in
  let enc_exp = LiquidEncode.encode_code tenv ty_exp in
  translate_const_exp enc_exp


let string_of_const ?ty c =
  let e = LiquidToOCaml.convert_const c in
  let e = match ty with
    | None -> e
    | Some ty ->
      Ast_helper.Exp.constraint_ e (LiquidToOCaml.convert_type ty)
  in
  LiquidToOCaml.string_of_expression e

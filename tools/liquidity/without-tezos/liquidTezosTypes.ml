(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type env = unit
type expr = LiquidTypes.michelson_exp LiquidTypes.const
type contract = LiquidTypes.michelson_contract
type json
let empty_env _ = ()
let set_generalize_types _ _ = ()

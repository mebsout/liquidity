(****************************************************************************)
(*                               Liquidity                                  *)
(*                                                                          *)
(*                  Copyright (C) 2017-2020 OCamlPro SAS                    *)
(*                                                                          *)
(*                    Authors: Fabrice Le Fessant                           *)
(*                             Alain Mebsout                                *)
(*                             David Declerck                               *)
(*                                                                          *)
(*  This program is free software: you can redistribute it and/or modify    *)
(*  it under the terms of the GNU General Public License as published by    *)
(*  the Free Software Foundation, either version 3 of the License, or       *)
(*  (at your option) any later version.                                     *)
(*                                                                          *)
(*  This program is distributed in the hope that it will be useful,         *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of          *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *)
(*  GNU General Public License for more details.                            *)
(*                                                                          *)
(*  You should have received a copy of the GNU General Public License       *)
(*  along with this program.  If not, see <https://www.gnu.org/licenses/>.  *)
(****************************************************************************)

open Js_of_ocaml
open Liquidity_js_michelson

let failwith fmt =
  Format.kasprintf
    (fun s ->
       Js.raise_js_error (jsnew Js.error_constr (Js.string s)))
    fmt

let mk_option (from_js : 'b -> 'a) (to_js : 'a -> 'b) (r : 'a ref) = jsobject
method get = (to_js !r : 'b)
method set (v : 'b) =
  try r := from_js v; Js.undefined
  with e -> failwith "%s" (Printexc.to_string e)
end

let mk_opt_option from_js to_js (r : 'a option ref) =
  mk_option
    (fun x -> match Js.Opt.to_option x with
       | None -> None
       | Some x -> Some (from_js x))
    (function
      | None -> Js.Opt.option None
      | Some x -> Js.Opt.option (Some (to_js x))) r

let mk_bool_option r = mk_option Js.to_bool Js.bool r
let mk_not_bool_option r =
  mk_option (fun v -> not (Js.to_bool v)) (fun v -> Js.bool (not v)) r
let mk_string_option r = mk_option Js.to_string Js.string r
let mk_int_option (r : int ref) = mk_option (fun x -> x) (fun x -> x) r
let mk_string_opt_option r = mk_opt_option Js.to_string Js.string r
let mk_tez_option r =
  mk_option
    (fun x -> LiquidNumber.tez_of_liq (Js.to_string x##toString()))
    (fun x -> Js.string (LiquidNumber.liq_of_tez x)) r
let mk_tez_opt_option r =
  mk_opt_option
    (fun x -> LiquidNumber.tez_of_liq (Js.to_string x##toString()))
    (fun x -> Js.string (LiquidNumber.liq_of_tez x)) r
let mk_int_opt_option (r (* : int option ref *)) = mk_opt_option (fun x -> x) (fun x -> x) r
let mk_network_option r =
  mk_option
    (fun x -> match String.lowercase_ascii @@ Js.to_string x with
       | "dune" | "dune network" | "dune_network" -> LiquidOptions.Dune_network
       | "tezos" -> LiquidOptions.Tezos_network
       | s -> failwith "Bad value for network: %s" s)
    (function
      | LiquidOptions.Dune_network -> Js.string "dune"
      | LiquidOptions.Tezos_network -> Js.string "tezos") r


let () =
  let open LiquidOptions in
  Js.export "options" @@ jsobject
val inline = mk_bool_option inline
val simplify = mk_bool_option simplify
val peephole = mk_bool_option peephole
val typeonly = mk_bool_option typeonly
val parseonly = mk_bool_option parseonly
val singleline = mk_bool_option singleline
val ignore_annots_ = mk_bool_option ignore_annots
val retry_without_annots_ = mk_bool_option retry_without_annots
val no_annot_ = mk_bool_option no_annot
val no_uncurrying_ = mk_bool_option no_uncurrying
val main = mk_string_opt_option main
val reason_syntax_ = mk_not_bool_option ocaml_syntax
val writeinfo = mk_bool_option writeinfo
val signature = mk_string_opt_option signature
val node = mk_string_option node
val source = mk_string_opt_option source
val private_key_ = mk_string_opt_option private_key
val public_key_ = mk_string_opt_option public_key
val amount = mk_tez_option amount
val fee = mk_tez_opt_option fee
val gas_limit_ = mk_int_opt_option gas_limit
val storage_limit_ = mk_int_opt_option storage_limit
val counter = mk_int_opt_option counter
val network = mk_network_option network
end

let () =
  Js.export "compiler" @@ jsobject
method compile js_str =
  Js.to_string js_str
  |> compile_to_string
  |> Js.string
method decompile js_str =
  Js.to_string js_str
  |> decompile_string
  |> Js.string
end

let () =
  Js.export "obj_compiler" @@ jsobject
method compile js_str =
  Js.to_string js_str
  |> compile_to_json
  |> Ezjsonm.value_to_js
method decompile obj =
  Ezjsonm.value_from_js obj
  |> decompile_json
  |> Js.string
end

module Client = LiquidityToMichelsonClient.String.Async
open LiquidityToMichelson
open Lwt.Infix

(* Helper functions *)
open Js_of_ocaml

let new_promise f =
  Js.Unsafe.new_obj Js.Unsafe.global##_Promise [|Js.Unsafe.inject f|]

let lwt_to_promise t =
  new_promise (fun resolve reject ->
      Lwt.catch (fun () -> t >>= resolve) reject
    )

let js_to_contract j =
  try
    match Js.to_string (Js.typeof j) with
    | "object" ->
      From_strings
        (Array.to_list
           (Js.to_array (Js.Unsafe.coerce j : 'a Js.js_array Js.t))
         |> List.map Js.to_string)
    | "string" ->
      From_strings [
        Js.to_string (Js.Unsafe.coerce j : Js.js_string Js.t)
      ]
    | _ -> raise Not_found
  with _ ->
    failwith "Bad contract argument: %s" j##toString()

let to_string_list j =
  try
    Array.to_list
      (Js.to_array (Js.Unsafe.coerce j : 'a Js.js_array Js.t))
    |> List.map Js.to_string
  with _ ->
    failwith "Must be an array of strings: %s" j##toString()

(* Client export *)

let () =
  Js.export "client" @@ jsobject

method init_storage_ contract args =
  lwt_to_promise (
    Client.init_storage (js_to_contract contract) (to_string_list args)
    >|= Js.string
  )

method run o =
  Client.run o##contract o##entry_name o##input o##storage

end

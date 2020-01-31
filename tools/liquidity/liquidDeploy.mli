(****************************************************************************)
(*                               Liquidity                                  *)
(*                                                                          *)
(*                  Copyright (C) 2017-2019 OCamlPro SAS                    *)
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


type from =
  | From_strings of string list
  | From_files of string list

type bm_id =
  | Bm_id of int
  | Bm_name of int * string

type ('id, 'const) big_map_diff_item =
  | Big_map_add of { id : 'id;
                     key_hash : string;
                     key : 'const;
                     value : 'const }
  | Big_map_remove of { id : 'id;
                        key_hash : string;
                        key : 'const }
  | Big_map_delete of { id : 'id }
  | Big_map_alloc of { id : 'id }
  | Big_map_copy of { source_id : 'id;
                      destination_id : 'id }

type ('id, 'const) big_map_diff = ('id, 'const) big_map_diff_item list

type liq_big_map_diff = (bm_id, LiquidTypes.typed_const) big_map_diff_item list

type stack_item =
  | StackConst of LiquidTypes.typed_const
  | StackCode of int

type trace_item = {
  loc : LiquidTypes.location option;
  gas : int;
  stack : (stack_item * string option) list;
}

type trace = trace_item array

type internal_operation =
  | Reveal of string
  | Transaction of {
      amount : string;
      destination : string;
      entrypoint : string;
      parameters : LiquidTypes.typed_const option;
    }
  | Origination of {
      delegate: string option ;
      script: (LiquidTypes.typed_contract * LiquidTypes.typed_const) option ;
      balance: string ;
    }
  | Delegation of string option

type operation = {
  source : string;
  nonce : int;
  op : internal_operation;
}

exception RequestError of int * string
exception ResponseError of string
exception RuntimeError of LiquidTypes.error * trace option
exception LocalizedError of LiquidTypes.error
exception RuntimeFailure of LiquidTypes.error * string option * trace option

val post : (data:string -> string -> string Lwt.t) ref
val get : (string -> string Lwt.t) ref

module type S = sig
  type 'a t

  (** Run contract with given parameter and storage on the Dune node specified
      in ![LiquidOptions], returns the return value, the storage and a diff of a
      big map id the contract contains any *)
  val run :
    from -> string -> string -> string ->
    (operation list * LiquidTypes.typed_const * liq_big_map_diff) t

  val run_debug :
    from -> string -> string -> string ->
    (operation list * LiquidTypes.typed_const * liq_big_map_diff * trace) t

  (** Compute the initial storage for a specific script, returns storage data *)
  val init_storage : from -> string list -> LiquidTypes.encoded_const t

  val forge_deploy_script :
    source:string -> from -> string list ->
    (string * string * LiquidToMicheline.loc_table) t

  (** Forge a deployment operation contract on the Dune node specified in
      ![LiquidOptions], returns the hex-encoded operation *)
  val forge_deploy : from -> string list -> string t

  (** Deploy a Liquidity contract on the Dune node specified in
      ![LiquidOptions], returns the operation hash and the contract address *)
  val deploy :
    from -> string list -> (string * (string, exn) result) t

  val get_storage : from -> string -> LiquidTypes.typed_const t

  val get_big_map_value :
    from -> bm_id * LiquidTypes.datatype * LiquidTypes.datatype -> string ->
    LiquidTypes.typed_const option t

  val forge_call_parameter :
    from -> string -> string -> string * LiquidToMicheline.loc_table

  (** Forge an operation to call a deploy contract, returns the hex-encoded
      operation *)
  val forge_call : from -> string -> string -> string -> string t

  (** Calls a deployed Liquidity contract on the Dune node specified in
      ![LiquidOptions], returns the operation hash *)
  val call : from -> string -> string -> string ->
    (string * (unit, exn) result) t

  val activate : secret:string -> string t

  (** Inject an operation in hexa with its signature, and returns an
      operation hash *)
  val inject : operation:string -> signature:string -> string t

  (** Packs data as bytes *)
  val pack : ?liquid:from -> const:string -> ty:string -> string t
end

module Async : S with type 'a t = 'a Lwt.t

module Sync : S with type 'a t = 'a

val forge_call_arg : ?entry_name:string ->  from -> string -> string

val list_big_maps : LiquidTypes.typed_const -> LiquidTypes.datatype ->
  (bm_id * LiquidTypes.datatype * LiquidTypes.datatype ) list

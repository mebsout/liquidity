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

(** Parse/convert Michelson contracts and constants. *)

exception Missing_program_field of string

(** Convert a Micheline contant as a typed Liquidity constant.

    The type is used to recover an actual Liquidity constant, e.g.,
    {[ (0, (1, 2)) ]} with type {[ t = { x:int; y:int; z:int } ]} is
    converted to {[ { x = 0; y = 1; z = 2 } ]}. *)
val convert_const_type :
  LiquidMichelineTypes.env ->
  LiquidMichelineTypes.expr ->
  LiquidTypes.datatype ->
  LiquidTypes.loc_michelson LiquidTypes.const * LiquidTypes.location

(** Convert a Micheline contant as Liquidity constant. *)
val convert_const_notype :
  LiquidMichelineTypes.env ->
  LiquidMichelineTypes.expr ->
  LiquidTypes.loc_michelson LiquidTypes.const * LiquidTypes.location

(** Parse a Micheline contract as an intermediate Michelson contract.  *)
val convert_contract :
  LiquidMichelineTypes.env ->
  LiquidMichelineTypes.contract ->
  LiquidTypes.loc_michelson_contract

(** Parse a string as a Micheline contract. *)
val contract_of_string :
  string -> (* maybe filename *)
  string -> (* content *)
  (LiquidMichelineTypes.contract * LiquidMichelineTypes.env) option

(** Parse a string as a Micheline constant. *)
val const_of_string :
  string -> (* maybe filename *)
  string -> (* content *)
  (LiquidMichelineTypes.expr * LiquidMichelineTypes.env) option

val convert_env : LiquidMichelineTypes.env -> LiquidTypes.env

(** Extract usefule information from environement for decompiling
    phase. *)
val infos_env :
  LiquidMichelineTypes.env ->
  bool (* true if tz annoted *)
  * (LiquidTypes.datatype, string) Hashtbl.t
  * (string * LiquidTypes.datatype) list

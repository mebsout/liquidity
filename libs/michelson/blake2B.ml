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

(* module S = struct
 *   #include "../../tezos/src/lib_crypto/s.ml"
 * end *)

module Clic = struct
  let param ~name ~desc _ _ = ()
  let parameter _ = ()
end

module RPC_arg = struct
  type 'a t
  let make
      ~name
      ~descr
      ~destruct
      ~construct
      () = () (* assert false *)
end

module Helpers = struct
  #include "../../tezos/src/lib_crypto/helpers.ml"
end

#include "../../tezos/src/lib_crypto/blake2B.ml"

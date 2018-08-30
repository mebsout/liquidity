(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)
module IntMap = Map.Make(struct type t = int let compare = compare end)

exception InvalidFormat of string * string

type tez = { tezzies : string; mutez : string option }
type integer = { integer : Z.t }

type const =
  | CUnit
  | CBool of bool
  | CInt of integer
  | CNat of integer
  | CTez of tez
  | CTimestamp of string
  | CString of string
  | CBytes of string
  | CKey of string
  | CSignature of string
  | CTuple of const list
  | CNone
  | CSome of const

  (* Map [ key_x_value_list ] or (Map [] : ('key,'value) map) *)
  | CMap of (const * const) list
  | CBigMap of (const * const) list
  | CList of const list
  | CSet of const list

  | CLeft of const
  | CRight of const

  | CKey_hash of string
  | CContract of string
  | CAddress of string

  | CRecord of (string * const) list
  | CConstr of string * const

 and datatype =
   (* michelson *)
  | Tunit
  | Tbool
  | Tint
  | Tnat
  | Ttez
  | Tstring
  | Tbytes
  | Ttimestamp
  | Tkey
  | Tkey_hash
  | Tsignature
  | Toperation
  | Taddress

  | Ttuple of datatype list

  | Toption of datatype
  | Tlist of datatype
  | Tset of datatype

  | Tmap of datatype * datatype
  | Tbigmap of datatype * datatype
  | Tcontract of contract_sig
  | Tor of datatype * datatype
  | Tlambda of datatype * datatype

  (* liquidity extensions *)
  | Trecord of string * (string * datatype) list
  | Tsum of string * (string * datatype) list
  | Tclosure of (datatype * datatype) * datatype
  | Tfail

and entry_sig = {
  entry_name : string;
  parameter : datatype;
  parameter_name : string;
  storage_name : string;
}

and 'exp entry = {
  entry_sig : entry_sig;
  code : 'exp;
}

and 'exp contract = {
  contract_name : string;
  storage : datatype;
  values : (string * bool (* inline *) * 'exp) list;
  entries : 'exp entry list;
}

and entries_sig = entry_sig list

and contract_sig = {
  sig_name : string option;
  entries_sig : entries_sig;
}

let size_of_type = function
  | Ttuple l -> List.length l
  | Trecord (_, l) -> List.length l
  | _ -> raise (Invalid_argument "size_of_type")

let comparable_type = function
  | Tbool
  | Tint
  | Tnat
  | Ttez
  | Tstring
  | Tbytes
  | Ttimestamp
  | Tkey_hash
  | Taddress -> true
  | _ -> false

let rec type_contains_nonlambda_operation = function
  | Toperation -> true
  | Tunit | Tbool | Tint | Tnat | Ttez | Tstring | Tbytes
  | Ttimestamp | Tkey | Tkey_hash | Tsignature | Taddress | Tfail -> false
  | Ttuple l -> List.exists type_contains_nonlambda_operation l
  | Toption ty | Tlist ty | Tset ty ->
    type_contains_nonlambda_operation ty
  | Tmap (t1, t2) | Tbigmap (t1, t2) | Tor (t1, t2) ->
    type_contains_nonlambda_operation t1 || type_contains_nonlambda_operation t2
  | Trecord (_, l) | Tsum (_, l) ->
    List.exists (fun (_, t) -> type_contains_nonlambda_operation t) l
  | Tcontract s ->
    List.exists (fun e -> type_contains_nonlambda_operation e.parameter)
      s.entries_sig
  | Tlambda _ | Tclosure _ -> false

let sig_of_contract c = {
  sig_name = None;
  entries_sig = List.map (fun e -> e.entry_sig) c.entries;
}

type location = {
  loc_file : string;
  loc_pos : ((int * int) * (int * int)) option;
}

type error = { err_loc: location; err_msg: string }

exception LiquidError of error

type 'a mic_contract = {
  mic_parameter : datatype;
  mic_storage : datatype;
  mic_code : 'a;
}

type primitive =
   (* resolved in LiquidCheck *)
  | Prim_unknown
  | Prim_coll_find
  | Prim_coll_update
  | Prim_coll_mem
  | Prim_coll_size

  (* generated in LiquidCheck *)
  | Prim_unused

  (* primitives *)
  | Prim_tuple_get
  (* | Prim_record_get of string *)
  | Prim_tuple_set
  (* | Prim_record_set of string *)
  | Prim_tuple

  | Prim_self
  | Prim_balance
  | Prim_now
  | Prim_amount
  | Prim_gas
  | Prim_Left
  | Prim_Right
  | Prim_source
  | Prim_sender
  | Prim_eq
  | Prim_neq
  | Prim_lt
  | Prim_le
  | Prim_gt
  | Prim_ge
  | Prim_compare
  | Prim_add
  | Prim_sub
  | Prim_mul
  | Prim_ediv

  | Prim_map_find
  | Prim_map_update
  | Prim_map_add
  | Prim_map_remove
  | Prim_map_mem
  | Prim_map_size

  | Prim_set_update
  | Prim_set_add
  | Prim_set_remove
  | Prim_set_mem
  | Prim_set_size

  | Prim_Some

  | Prim_list_size
  | Prim_list_rev

  | Prim_create_account
  | Prim_blake2b
  | Prim_sha256
  | Prim_sha512
  | Prim_hash_key
  | Prim_check
  | Prim_default_account
  | Prim_set_delegate
  | Prim_address
  | Prim_pack

  | Prim_Cons
  | Prim_or
  | Prim_and
  | Prim_xor
  | Prim_not
  | Prim_abs
  | Prim_is_nat
  | Prim_int
  | Prim_neg
  | Prim_lsr
  | Prim_lsl

  | Prim_exec

  | Prim_bytes_size
  | Prim_string_size

  | Prim_slice
  | Prim_bytes_sub
  | Prim_string_sub

  | Prim_concat
  | Prim_concat_two
  | Prim_string_concat
  | Prim_bytes_concat

type prim_fold =
  | Prim_map_iter
  | Prim_set_iter
  | Prim_list_iter
  | Prim_map_fold
  | Prim_set_fold
  | Prim_list_fold

  | Prim_coll_iter
  | Prim_coll_fold

type prim_map =
  | Prim_map_map
  | Prim_set_map
  | Prim_list_map
  | Prim_coll_map


type prim_map_fold =
  | Prim_map_map_fold
  | Prim_set_map_fold
  | Prim_list_map_fold
  | Prim_coll_map_fold


let primitive_of_string = Hashtbl.create 101
let string_of_primitive = Hashtbl.create 101


(* Some primitives should be kept internal:
* get and set
* get_last and set_last
* tuple
*)
let () =
  List.iter (fun (n,p) ->
      Hashtbl.add primitive_of_string n p;
      Hashtbl.add string_of_primitive p n;
    )
            [
              "get", Prim_tuple_get;
              "set", Prim_tuple_set;
              "tuple", Prim_tuple;

              "Array.get", Prim_tuple_get;
              "Array.set", Prim_tuple_set;

              "Current.balance", Prim_balance;
              "Current.time", Prim_now;
              "Current.amount", Prim_amount;
              "Current.gas", Prim_gas;
              "Current.source", Prim_source;
              "Current.sender", Prim_sender;

              "Left", Prim_Left;
              "Right", Prim_Right;
              "=", Prim_eq;
              "<>", Prim_neq;
              "<", Prim_lt;
              "<=", Prim_le;
              ">", Prim_gt;
              ">=", Prim_ge;
              "compare", Prim_compare;
              "+", Prim_add;
              "-", Prim_sub;
              "*", Prim_mul;
              "/", Prim_ediv;
              "~-", Prim_neg;

              "Map.find", Prim_map_find;
              "Map.update", Prim_map_update;
              "Map.add", Prim_map_add;
              "Map.remove", Prim_map_remove;
              "Map.mem", Prim_map_mem;
              "Map.cardinal", Prim_map_size;
              "Map.size", Prim_map_size;

              "Set.update", Prim_set_update;
              "Set.add", Prim_set_add;
              "Set.remove", Prim_set_remove;
              "Set.mem", Prim_set_mem;
              "Set.cardinal", Prim_set_size;
              "Set.size", Prim_set_size;

              "Some", Prim_Some;

              "List.rev", Prim_list_rev;
              "List.length", Prim_list_size;
              "List.size", Prim_list_size;

              "Contract.set_delegate", Prim_set_delegate;
              "Contract.address", Prim_address;
              "Contract.self", Prim_self;

              "Account.create", Prim_create_account;
              "Account.default", Prim_default_account;

              "Crypto.blake2b", Prim_blake2b;
              "Crypto.sha256", Prim_sha256;
              "Crypto.sha512", Prim_sha512;
              "Crypto.hash_key", Prim_hash_key;
              "Crypto.check", Prim_check;

              "Bytes.pack", Prim_pack;
              "Bytes.length", Prim_bytes_size;
              "Bytes.size", Prim_bytes_size;
              "Bytes.concat", Prim_bytes_concat;
              "Bytes.slice", Prim_bytes_sub;
              "Bytes.sub", Prim_bytes_sub;

              "String.length", Prim_string_size;
              "String.size", Prim_string_size;
              "String.concat", Prim_string_concat;
              "String.slice", Prim_string_sub;
              "String.sub", Prim_string_sub;

              "@", Prim_concat_two;

              "::", Prim_Cons;
              "lor", Prim_or;
              "or", Prim_or;
              "||", Prim_or;
              "&", Prim_and;
              "land", Prim_and;
              "&&", Prim_and;
              "lxor", Prim_xor;
              "xor", Prim_xor;
              "not", Prim_not;
              "abs", Prim_abs;
              "is_nat", Prim_is_nat;
              "int", Prim_int;
              ">>", Prim_lsr;
              "lsr", Prim_lsr;
              "<<", Prim_lsl;
              "lsl", Prim_lsl;

              "Lambda.pipe" , Prim_exec;
              "|>", Prim_exec;

              "Coll.update", Prim_coll_update;
              "Coll.mem", Prim_coll_mem;
              "Coll.find", Prim_coll_find;
              "Coll.size",Prim_coll_size;
              "Coll.concat",Prim_concat;
              "Coll.slice",Prim_slice;

              "<unknown>", Prim_unknown;
              "<unused>", Prim_unused;

            ]

let primitive_of_string s =
  try
    Hashtbl.find primitive_of_string s
  with Not_found ->
    Printf.eprintf "Debug: primitive_of_string(%S) raised Not_found\n%!" s;
    raise Not_found

let string_of_primitive prim =
  try
    Hashtbl.find string_of_primitive prim
  with Not_found ->
    Printf.eprintf "Debug: string_of_primitive(%d) raised Not_found\n%!"
                   (Obj.magic prim : int);
    raise Not_found


let fold_primitive_of_string = Hashtbl.create 8
let string_of_fold_primitive = Hashtbl.create 8
let () =
  List.iter (fun (n,p) ->
      Hashtbl.add fold_primitive_of_string n p;
      Hashtbl.add string_of_fold_primitive p n;
    )
            [
              "Map.iter", Prim_map_iter;
              "Set.iter", Prim_set_iter;
              "List.iter", Prim_list_iter;
              "Map.fold", Prim_map_fold;
              "Set.fold", Prim_set_fold;
              "List.fold", Prim_list_fold;
              "Coll.iter", Prim_coll_iter;
              "Coll.fold", Prim_coll_fold;
            ]

let fold_primitive_of_string s =
  try
    Hashtbl.find fold_primitive_of_string s
  with Not_found ->
    Printf.eprintf "Debug: fold_primitive_of_string(%S) raised Not_found\n%!" s;
    raise Not_found

let string_of_fold_primitive prim =
  try
    Hashtbl.find string_of_fold_primitive prim
  with Not_found ->
    Printf.eprintf "Debug: string_of_fold_primitive(%d) raised Not_found\n%!"
                   (Obj.magic prim : int);
    raise Not_found


let map_primitive_of_string = Hashtbl.create 4
let string_of_map_primitive = Hashtbl.create 4
let () =
  List.iter (fun (n,p) ->
      Hashtbl.add map_primitive_of_string n p;
      Hashtbl.add string_of_map_primitive p n;
    )
            [
              "Map.map", Prim_map_map;
              "Set.map", Prim_set_map;
              "List.map", Prim_list_map;
              "Coll.map", Prim_coll_map;
            ]

let map_primitive_of_string s =
  try
    Hashtbl.find map_primitive_of_string s
  with Not_found ->
    Printf.eprintf "Debug: map_primitive_of_string(%S) raised Not_found\n%!" s;
    raise Not_found

let string_of_map_primitive prim =
  try
    Hashtbl.find string_of_map_primitive prim
  with Not_found ->
    Printf.eprintf "Debug: string_of_map_primitive(%d) raised Not_found\n%!"
                   (Obj.magic prim : int);
    raise Not_found


let map_fold_primitive_of_string = Hashtbl.create 4
let string_of_map_fold_primitive = Hashtbl.create 4
let () =
  List.iter (fun (n,p) ->
      Hashtbl.add map_fold_primitive_of_string n p;
      Hashtbl.add string_of_map_fold_primitive p n;
    )
            [
              "Map.map_fold", Prim_map_map_fold;
              "Set.map_fold", Prim_set_map_fold;
              "List.map_fold", Prim_list_map_fold;
              "Coll.map_fold", Prim_coll_map_fold;
            ]

let map_fold_primitive_of_string s =
  try
    Hashtbl.find map_fold_primitive_of_string s
  with Not_found ->
    Printf.eprintf "Debug: map_fold_primitive_of_string(%S) raised Not_found\n%!" s;
    raise Not_found

let string_of_map_fold_primitive prim =
  try
    Hashtbl.find string_of_map_fold_primitive prim
  with Not_found ->
    Printf.eprintf "Debug: string_of_map_fold_primitive(%d) raised Not_found\n%!"
                   (Obj.magic prim : int);
    raise Not_found


(* `variant` is the only parameterized type authorized in Liquidity.
   Its constructors, `Left` and `Right` must be constrained with type
   annotations, for the correct types to be propagated in the sources.
*)
type constructor =
  Constr of string
| Left of datatype
| Right of datatype

type pattern =
  | CConstr of string * string list
  | CAny

type ('ty, 'a) exp = {
  desc : ('ty, 'a) exp_desc;
  name : string option;
  ty : 'ty;
  bv : StringSet.t;
  fail : bool;
  transfer : bool;
}

and ('ty, 'a) exp_desc =
  | Let of string * bool * location * ('ty, 'a) exp * ('ty, 'a) exp
  | Var of string * location * string list
  | SetVar of string * location * string list * ('ty, 'a) exp
  | Const of location * datatype * const
  | Apply of primitive * location * ('ty, 'a) exp list
  | If of ('ty, 'a) exp * ('ty, 'a) exp * ('ty, 'a) exp
  | Seq of ('ty, 'a) exp * ('ty, 'a) exp
  | Transfer of location
                * (* contract_ *) ('ty, 'a) exp
                * (* tez_ *) ('ty, 'a) exp
                * (* entry_ *) string option
                * (* arg_ *) ('ty, 'a) exp
  | MatchOption of ('ty, 'a) exp  (* argument *)
                     * location
                     * ('ty, 'a) exp  (* ifnone *)
                     * string * ('ty, 'a) exp (*  ifsome *)
  | MatchList of ('ty, 'a) exp  (* argument *)
                 * location
                 * string * string * ('ty, 'a) exp * (* ifcons *)
                       ('ty, 'a) exp (*  ifnil *)
  | Loop of string * location
              * ('ty, 'a) exp  (* body *)
              * ('ty, 'a) exp (*  arg *)

  | Fold of prim_fold (* iter/fold *)
            * string * location
            * ('ty, 'a) exp (* body *)
            * ('ty, 'a) exp (* arg *)
            * ('ty, 'a) exp (* acc *)

  | Map of prim_map
           * string * location
           * ('ty, 'a) exp (* body *)
           * ('ty, 'a) exp (* arg *)

  | MapFold of prim_map_fold
               * string * location
               * ('ty, 'a) exp (* body *)
               * ('ty, 'a) exp (* arg *)
               * ('ty, 'a) exp (* acc *)

  | Lambda of string (* argument name *)
              * datatype (* argument type *)
              * location
              * ('ty, 'a) exp (* body *)
              * datatype (* final datatype,
                            inferred during typechecking *)

  | Closure of string (* argument name *)
              * datatype (* argument type *)
              * location
              * (string * ('ty, 'a) exp) list (* call environment *)
              * ('ty, 'a) exp (* body *)
              * datatype (* final datatype,
                            inferred during typechecking *)

  | Record of location * (string * ('ty, 'a) exp) list
  | Constructor of location * constructor * ('ty, 'a) exp

  | MatchVariant of ('ty, 'a) exp
                    * location
                    * (pattern * ('ty, 'a) exp) list

  | MatchNat of ('ty, 'a) exp  (* argument *)
                * location
                * string * ('ty, 'a) exp (* ifplus *)
                * string * ('ty, 'a) exp (* ifminus *)

  | Failwith of ('ty, 'a) exp * location

  | CreateContract of location
                      * ('ty, 'a) exp list (* arguments *)
                      * ('ty, 'a) exp contract (* body *)

  | ContractAt of location
                  * ('ty, 'a) exp
                  * datatype

  | Unpack of location
              * ('ty, 'a) exp
              * datatype


type typed
type encoded
type syntax_exp = (unit, unit) exp
type typed_exp = (datatype, typed) exp
type encoded_exp = (datatype, encoded) exp
type live_exp = (datatype * datatype StringMap.t, encoded) exp


let mk =
  let bv = StringSet.empty in
  fun ?name desc ty ->
    let fail, transfer = match desc with
      | Const (_, _, _)
      | Var (_, _, _) -> false, false

      | Failwith (_, _) -> true, false

      | SetVar (_, _, _, e)
      | Constructor (_, _, e)
      | ContractAt (_, e, _)
      | Unpack (_, e, _)
      | Lambda (_, _, _, e, _) -> e.fail, false (* e.transfer *)

      | Seq (e1, e2)
      | Let (_, _, _, e1, e2)
      | Loop (_, _, e1, e2)
      | Map (_, _, _, e1, e2) ->
        e1.fail || e2.fail, false (* e1.transfer || e2.transfer *)

      | Transfer (_, e1, e2, _, e3) ->
        e1.fail || e2.fail || e3.fail,
        true

      | If (e1, e2, e3)
      | MatchOption (e1, _, e2, _, e3)
      | MatchNat (e1, _, _, e2, _, e3)
      | MatchList (e1, _, _, _, e2, e3)
      | Fold (_, _, _, e1, e2, e3)
      | MapFold (_, _, _, e1, e2, e3) ->
        e1.fail || e2.fail || e3.fail,
        false (* e1.transfer || e2.transfer || e3.transfer *)

      | Apply (prim, _, l) ->
        List.exists (fun e -> e.fail) l,
        prim = Prim_set_delegate
        || prim = Prim_create_account
        (* || List.exists (fun e -> e.transfer) l *)

      | Closure (_, _, _, env, e, _) ->
        e.fail || List.exists (fun (_, e) -> e.fail) env,
        false (* e.transfer || List.exists (fun (_, e) -> e.transfer) env *)

      | Record (_, labels) ->
        List.exists (fun (_, e) -> e.fail) labels,
        false (* List.exists (fun (_, e) -> e.transfer) labels *)

      | MatchVariant (e, _, cases) ->
        e.fail || List.exists (fun (_, e) -> e.fail) cases,
        false (* e.transfer || List.exists (fun (_, e) -> e.transfer) cases *)

      | CreateContract (_, l, _) ->
        List.exists (fun e -> e.fail) l,
        true

    in
    { desc; name; ty; bv; fail; transfer }


type michelson_exp =
  | M_INS of string * string option
  | M_INS_CST of string * datatype * const * string option
  | M_INS_EXP of string * datatype list * michelson_exp list * string option

type 'a pre_michelson =
  | RENAME of string option
  | SEQ of 'a list
  | DIP of int * 'a
  | IF of 'a * 'a
  | IF_NONE of 'a * 'a
  | IF_CONS of 'a * 'a
  | IF_LEFT of 'a * 'a
  | LOOP of 'a
  | ITER of 'a
  | MAP of 'a

  | LAMBDA of datatype * datatype * 'a
  | EXEC

  | DUP of int
  | DIP_DROP of int * int
  | DROP
  | CAR
  | CDR
  | CDAR of int
  | CDDR of int
  | PUSH of datatype * const
  | PAIR
  | COMPARE
  | LE | LT | GE | GT | NEQ | EQ
  | FAILWITH
  | NOW
  | TRANSFER_TOKENS
  | ADD
  | SUB
  | BALANCE
  | SWAP
  | GET
  | UPDATE
  | SOME
  | CONCAT
  | MEM
  | SLICE

  | SELF
  | AMOUNT
  | STEPS_TO_QUOTA
  | CREATE_ACCOUNT
  | BLAKE2B
  | SHA256
  | SHA512
  | HASH_KEY
  | CHECK_SIGNATURE
  | ADDRESS

  | CONS
  | OR
  | XOR
  | AND
  | NOT

  | INT
  | ABS
  | ISNAT
  | NEG
  | MUL

  | LEFT of datatype
  | RIGHT of datatype
  | CONTRACT of datatype

  | EDIV
  | LSL
  | LSR

  | SOURCE
  | SENDER

  | SIZE
  | IMPLICIT_ACCOUNT
  | SET_DELEGATE

  | CREATE_CONTRACT of 'a mic_contract

  | PACK
  | UNPACK of datatype

  (* obsolete *)
  | MOD
  | DIV

type loc_michelson = {
  loc : location;
  ins : loc_michelson pre_michelson;
  mutable loc_name : string option;
}

(* let mic ins = ins *)
(* let mic_loc loc ins = { loc; ins } *)

type closure_env = {
  env_vars :  (string (* name outside closure *)
               * datatype
               * int (* index *)
               * (int ref * (* usage counter inside closure *)
                  int ref (* usage counter outside closure *)
                 )) StringMap.t;
  env_bindings : (encoded_exp (* expression to access variable inside closure *)
                  * (int ref * (* usage counter inside closure *)
                     int ref (* usage counter outside closure *)
                    )) StringMap.t;
  call_bindings : (string * encoded_exp) list;
}

type env = {
    (* name of file being compiled *)
    filename : string;

    (* fields modified in LiquidFromOCaml *)
    (* type definitions *)
    mutable types : datatype StringMap.t;
    (* type definitions *)
    mutable contract_types : contract_sig StringMap.t;
    (* labels of records in type definitions *)
    mutable labels : (string * int * datatype) StringMap.t;
    (* constructors of sum-types in type definitions *)
    mutable constrs : (string * datatype) StringMap.t;
  }

(* fields updated in LiquidCheck *)
type typecheck_env = {
    warnings : bool;
    annot : bool;
    decompiling : bool;
    counter : int ref;
    vars : (string * datatype * bool (* fails *) ) StringMap.t;
    vars_counts : int ref StringMap.t;
    env : env;
    to_inline : encoded_exp StringMap.t ref;
    force_inline : encoded_exp StringMap.t ref;
    t_contract_sig : contract_sig;
    t_contract_storage : datatype;
    clos_env : closure_env option;
}

let empty_typecheck_env ~warnings t_contract_sig t_contract_storage env = {
  warnings;
  decompiling = false;
  annot = false;
  counter = ref 0;
  vars = StringMap.empty;
  vars_counts = StringMap.empty;
  to_inline = ref StringMap.empty;
  force_inline = ref StringMap.empty;
  env = env;
  clos_env = None;
  t_contract_sig;
  t_contract_storage;
}


let new_binding env name ?(fail=false) ty =
  let count = ref 0 in
  let env = { env with
              vars = StringMap.add name (name, ty, fail) env.vars;
              vars_counts = StringMap.add name count env.vars_counts;
            } in
  (env, count)


(* decompilation *)

type node = {
  num : int;
  loc : location;
  mutable node_name : string option;
  mutable kind : node_kind;
  mutable args : node list; (* dependencies *)

  mutable next : node option;
  mutable prevs : node list;
}

 and node_kind =
   | N_UNKNOWN of string
   | N_VAR of string
   | N_START
   | N_IF of node * node
   | N_IF_RESULT of node * int
   | N_IF_THEN of node
   | N_IF_ELSE of node
   | N_IF_END of node * node
   | N_IF_END_RESULT of node * node option * int
   | N_IF_NONE of node
   | N_IF_SOME of node * node
   | N_IF_NIL of node
   | N_IF_CONS of node * node * node
   | N_IF_LEFT of node * node
   | N_IF_RIGHT of node * node
   | N_IF_PLUS of node * node
   | N_IF_MINUS of node * node
   | N_TRANSFER
   | N_CREATE_CONTRACT of node_exp mic_contract
   | N_CONST of datatype * const
   | N_PRIM of string
   | N_FAILWITH
   | N_ARG of node * int
   | N_LOOP of node * node
   | N_LOOP_BEGIN of node
   | N_LOOP_RESULT of (* N_LOOP *) node
                                   * (* N_LOOP_BEGIN *) node * int
   | N_LOOP_END of (* N_LOOP *) node
                                * (* N_LOOP_BEGIN *) node
                                * (* final_cond *) node
   | N_FOLD of node * node
   | N_FOLD_BEGIN of node
   | N_FOLD_RESULT of node (* N_FOLD *)
                      * node * int (* N_FOLD_BEGIN *)
   | N_FOLD_END of node (* N_FOLD *)
                   * node (* N_FOLD_BEGIN *)
                   * node (* accumulator *)
   | N_MAP of node * node
   | N_MAP_BEGIN of node
   | N_MAP_RESULT of node (* N_MAP *)
                      * node * int (* N_MAP_BEGIN *)
   | N_MAP_END of node (* N_MAP *)
                   * node (* N_MAP_BEGIN *)
                   * node (* accumulator *)
   | N_LAMBDA of node * node * datatype * datatype
   | N_LAMBDA_BEGIN
   | N_LAMBDA_END of node
   | N_END
   | N_LEFT of datatype
   | N_RIGHT of datatype
   | N_CONTRACT of datatype
   | N_UNPACK of datatype
   | N_ABS
   | N_RESULT of node * int

and node_exp = node * node


type syntax_init = (
    (string * location * datatype) list (* arguments *)
    * syntax_exp (* init code *)
  )

type syntax_contract = syntax_exp contract
type typed_contract = typed_exp contract
type encoded_contract = encoded_exp contract
type michelson_contract = michelson_exp list
type node_contract = node_exp mic_contract
type loc_michelson_contract = loc_michelson mic_contract

let noloc = { loc_file = "<unspecified>"; loc_pos = None }


let unit_contract_sig = {
  sig_name = None;
  entries_sig = [ {
      entry_name = "main";
      parameter = Tunit;
      parameter_name = "parameter";
      storage_name = "storage";
    }];
}

let dummy_contract_sig = {
  sig_name = None;
  entries_sig = [];
}

let dummy_syntax_contract : syntax_contract = {
  contract_name = "_DUMMY";
  values = [];
  storage = Tunit;
  entries = [];
}

type warning =
  | Unused of string
  | UnusedMatched of string

let reserved_keywords = [
  "let"; "in"; "match" ; "int"; "bool"; "string"; "bytes";
  "get"; "set"; "tuple"; "with"; "fun"; "or"; "and"; "land";
  "lor"; "xor"; "not"; "lsl"; "lsr"; "lxor"; "abs"; "type";
  "is_nat";
]

let has_reserved_prefix s =
  (* [
  "tz1"; "tz2"; "tz3" ;
  "edpk"; "sppk"; "p2pk";
  "edsig"; "spsig1"; "p2sig";
     ] *)
  let len = String.length s in
  len >= 3 &&
  match s.[0], s.[1], s.[2] with
  | 't', 'z', ('1' | '2' | '3') -> true
  | 'e', 'd', 'p'
  | 's', 'p', 'p'
  | 'p', '2', 'p' -> len >= 4 && s.[3] = 'k'
  | 'e', 'd', 's'
  | 'p', '2', 's' -> len >= 5 && s.[3] = 'i' && s.[4] = 'g'
  | 's', 'p', 's' -> len >= 6 && s.[3] = 'i' && s.[4] = 'g' && s.[4] = '1'
  | _ -> false

let prefix_entry = "_Liq_entry_"

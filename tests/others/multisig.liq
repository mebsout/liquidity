[%%version 0.13]

(* A proposition of transfer to a destination, the address of (unit, unit)
   contract *)
type proposition = {
  destination : key_hash;
  amount : tez;
}

(* An owner can submit a proposition or remove a previously submitted
   proposition *)
type action = proposition option

(* The multisig contract can be payed to, simply transfering tokens to it,
   or an owner can submit an action *)
type parameter =
  | Pay
  | Manage of action

(* min is the minimum number of owner who must agree for the transaction to
   proceed
   actions is a map from owners to their submited action, for an address to be
   an owner it must have a binding in this map
*)
type storage = {
  owners : (key_hash, nat) map;
  actions : (nat, proposition) map;
  owners_length : nat;
  min_agree : nat;
}

(* fails if the proposition is not a valid one *)
let check_proposition (p:proposition)
    (* [@fails p.amount > Current.balance () ] *)
  =
  if p.amount > Current.balance () then Current.fail () else ()


(* returns the address of the current caller of this multisig contract *)
let get_caller (_ : unit)
    (* [@ensures @result = source_address ] *)
  =
  let c = (Source : (unit, unit) contract) in
  Contract.manager c

(* returns true if the current caller is also an owner *)
let check_caller_owner (s : storage)
    (* [@fails s.actions[source_address] = None ] *)
  =
  let k = get_caller () in
  match Map.find k s.owners with
  | Some _ -> ()
  | None -> Current.fail ()

(* returns true if two proositions are identical *)
let equal_props ((p1:proposition), (p2:proposition))
    (* [@ensures @result = p1 = p2 ] *)
  =
  p1.destination = p2.destination &
  p1.amount = p2.amount

(*@why3

use map.Occ

*)

(* returns true if a proposition p should be executed immediately *)
let should_execute ((p : proposition), (storage : storage))
    (* [@ensures
       @result <=>
         Occ.occ p 0p storage.owners_length >= storage.min_agree
    ] *)
  =
  let nb_agree =
    Map.reduce (fun (((_i:nat), (p':proposition)), (cpt:nat)) ->
        if equal_props (p, p') then cpt + 1p else cpt
      ) storage.actions 0p
  in
  nb_agree >= storage.min_agree


let%entry main
    (parameter : parameter)
    (storage : storage)
  : unit * storage
  [@ensures
    let delta = Current.balance () - old (Current.balance ()) in
    match parameter with
    | Pay -> delta >= 0tz
    | Manage a ->
      match a with
      | None -> delta = 0tz
      | Some p ->
        (* Map.find (Contract.manager (Source : (unit, unit) contract)) *)
        (*   storage.owners <> (None : nat option) *)
        (* /\ *) delta <= 0tz
        /\ (delta < 0tz => p.amount + delta = 0tz)
  ]
  =
  match parameter with
  | Pay ->
    (* Simple payment, nothing to do *)
    (), storage
  | Manage action ->
    (* Owner want to perform an action *)
    let k = get_caller () in
    match Map.find k storage.owners with
    | None -> Current.fail () (* the caller must be an owner*)
    | Some owner ->
    (* Register the desired action in the storage *)
    let storage =
      storage.actions <- Map.update owner action storage.actions in
    let storage =
      match action with
      | None ->
        (* If the action is to remove previous proposition, then nothing to
           tdo *)
        storage
      | Some p ->
        (* The action is new proposition *)
        check_proposition p; (* it must be a valid one *)
        if should_execute (p, storage) then
          (* execute proposition, i.e. transfer tokens *)
          let c_dest = Account.default p.destination in
          let _, storage = Contract.call c_dest p.amount storage () in
          (* reset the map of actions *)
          storage.actions <- (Map [] : (nat, proposition) map)
        else
          storage
    in
    (), storage

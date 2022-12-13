
open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

(****************)
(* Part 1: NFAs *)
(****************)
  

let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list = match s with
 None -> 
    let filt = List.filter (fun (q1,s1,_) -> List.mem q1 qs &&  s1 = s) nfa.delta in
    let mapped = List.map (fun (_,_,z) -> z) filt in
    List.sort_uniq Stdlib.compare mapped

|Some x -> 
  if List.mem x nfa.sigma = true then
    let filt = List.filter (fun (q1,s1,_) -> List.mem q1 qs &&  s1 = s) nfa.delta in
    let mapped = List.map (fun (_,_,z) -> z) filt in
    List.sort_uniq Stdlib.compare mapped
  else []
  (*failwith "unimplemented"*)

let rec e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list = 
  let e_list = union (move nfa qs None) qs in
    if eq qs e_list then qs else e_closure nfa e_list
  (*failwith "unimplemented"*)

let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
    let es = explode s in
      let rec check lst = (function
      |ch::ct ->
        let check2 nfa lst1 s1 = 
          (let aux = move nfa lst1 s1 in 
          e_closure nfa aux) 
        in
          check (check2 nfa lst (Some ch)) ct

      |_-> intersection nfa.fs lst <> [] )
    in
      check (e_closure nfa [nfa.q0]) es

  (*failwith "unimplemented"*)

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)


let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  let f acc c = 
    let moved = move nfa qs (Some c) in
    union [e_closure nfa moved] acc
  in
    List.fold_left f [] nfa.sigma
  (*failwith "unimplemented"*)


let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  let f acc c = 
    let moved = move nfa qs (Some c) in
    let tup = (qs, (Some c), e_closure nfa moved) in
    union [tup] acc 
  in
    List.fold_left f [] nfa.sigma

  (*failwith "unimplemented"*)

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  let lst = intersection qs nfa.fs in
  match lst with
  |[]->[]
  |_->[qs]

  (*failwith "unimplemented"*)


let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) qs work = 
  failwith "unimplemented"

let rec helper_nfa_to_dfa (nfa: ('q,'s) nfa_t) qs work = 
  if work = [] then qs 
  else
      let mapped = List.map (new_states nfa) work in
      let fl = List.flatten mapped in
      let sorted = List.sort_uniq Stdlib.compare fl in
      let w = diff sorted qs in
      let u = union w qs in
      helper_nfa_to_dfa nfa u w


let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t = 
  let e_cl = e_closure nfa [nfa.q0] in
  let lst = helper_nfa_to_dfa nfa [e_cl] [e_cl] in
  let qs = lst in
  let sigma = nfa.sigma in
  let q0 = e_cl in
  let fs = 
        let f acc l = union (new_finals nfa l) acc in
        List.fold_left f [] lst 
  in
  let delta = 
        let f acc l = union (new_trans nfa l) acc in
        List.fold_left f [] lst
  in
  {
      sigma;
      qs;
      q0;
      fs;
      delta
  }

  (*failwith "unimplemented"*)



  (*
  let nfa_ex = {
    sigma = ['a'];
    qs = [0; 1; 2];
    q0 = 0;
    fs = [2];
    delta = [(0, Some 'a', 1); (1, None, 2)]
}

let dfa_ex = {
    sigma = ['a'; 'b'; 'c'];
    qs = [0; 1; 2];
    q0 = 0;
    fs = [2];
    delta = [(0, Some 'a', 1); (1, Some 'b', 0); (1, Some 'c', 2)]
}
    
    *)
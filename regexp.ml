open List
open Nfa
open Sets

(*********)
(* Types *)
(*********)

type regexp_t =
  | Empty_String
  | Char of char
  | Union of regexp_t * regexp_t
  | Concat of regexp_t * regexp_t
  | Star of regexp_t

(***********)
(* Utility *)
(***********)

let fresh =
  let cntr = ref 0 in
  fun () ->
    cntr := !cntr + 1 ;
    !cntr

(*******************************)
(* Part 3: Regular Expressions *)
(*******************************)

let change lst = match lst with
|[x]-> x
|_->failwith "wrong"


let rec regexp_to_nfa (regexp: regexp_t) : (int, char) nfa_t =
  let st_state = fresh() in
  let f_state = fresh() in

  match regexp with
  |Empty_String -> 
    {
      sigma = []
    ; qs = [st_state]
    ; q0 = st_state
    ; fs = [st_state]
    ; delta = []
    }
  
  |Char ch ->
    {
      sigma = [ch]
    ; qs = [st_state; f_state]
    ; q0 = st_state
    ; fs = [f_state]
    ; delta = [(st_state, Some ch, f_state)]  
    }


  |Concat (ch1, ch2) ->
    let l = regexp_to_nfa ch1 in
    let r = regexp_to_nfa ch2 in

    let sigma = union l.sigma r.sigma in
    let qs = union l.qs r.qs in
    let q0 = l.q0 in
    let fs = r.fs in
    
    let delta = 
      (let lst1 = union l.delta r.delta in 
      insert(change l.fs, None, r.q0) lst1) in

      {
        sigma
      ; qs
      ; q0
      ; fs
      ; delta

      }

  |Union (ch1, ch2) ->
    let l = regexp_to_nfa ch1 in
    let r = regexp_to_nfa ch2 in
    let qs = 
      (let lst = union l.qs r.qs in
    insert_all [st_state;f_state] lst) in

    let sigma = 
      union l.sigma r.sigma in

    let q0 = st_state in
    let fs = [f_state] in

    let delta = 
      let lst1 = union l.delta r.delta in
      let lst2 = 
        let st1 = (st_state, None, l.q0) in
        let st2 = (st_state, None, r.q0) in
        let st3 = (change l.fs, None, f_state) in
        let st4 = (change r.fs, None , f_state) in
        [st1;st2;st3;st4] in

     insert_all lst1 lst2
    in
      {
          sigma
        ; qs
        ; q0
        ; fs
        ; delta
      }
        
  |Star ch ->
    let st = regexp_to_nfa ch in

    let sigma = st.sigma in
    let qs = insert_all [st_state; f_state] st.qs in
    let q0 = st_state in
    let fs = [f_state] in

    let delta = 
      let lst = 
        let st1 = (change st.fs, None, f_state) in
        let st2 = (st_state, None, st.q0) in
        let st3 = (st_state, None, f_state) in
        let st4 = (f_state, None, st_state) in
        [st1;st2;st3;st4] in
    insert_all lst st.delta
    in

    {
      sigma
    ; qs
    ; q0
    ; fs
    ; delta
    }




  (*failwith "unimplemented"*)

(*****************************************************************)
(* Below this point is parser code that YOU DO NOT NEED TO TOUCH *)
(*****************************************************************)

exception IllegalExpression of string

(* Scanner *)
type token =
  | Tok_Char of char
  | Tok_Epsilon
  | Tok_Union
  | Tok_Star
  | Tok_LParen
  | Tok_RParen
  | Tok_END

let tokenize str =
  let re_var = Str.regexp "[a-z]" in
  let re_epsilon = Str.regexp "E" in
  let re_union = Str.regexp "|" in
  let re_star = Str.regexp "*" in
  let re_lparen = Str.regexp "(" in
  let re_rparen = Str.regexp ")" in
  let rec tok pos s =
    if pos >= String.length s then [Tok_END]
    else if Str.string_match re_var s pos then
      let token = Str.matched_string s in
      Tok_Char token.[0] :: tok (pos + 1) s
    else if Str.string_match re_epsilon s pos then
      Tok_Epsilon :: tok (pos + 1) s
    else if Str.string_match re_union s pos then Tok_Union :: tok (pos + 1) s
    else if Str.string_match re_star s pos then Tok_Star :: tok (pos + 1) s
    else if Str.string_match re_lparen s pos then Tok_LParen :: tok (pos + 1) s
    else if Str.string_match re_rparen s pos then Tok_RParen :: tok (pos + 1) s
    else raise (IllegalExpression ("tokenize: " ^ s))
  in
  tok 0 str

let tok_to_str t =
  match t with
  | Tok_Char v -> Char.escaped v
  | Tok_Epsilon -> "E"
  | Tok_Union -> "|"
  | Tok_Star -> "*"
  | Tok_LParen -> "("
  | Tok_RParen -> ")"
  | Tok_END -> "END"

(*
   S -> A Tok_Union S | A
   A -> B A | B
   B -> C Tok_Star | C
   C -> Tok_Char | Tok_Epsilon | Tok_LParen S Tok_RParen

   FIRST(S) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(A) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(B) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(C) = Tok_Char | Tok_Epsilon | Tok_LParen
 *)

let parse_regexp (l : token list) =
  let lookahead tok_list =
    match tok_list with
    | [] -> raise (IllegalExpression "lookahead")
    | h :: t -> (h, t)
  in
  let rec parse_S l =
    let a1, l1 = parse_A l in
    let t, n = lookahead l1 in
    match t with
    | Tok_Union ->
        let a2, l2 = parse_S n in
        (Union (a1, a2), l2)
    | _ -> (a1, l1)
  and parse_A l =
    let a1, l1 = parse_B l in
    let t, n = lookahead l1 in
    match t with
    | Tok_Char c ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | Tok_Epsilon ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | Tok_LParen ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | _ -> (a1, l1)
  and parse_B l =
    let a1, l1 = parse_C l in
    let t, n = lookahead l1 in
    match t with Tok_Star -> (Star a1, n) | _ -> (a1, l1)
  and parse_C l =
    let t, n = lookahead l in
    match t with
    | Tok_Char c -> (Char c, n)
    | Tok_Epsilon -> (Empty_String, n)
    | Tok_LParen ->
        let a1, l1 = parse_S n in
        let t2, n2 = lookahead l1 in
        if t2 = Tok_RParen then (a1, n2)
        else raise (IllegalExpression "parse_C 1")
    | _ -> raise (IllegalExpression "parse_C 2")
  in
  let rxp, toks = parse_S l in
  match toks with
  | [Tok_END] -> rxp
  | _ -> raise (IllegalExpression "parse didn't consume all tokens")


let string_to_regexp str = parse_regexp @@ tokenize str

let string_to_nfa str = regexp_to_nfa @@ string_to_regexp str

(*
  Copyright (C) 2018
  AIST Program-IP No. H30PRO-2219
  National Institute of Advanced Industrial Science and Technology (AIST),
  East Japan Railway Company (JR-East)
*)

exception Utils_error of string;;

module IntMap =
  Map.Make (struct
             type t = int
             let compare i j = i - j
           end);;

let intmap_lookup map key default =
  if IntMap.mem key map then
    IntMap.find key map
  else
    default
;;

let iter_with_index proc xs =
  let rec f k xs =
    match xs with
      [] -> ()
    | x::xs -> proc k x; f (k+1) xs
  in
  f 0 xs
;;

let rec interval a b =
  if a >= b then
    []
  else
    a::(interval (a+1) b)
;;

let fold_left_with_index f acc xs =
  let rec g acc k xs =
    if xs = []  then
      acc
    else
      g (f acc k (List.hd xs)) (k + 1) (List.tl xs)
  in
  g acc 0 xs
;;

let butlast xs =
  List.rev (List.tl (List.rev xs))
;;

let rec comb xs n =
  if n = 0 then
    [[]]
  else if xs = [] then
    []
  else
    List.append
      (comb (List.tl xs) n)
      (let x = List.hd xs in
       List.map
         (fun zs -> x::zs)
         (comb (List.tl xs) (n-1)))
;;

let rec append_reverse xs ys =
  match xs with
    [] -> ys
  | (x::xs') -> append_reverse xs' (x::ys)
;;

let map_with_index_n_tail f tail xs =
  let rec g rs k = function
      [] -> append_reverse rs tail
    | (x::xs) -> g ((f k x)::rs) (k+1) xs
  in
  g [] 0 xs
;;

let map_with_index f xs =
  let rec g rs k = function
      [] -> List.rev rs
    | (x::xs) -> g ((f k x)::rs) (k+1) xs
  in
  g [] 0 xs
;;

let char_list_to_string cs =
  let n = List.length cs in
  let s = String.make n ' ' in
  iter_with_index (fun i c -> Bytes.set s i c) cs;
  s
;;

let string_to_char_list s =
  let rec f k cs =
    if k = 0 then
      cs
    else
      f (k-1) ((String.get s (k-1))::cs)
  in
  f (String.length s) []
;;

let last xs =
  List.hd (List.rev xs)
;;

(* path => (dir, fname, ext) *)
let splitpath path =
  let rec f rs = function
      [] -> ("", char_list_to_string rs, "")
    | c::cs ->
       if c = '.' then
         if rs = [] then
           g "" [] cs
         else
           g (char_list_to_string (c::rs)) [] cs
       else if c = '/' || c = '\\' || c = ':' then
         (char_list_to_string (List.rev (c::cs)), char_list_to_string rs, "")
       else
         f (c::rs) cs
  and g ext rs = function
      [] -> ("", char_list_to_string rs, ext)
    | c::cs ->
       if c = '/' || c = '\\' || c = ':' then
         (char_list_to_string (List.rev (c::cs)), char_list_to_string rs, ext)
       else
         g ext (c::rs) cs
  in
  f [] (List.rev (string_to_char_list path))
;;

let list_index pred xs =
  let rec loop k = function
      [] -> None
    | x::xs ->
       if pred x then
         Some k
       else
         loop (k+1) xs
  in
  loop 0 xs
;;

let rec list_update xs k x =
  if xs=[] then
    raise (Utils_error "list_update: index out of range")
  else
    if k=0 then
      x::(List.tl xs)
    else
      (List.hd xs)::(list_update (List.tl xs) (k-1) x)
;;
let is_whitespace c =
  c = ' ' || c = '\t' ||c = '\n' || c = '\r'
;;

let is_digit c =
  c >= '0' && c <= '9'
;;

let digit_char_to_int c =
  (int_of_char c) - (int_of_char '0')
;;

let member eq x xs =
  let rec loop = function
	  [] -> false
    | y::ys ->
	   if eq x y then
		 true
	   else
		 loop ys
 in
 loop xs
;;

let adjoin eq xs x =
  if member eq x xs then
	xs
  else
	x::xs
;;

let union eq a b =
  let rec loop rs = function
	  [] -> rs
	| x::xs ->
	   if member eq x a then
		 loop rs xs
	   else
		 loop (x::rs) xs
  in loop a b
;;

let diff eq a b =
  let rec loop rs = function
	  [] -> rs
	| x::xs ->
	   if member eq x b then
		 loop rs xs
	   else
		 loop (x::rs) xs
  in loop [] a
;;

let print_string_int k = Printf.sprintf "%d" k;;

let print_string_list print_string xs =
  let rec loop s = function
      [] -> s ^ "]"
    | x::xs ->
       loop (s ^ "; " ^ (print_string x)) xs
  in
  match xs with
    [] -> "[]"
  | x::xs ->
     loop ("[" ^ (print_string x)) xs
;;

let divide_list pred xs =
  let rec loop ys zs = function
      [] -> (List.rev ys, List.rev zs)
    | (x::xs) ->
       if pred x then
         loop (x::ys) zs xs
       else
         loop ys (x::zs) xs
  in
  loop [] [] xs
;;

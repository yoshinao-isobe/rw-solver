(*
  Copyright (C) 2018
  AIST Program-IP No. H30PRO-2219
  National Institute of Advanced Industrial Science and Technology (AIST),
  East Japan Railway Company (JR-East)
*)

(*
  restricted symbolic expressions
  with reader/writer
*)
open Utils
open Ungetch

type rsexpr = Sym of string | Int of int | List of rsexpr list;;

exception Rsexpr_error;;
exception Invalid_rsexpr;;

let cons x xs =
  match xs with
    List zs -> List (x::zs)
  | _ -> raise Rsexpr_error
;;

let read_rsexpr uch =
  let rec rd () =
    skip_space uch;
    let c = getc uch in
    if c = end_of_file then
      raise End_of_file
    else if c = '(' then
      rd_list []
    else if c = '-' then
      let c = getc uch in
      if is_digit c then
        rd_int (-1) (digit_char_to_int c)
      else (
        ungetc uch c;
        rd_sym ['-']
      )
    else if is_digit c then
      rd_int 1 (digit_char_to_int c)
    else
      rd_sym [c]
  and rd_list xs =
    skip_space uch;
    let c = getc uch in
    if c = ')' then
      List (List.rev xs)
    else
      (ungetc uch c;
       let x = rd () in
       rd_list (x::xs))
  and rd_sym cs =
    let c = getc uch in
    if c = end_of_file || is_whitespace c then
      if cs == [] then
        raise Invalid_rsexpr
      else
        Sym (char_list_to_string (List.rev cs))
    else
      rd_sym (c::cs)
  and rd_int sign k =
    let c = getc uch in
    if is_digit c then
      rd_int sign (k * 10 + digit_char_to_int c)
    else (
      ungetc uch c;
      Int (sign * k))
  in
  rd ()
;;

let rec write_rsexpr ch = function
    Sym s -> Printf.fprintf ch "%s" s
  | Int k -> Printf.fprintf ch "%d" k
  | List [] -> Printf.fprintf ch "()"
  | List (x::xs) ->
     Printf.fprintf ch "(";
     write_rsexpr ch x;
     List.iter
       (fun x ->
         Printf.fprintf ch " ";
         write_rsexpr ch x)
       xs;
     Printf.fprintf ch ")"
;;

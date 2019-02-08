(*
  Copyright (C) 2018
  AIST Program-IP No. H30PRO-2219
  National Institute of Advanced Industrial Science and Technology (AIST),
  East Japan Railway Company (JR-East)
*)

(*
  ungetc
*)
open Utils

type unget_channel = {ch : in_channel; buf : char list ref};;

let end_of_file = '\x00';;

let make_unget_channel ch = {ch = ch; buf = ref []};;
let open_in path = make_unget_channel (Pervasives.open_in path);;
let close_in uch = Pervasives.close_in uch.ch;;

let getc uch =
  match !(uch.buf) with
    [] -> (try
             input_char uch.ch
           with
             End_of_file -> end_of_file)
  | c::cs -> uch.buf := cs; c
;;

let ungetc uch c =
  uch.buf := c::!(uch.buf)
;;

let rec skip_space uch =
  let c = getc uch in
  if is_whitespace c then
    skip_space uch
  else
    ungetc uch c
;;

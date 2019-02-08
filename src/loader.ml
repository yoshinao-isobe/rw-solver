(*
  Copyright (C) 2018
  AIST Program-IP No. H30PRO-2219
  National Institute of Advanced Industrial Science and Technology (AIST),
  East Japan Railway Company (JR-East)
*)

open Model;;

exception Loader_error of string;;

let add_id_to_modules ms =
  let rec loop k rs = function
      [] -> List.rev rs
    | (m:rw_module)::ms ->
       loop (k+1) ({ m with id=k }::rs) ms
  in
  loop 0 [] ms
;;

let add_id_to_trains ts =
  let rec loop k rs = function
      [] -> List.rev rs
    | t::ts ->
       loop (k+1) ({ t with id=k }::rs) ts
  in
  loop 0 [] ts
;;

let load_model model_filename =
  let ch = open_in model_filename in
  let lexbuf = Lexing.from_channel ch in
  try
    let model = Parser.model Lexer.token lexbuf in
    close_in ch;
    { model with
      module_list = add_id_to_modules model.module_list;
      train_list = add_id_to_trains model.train_list; }
  with
    Parsing.Parse_error ->
    close_in ch;
    let pos = Lexing.lexeme_start_p lexbuf in
    let msg =
      Printf.sprintf 
        "%s:%d:%d: parse error around the token: '%s'\n"
        model_filename
		pos.Lexing.pos_lnum
		(pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
        (Lexing.lexeme lexbuf) in
    raise (Loader_error msg)
;;

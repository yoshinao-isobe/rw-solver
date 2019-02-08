(*
  Copyright (C) 2018
  AIST Program-IP No. H30PRO-2219
  National Institute of Advanced Industrial Science and Technology (AIST),
  East Japan Railway Company (JR-East)
*)

{
open Parser;;
}

let space = [' ' '\t' '\r']
let digit = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z']

rule token = parse
| space+
	{ token lexbuf }
| "//"
	{ line_comment lexbuf; token lexbuf }
| '\n'
	{ Lexing.new_line lexbuf; token lexbuf }
| "("
	{ LPAR }
| ")"
	{ RPAR }
| "["
	{ LSQBRACKET }
| "]"
	{ RSQBRACKET }
| "{"
	{ LCURBRACE }
| "}"
	{ RCURBRACE }
| ","
	{ COMMA }
| ";"
	{ SEMICOLON }
| "="
	{ EQ }
| "max_time"
    { MAX_TIME }
| "repeat_interval"
	{ REPEAT_INTERVAL }
| "period"
	{ REPEAT_INTERVAL }
| "module"
	{ MODULE }
| "struct"
	{ MODULE }
| "train"
	{ TRAIN }
| "station"
	{ STATION }
| "name"
	{ NAME }
| "capacity"
	{ CAPACITY }
| "interval"
	{ INTERVAL }
| "headway"
	{ INTERVAL }
| "links"
	{ LINKS }
| "fifos"
	{ FIFOS }
| "singletracks"
	{ SINGLETRACKS }
| "excls"
	{ SINGLETRACKS }
| "end_time"
	{ END_TIME }
| "route"
	{ ROUTE }
| "req_time"
	{ REQ_TIME }
| "req_times"
	{ REQ_TIMES }
| "req_platforms"
	{ REQ_PLATFORMS }
| "req_tracks"
	{ REQ_PLATFORMS }
| "start_time"
    { START_TIME_BETWEEN }
| "total_time"
    { MAX_TOTAL_TIME }
| "max_total_time"
    { MAX_TOTAL_TIME }
| "max_total_time_until"
    { MAX_TOTAL_TIME_UNTIL }
| "infinity"
	{ INFINITY }
| "_"
    { WILDCARD }
| "@"
    { MODEL_PARAMETER_MARK }
| digit+
	{ NUM(int_of_string (Lexing.lexeme lexbuf)) }
| '"' [^ '"']* '"'
    {
      let s = Lexing.lexeme lexbuf in
      let s' = String.sub s 1 ((String.length s) - 2) in
      STRING(s')
    }
| alpha (alpha | digit | '_')*
    { IDENTIFIER(Lexing.lexeme lexbuf) }
| eof
	{ EOF }
| _
	{ failwith
        (Printf.sprintf "unknown token %s near characters %d-%d"
           (Lexing.lexeme lexbuf)
           (Lexing.lexeme_start lexbuf)
           (Lexing.lexeme_end lexbuf)) }

and block_comment = parse
| "/*"
	{ () }
| "*/"
	{ block_comment lexbuf;
	  block_comment lexbuf }
| eof
	{ () }
| _
	{ block_comment lexbuf }

and line_comment = parse
| "\n"
	{ Lexing.new_line lexbuf; () }
| eof
	{ () }
| _
	{ line_comment lexbuf }

(*
  Copyright (C) 2018
  AIST Program-IP No. H30PRO-2219
  National Institute of Advanced Industrial Science and Technology (AIST),
  East Japan Railway Company (JR-East)
*)

exception Error of string

type output_type = PDF | SVG

let option_xscale = ref 1.0
let option_yscale = ref 1.0
let option_output_type = ref PDF
let option_output_filename = ref ""
let option_leave_z3_files = ref ""
let option_reuse_z3_files = ref ""
let option_suppress_output = ref false
let option_ignore_req_time_max = ref false
let option_suppress_consistency_module_links = ref false
let option_calc_platform_by_z3 = ref true
let option_prt_slot_no = ref false
let option_smt_solver = ref "z3"

let usage_msg =
"usage: rwsolver [options] <model-filename>
  The option '--help' shows more details.
"
;;

let spec =
  [
   ("-xs", Arg.Float(fun x -> option_xscale := x),
	"<x-scale>             x-scale = 0.1 ~ 10.0");
   ("-ys", Arg.Float(fun x -> option_yscale := x),
	"<y-scale>             y-scale = 0.1 ~ 10.0");
   ("-t", Arg.String(fun x ->
              if x = "pdf" || x = "PDF" then
                option_output_type := PDF
              else if x = "svg" || x = "SVG" then
                option_output_type := SVG
              else
                raise (Error ("unknown output type: " ^ x))),
	"{pdf|svg}              output file type");
   ("-o", Arg.String(fun x -> option_output_filename := x),
	"<filename>             output filename");
   ("-lz", Arg.String(fun x -> option_leave_z3_files := x),
	"<basename>            leave Z3 files");
   ("-rz", Arg.String(fun x -> option_reuse_z3_files := x),
	"<filename>            reuse Z3 files");
   ("-so", Arg.Unit(fun () -> option_suppress_output := true),
	"        calc only");
   ("-irtm", Arg.Unit(fun () -> option_ignore_req_time_max := true),
	"    ignore 'max' part of all the 'req_times'");
   ("-scml", Arg.Unit(fun () -> option_suppress_consistency_module_links := true),
	"    suppress checking for consistency of module links");
   ("-itn", Arg.Unit(fun () -> option_calc_platform_by_z3 := false),
	"    ignore track number requests");
   ("-ptn", Arg.Unit(fun () -> option_prt_slot_no := true),
	"    print track number in timetable");
   ("--solver", Arg.String(fun x -> option_smt_solver := x),
	"<solver-name>             specify SMT solver");
  ]
;;

let g_r_args = ref ([] : string list);;

let anon_fun s =
  g_r_args := s::!g_r_args
;;

let get_args () = List.rev !g_r_args;;

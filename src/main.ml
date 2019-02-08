(*
  Copyright (C) 2018
  AIST Program-IP No. H30PRO-2219
  National Institute of Advanced Industrial Science and Technology (AIST),
  East Japan Railway Company (JR-East)
*)

open Model
open Utils
open Loader
open Solver
open Viz
open Viz2

let main () =
  Arg.parse Getopt.spec Getopt.anon_fun Getopt.usage_msg;
  let args = Getopt.get_args() in
  if List.length args <> 1 then
    Printf.printf "%s" Getopt.usage_msg
  else (
    let model_filename = List.hd args in
    let (dir, fname, ext) = splitpath model_filename in
    let base_name = dir ^ fname in
    (* load model *)
    let model = load_model model_filename in
    let options = { default_options with
                    max_time_opt = model.max_time_opt;
                    repeat_interval_opt = model.repeat_interval_opt;
                    parameter_map = model.parameter_map;
                  } in
    (* solver *)
    let (train_map, map_link_to_modules, map_module_to_trains, train_time_map, platform_map, min_time, max_time) =
      process_model model.module_list model.train_list options in
    (* draw diagram *)
    if not !Getopt.option_suppress_output then
      draw_diagram base_name model.module_list model.train_list train_map map_link_to_modules map_module_to_trains
                   train_time_map platform_map min_time max_time options
                   (4.0 *. !Getopt.option_xscale) (4.0 *. !Getopt.option_yscale)
  )
;;

let () = main ();;

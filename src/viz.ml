(*
  Copyright (C) 2018
  AIST Program-IP No. H30PRO-2219
  National Institute of Advanced Industrial Science and Technology (AIST),
  East Japan Railway Company (JR-East)
*)

open Model
open Utils
open Solver

(*
A4 size: pt = 1/72 inch
let width  = 11.692 *. 72.0;;
let height =  8.267 *. 72.0;;
*)

let left_margin = ref 30.0
let right_margin = ref 40.0
let top_margin = ref 30.0
let bottom_margin = ref 30.0  (* - module_offset *)
let module_title_width = ref 10.0
let module_name_size = ref 12.0
let module_offset = ref 0.0
let stop_line_width = ref 4.0
let run_line_width = ref 2.0
let circle_raidus = ref 3.0
let train_top_margin = ref 18.0
let train_bottom_margin = ref 18.0
let train_offset = ref 18.0
let pen_width_station = ref 6.0
let pen_width_rail = ref 2.0
let pen_width_dash = ref 1.0
let font_size_station = ref 14.0
let font_size_time = ref 8.0

let pen_dash = ref [| 2.0; 2.0 |]  (* see set_viz_parameters *)

let color_list = [
    (1.0, 0.0, 0.0);
    (0.0, 0.0, 1.0);
    (0.5, 0.0, 0.5);
    (0.6, 0.6, 0.0);
    (0.65, 0.16, 0.26);
  ];;

(*
  線路モジュールの最小通過時間を求める
  各モジュールについて
  列車インスタンスを列挙し
  seq_id から req_time を求め
  min の min を計算する
  => map: module_id => time
*)
let seek_min_req_time module_list train_map map_module_to_trains =
  List.fold_left
    (fun map m ->
      if m.station then
        map
      else
        let ts = get_train_infos map_module_to_trains m.id in
        match ts with
          [] ->
          let mn =
            match m.req_time with
              None -> 1
            | Some (mn, mx_opt) -> mn
          in
          IntMap.add m.id mn map
        | (t0, s0)::ts ->
           let train0 = IntMap.find t0 train_map in
           let mn = List.fold_left
                      (fun mn (t, s) ->
                        let train = IntMap.find t train_map in
                        let req_times =
                          match train.req_times with
                            None -> raise (RWError "req_times")
                          | Some rt -> rt in
                        let req_time = List.nth req_times s in
                        match req_time with
                          None -> raise (RWError "default req_time is not set properly")
                        | Some (mn2, mx_opt) ->
                           min mn mn2)
                      (let req_times =
                         match train0.req_times with
                           None -> raise (RWError "default req_time is not set properly")
                         | Some rt -> rt in
                         match List.nth req_times s0 with
                          None -> raise (RWError "default req_time is not set properly")
                        | Some (mn, mx_opt) -> mn)
                      ts in
           IntMap.add m.id mn map)
    IntMap.empty module_list
;;

(*
  Calc the height of each module, and the position by accumulating them

  => (total_height, map: module_id => (pos, height))
*)
let calc_module_height_n_diagram_height module_list map_line_module_to_width scale =
  List.fold_left
    (fun (h, map) m ->
      if m.station then
        let dh = !train_top_margin +. (float_of_int ((station_capacity m) - 1)) *. !train_offset +. !train_bottom_margin in
        (h +. !module_offset +. dh, IntMap.add m.id (h, dh) map)
      else
        let min_time = IntMap.find m.id map_line_module_to_width in
        let dh = (float_of_int min_time) *. scale in
        (h +. !module_offset +. dh, IntMap.add m.id (h, dh) map))
    (0., IntMap.empty) module_list
;;


(*
  enter_time 順にソートされた
  ss : ((train_id, seq_id), (enter_time, exit_time)) list
  を受け取り
  (train_id, seq_id) における platform_no を決定する
*)
let decide_platform_no pm capacity ss =
  (* platform management list *)
  let ps = List.map (fun k -> None) (interval 0 capacity) in
  let (pm, ps) =
    List.fold_left
      (fun (pm, ps) ((train_id, seq_id), (enter_time, exit_time)) ->
        (* search for available platform *)
        let p = list_index
                  (fun p ->
                    match p with
                      None -> true
                    | Some time -> time <= enter_time)
                  ps in
        match p with
          None -> raise (RWError "decide_platform_no: something bad happened")
        | Some k ->
           let ps' = list_update ps k (Some exit_time) in
           let pm' = IntMap.add train_id
                                (IntMap.add seq_id
                                            k
                                            (intmap_lookup pm train_id IntMap.empty))
                                pm in
           (pm', ps'))
      (pm, ps) ss in
  pm
;;

let print_ss ss =
  let rec f = function
      [] -> Printf.printf "\n";
    | ((t, s), (nt, xt))::ss ->
       Printf.printf "(%d %d %d %d) " t s nt xt;
       f ss
  in f ss
;;

(*
  各モジュールにおいて
  各列車インスタンスの停車プラットホームを求める

  => map: train_id => seq_id => platform_no
*)
let assign_platform module_list map_module_to_trains train_time_map repeat_flag repeat_interval =
  List.fold_left
    (fun pm (m:rw_module) ->
      if m.station then
        (* train instances (t, s) list of the module m *)
        let ts = get_train_infos map_module_to_trains m.id in
        (* (t, s) list => ((t, s), (enter_time, exit_time)) list *)
        let ss = List.fold_left
                   (fun acc (t, s) ->
                     let time_list = IntMap.find t train_time_map in
                     let enter_time = List.nth time_list s
                     and exit_time = List.nth time_list (s+1) in
                     if not repeat_flag then
                       ((t, s), (enter_time, exit_time))::acc
                     else
                       let num_bands = 2 in
                       List.fold_left
                         (fun acc i ->
                           let j =
                             let ntd = enter_time / repeat_interval
                             and xtd = exit_time / repeat_interval in
                             if ntd = xtd then i else i + 1 in
                           let ntm = enter_time mod repeat_interval
                           and xtm = exit_time mod repeat_interval in
                           ((t, s), (ntm + i * repeat_interval, xtm + j * repeat_interval))::acc)
                         acc (interval 0 num_bands)
                   )
                   [] ts in
        (* sort ss by time *)
        let ss' = List.sort (fun ((t0, s0), (nt0, xt0)) ((t1, s1), (nt1, xt1)) -> nt0 - nt1) ss in
        (* print_ss ss'; *)
        decide_platform_no pm (station_capacity m) ss'
      else
        pm
    )
    IntMap.empty module_list
;;

let set_viz_parameters options =
  left_margin := float_of_int (get_model_parameter_int options "left_margin" (int_of_float !left_margin));
  right_margin := float_of_int (get_model_parameter_int options "right_margin" (int_of_float !right_margin));
  top_margin := float_of_int (get_model_parameter_int options "top_margin" (int_of_float !top_margin));
  bottom_margin := float_of_int (get_model_parameter_int options "bottom_margin" (int_of_float !bottom_margin));
  module_title_width := float_of_int (get_model_parameter_int options "module_title_width" (int_of_float !module_title_width));
  module_name_size := float_of_int (get_model_parameter_int options "module_name_size" (int_of_float !module_name_size));
  module_offset := float_of_int (get_model_parameter_int options "module_offset" (int_of_float !module_offset));
  stop_line_width := float_of_int (get_model_parameter_int options "stop_line_width" (int_of_float !stop_line_width));
  run_line_width := float_of_int (get_model_parameter_int options "run_line_width" (int_of_float !run_line_width));
  circle_raidus := float_of_int (get_model_parameter_int options "circle_raidus" (int_of_float !circle_raidus));
  train_top_margin := float_of_int (get_model_parameter_int options "train_top_margin" (int_of_float !train_top_margin));
  train_bottom_margin := float_of_int (get_model_parameter_int options "train_bottom_margin" (int_of_float !train_bottom_margin));
  train_offset := float_of_int (get_model_parameter_int options "train_offset" (int_of_float !train_offset));
  pen_width_station := float_of_int (get_model_parameter_int options "pen_width_station" (int_of_float !pen_width_station));
  pen_width_rail := float_of_int (get_model_parameter_int options "pen_width_rail" (int_of_float !pen_width_rail));
  pen_width_dash := float_of_int (get_model_parameter_int options "pen_width_dash" (int_of_float !pen_width_dash));
  font_size_station := float_of_int (get_model_parameter_int options "font_size_station" (int_of_float !font_size_station));
  font_size_time := float_of_int (get_model_parameter_int options "font_size_time" (int_of_float !font_size_time));

  (* pen dash *)
  let w0 = float_of_int (get_model_parameter_int options "pen_dash_on_length" 2) in
  let w1 = float_of_int (get_model_parameter_int options "pen_dash_off_length" 2) in
  pen_dash := [| w0; w1 |]
;;


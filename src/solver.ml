(*
  Copyright (C) 2018
  AIST Program-IP No. H30PRO-2219
  National Institute of Advanced Industrial Science and Technology (AIST),
  East Japan Railway Company (JR-East)
*)

open Model;;
open Utils;;
open Rsexpr;;

exception Unsat
exception Not_supported

            
type check_options = {
    dummy_opt : bool option;
    max_time_opt : int option;
    repeat_interval_opt : int option;
    parameter_map : model_parameter_value StringMap.t;
  };;

let default_options = {
    dummy_opt = None;
    max_time_opt = None;
    repeat_interval_opt = None;
    parameter_map = StringMap.empty;
  };;

let select_min n m = if (n < m) then n else m;;

let get_model_parameter (options:check_options) parameter_name =
  if StringMap.mem parameter_name options.parameter_map then
    Some (StringMap.find parameter_name options.parameter_map)
  else
    None
;;

let get_model_parameter_int options parameter_name default =
  match get_model_parameter options parameter_name with
    Some v ->
    (match v with
       ModelParameterInt n -> n
     | _ -> default)
  | None -> default
;;

let station_capacity m = 
  match m.capacity with
    Capacity c -> c
  | _ -> raise (RWError (Printf.sprintf "station %d has no capacity" m.id))
;;

(* module_id => module *)
let make_module_map module_list =
  List.fold_left
    (fun map (m:rw_module) ->
      if IntMap.mem m.id map then
        raise (RWError (Printf.sprintf "duplicate module id: %d" m.id))
      else
        IntMap.add m.id m map)
    IntMap.empty module_list
;;

(* train_id => train *)
let make_train_map train_list =
  List.fold_left
    (fun map (t:rw_train) ->
      if IntMap.mem t.id map then
        raise (RWError (Printf.sprintf "duplicate train id: %d" t.id))
      else
        IntMap.add t.id t map)
    IntMap.empty train_list
;;

(* (train_id, seq_id) => var_name *)
let var_name train_id seq_id =
  Printf.sprintf "x_%d_%d" train_id seq_id
;;

let platform_var_name train_id seq_id =
  Printf.sprintf "p_%d_%d" train_id seq_id
;;

(* link_id => module_id list *)
let make_map_link_to_modules module_list =
  List.fold_left
    (fun map (m:rw_module) ->
      List.fold_left
        (fun map link_id ->
          if IntMap.mem link_id map then
            let ms = IntMap.find link_id map in
            if List.exists (fun (m':rw_module) -> m.id = m'.id) ms then
              raise (RWError (Printf.sprintf "duplicate link_id %d at moudle %d" link_id m.id))
            else
              IntMap.add link_id (m::ms) map
          else
            IntMap.add link_id [m] map)
        map m.links)
    IntMap.empty module_list
;;

(* map: link_id => link_id list *)
let make_link_rel module_list =
  List.fold_left
    (fun map (m:rw_module) ->
      List.fold_left
        (fun map link_id ->
          let ls = intmap_lookup map link_id [] in
          IntMap.add link_id (union (=) ls (diff (=) m.links [link_id])) map)
        map m.links)
    IntMap.empty module_list
;;

(* seq_id -> route -> module

PRE: seq_id < length route - 1

normally the pair of link_ids (route[seq_id], route[seq_id + 1]) uniquely determines the module
to which the both links belong.

however, in the case of route[seq_id] = route[seq_id + 1],
it is needed to check the previous or the next link_id to determine the module:

route = [...; a; a; ...]

-x- [m0] -a- [m1] -y-

(i) seq_id > 0
route = [...; c; a; a; ...]
if c connects to m0 then m1 else m0
path = -c-> [m?] -a-> [m?] -a-> [m?]
                 ~~~~~~~~~~~~~~

(ii) seq_id < length route - 2
route = [...; a; a; c; ...]
path = -a-> [m?] -a-> [m?] -c-> [m?]
       ~~~~~~~~~~~~~~

the other cases are not supported.
*)
let find_module map_link_to_modules seq_id route =
  let a = List.nth route seq_id and
      b = List.nth route (seq_id + 1) in
  let ms = IntMap.find a map_link_to_modules in
  let ms = List.filter (fun m -> List.mem b m.links) ms in
  if List.length ms = 1 then
    List.hd ms
  else if a = b && List.length ms = 2 then
    let m0 = List.nth ms 0 and m1 = List.nth ms 1 in
    if seq_id > 0 then (* check previous *)
      let c = List.nth route (seq_id - 1) in
      if c <> a then
        if List.mem c m0.links then
          m1
        else
          m0
      else
        raise Not_supported
    else if seq_id < List.length route - 2 then (* check successor *)
      let c = List.nth route (seq_id + 2) in
      if c <> a then
        if List.mem c m0.links then
          m1
        else
          m0
      else
        raise Not_supported
    else
      raise Not_supported
  else
    raise Not_supported
;;

(* module_id -> (train_id, seq_id) list *)
let make_map_module_to_train_pos map_link_to_modules train_list =
  List.fold_left
    (fun map train ->
      fold_left_with_index
        (fun map seq_id _ ->
          let m = find_module map_link_to_modules seq_id train.route in
          let ps = intmap_lookup map m.id [] in
          IntMap.add m.id ((train.id, seq_id)::ps) map)
        map (butlast train.route))
    IntMap.empty train_list
;;

let get_train_infos map_module_to_trains module_id =
  try
    IntMap.find module_id map_module_to_trains
  with
    Not_found -> []
;;

(* vars *)
let make_declaration_time_vars train_list =
  List.fold_left
    (fun acc train ->
      List.append
        acc
        (map_with_index
           (fun seq_id _ ->
             List [Sym "declare-const"; Sym (var_name train.id seq_id); Sym "Int"])
           train.route))
    [] train_list
;;

let make_declaration_platform_vars map_link_to_modules train_list =
  List.fold_left
    (fun acc train ->
      fold_left_with_index
        (fun acc seq_id _ ->
(*        let m = find_module map_link_to_modules seq_id train.route in
          if m.station then  *)
          if true then 
            List [Sym "declare-const"; Sym (platform_var_name train.id seq_id); Sym "Int"]::acc
          else
            acc)
        acc (butlast train.route))
    [] train_list
;;

let make_declaration_vars map_link_to_modules train_list =
  let vs1 = make_declaration_time_vars train_list in
  if !Getopt.option_calc_platform_by_z3 then
    let vs2 = make_declaration_platform_vars map_link_to_modules train_list in
    List.append vs1 vs2
  else
    vs1
;;

(* 
  => assert list
*)
let make_init_vars train_list max_time_opt =
  List.fold_left
    (fun acc train ->
      let n = List.length train.route in
      let acc =
        match train.start_time with
          None -> acc
        | Some time ->
           (List [Sym "assert";
                  List [Sym "="; Sym (var_name train.id 0); Int time]])::acc
      in
      let acc =
        match train.end_time with
          None -> acc
        | Some time ->
           (List [Sym "assert";
                  List [Sym "<="; Sym (var_name train.id (n-1)); Int time]])::acc
      in
      match max_time_opt with
        None -> acc
      | Some time ->
         (List [Sym "assert";
                List [Sym "<="; Sym (var_name train.id (n-1)); Int time]])::acc
    )
    [] train_list
;;

(*
  non_collision
*)
let make_non_collision1 repeat_interval start0 end0 start1 end1 =
  List [Sym "and";
        List [Sym "=";
              List [Sym "div"; start0; Int repeat_interval];
              List [Sym "div"; end0; Int repeat_interval]];
        List [Sym "=";
              List [Sym "div"; start1; Int repeat_interval];
              List [Sym "div"; end1; Int repeat_interval]];
        List [Sym "or";
              List [Sym "<=";
                    List [Sym "mod"; end0; Int repeat_interval];
                    List [Sym "mod"; start1; Int repeat_interval]];
              List [Sym "<=";
                    List [Sym "mod"; end1; Int repeat_interval];
                    List [Sym "mod"; start0; Int repeat_interval]]]]
;;

let make_non_collision2 repeat_interval start0 end0 start1 end1 =
  List [Sym "and";
        List [Sym "or";
              List [Sym "and";
                    List [Sym "=";
                          List [Sym "+"; Int 1;
                                List [Sym "div"; start0; Int repeat_interval]];
                          List [Sym "div"; end0; Int repeat_interval]];
                    List [Sym "=";
                          List [Sym "div"; start1; Int repeat_interval];
                          List [Sym "div"; end1; Int repeat_interval]]];
              List [Sym "and";
                    List [Sym "=";
                          List [Sym "div"; start0; Int repeat_interval];
                          List [Sym "div"; end0; Int repeat_interval]];
                    List [Sym "=";
                          List [Sym "+"; Int 1;
                                List [Sym "div"; start1; Int repeat_interval]];
                          List [Sym "div"; end1; Int repeat_interval]]]];
        List [Sym "<=";
              List [Sym "mod"; end0; Int repeat_interval];
              List [Sym "mod"; start1; Int repeat_interval]];
        List [Sym "<=";
              List [Sym "mod"; end1; Int repeat_interval];
              List [Sym "mod"; start0; Int repeat_interval]]]
;;

let make_non_collision options start0 end0 start1 end1 =
  match options.repeat_interval_opt with
    None -> 
    List [Sym "or";
          List [Sym "<="; end0; start1];
          List [Sym "<="; end1; start0]]
  | Some repeat_interval ->
     List [Sym "or";
           make_non_collision1 repeat_interval start0 end0 start1 end1;
           make_non_collision2 repeat_interval start0 end0 start1 end1]
;;

(* non_overtaking *)
let make_non_overtaking1 repeat_interval start0 end0 start1 end1 =
  List [Sym "and";
        List [Sym "or";
              List [Sym "and";
                    List [Sym "=";
                          List [Sym "div"; start0; Int repeat_interval];
                          List [Sym "div"; end0; Int repeat_interval]];
                    List [Sym "=";
                          List [Sym "div"; start1; Int repeat_interval];
                          List [Sym "div"; end1; Int repeat_interval]]];
              List [Sym "and";
                    List [Sym "=";
                          List [Sym "+"; Int 1;
                                List [Sym "div"; start0; Int repeat_interval]];
                          List [Sym "div"; end0; Int repeat_interval]];
                    List [Sym "=";
                          List [Sym "+"; Int 1;
                                List [Sym "div"; start1; Int repeat_interval]];
                          List [Sym "div"; end1; Int repeat_interval]]]];
        List [Sym "<=";
              List [Sym "mod"; start0; Int repeat_interval];
              List [Sym "mod"; start1; Int repeat_interval]];
        List [Sym "<=";
              List [Sym "mod"; end0; Int repeat_interval];
              List [Sym "mod"; end1; Int repeat_interval]]]
;;

let make_non_overtaking2 repeat_interval start0 end0 start1 end1 =
  List [Sym "and";
        List [Sym "=";
              List [Sym "div"; start0; Int repeat_interval];
              List [Sym "div"; end0; Int repeat_interval]];
        List [Sym "=";
              List [Sym "+"; Int 1;
                    List [Sym "div"; start1; Int repeat_interval]];
              List [Sym "div"; end1; Int repeat_interval]];
        List [Sym "<=";
              List [Sym "mod"; start0; Int repeat_interval];
              List [Sym "mod"; start1; Int repeat_interval]]]
;;
        
let make_non_overtaking options start0 end0 start1 end1 =
  match options.repeat_interval_opt with
    None ->
    List [Sym "or";
          List [Sym "and";
                List [Sym "<="; start0; start1];
                List [Sym "<="; end0; end1]];
          List [Sym "and";
                List [Sym "<="; start1; start0];
                List [Sym "<="; end1; end0]]]
  | Some repeat_interval ->
     List [Sym "or";
           make_non_overtaking1 repeat_interval start0 end0 start1 end1;
           make_non_overtaking1 repeat_interval start1 end1 start0 end0;
           make_non_overtaking2 repeat_interval start0 end0 start1 end1;
           make_non_overtaking2 repeat_interval start1 end1 start0 end0]
;;

(* self overtaking *)
let make_non_self_overtaking options start0 end0 =
  match options.repeat_interval_opt with
    None ->
    List [Sym "and"]
  | Some repeat_interval ->
     List [Sym "or";
           List [Sym "=";
                 List [Sym "div"; start0; Int repeat_interval];
                 List [Sym "div"; end0; Int repeat_interval]];
           List [Sym "and";
                 List [Sym "=";
                       List [Sym "+"; Int 1;
                             List [Sym "div"; start0; Int repeat_interval]];
                       List [Sym "div"; end0; Int repeat_interval]];
                 List [Sym "<=";
                       List [Sym "mod"; end0; Int repeat_interval];
                       List [Sym "mod"; start0; Int repeat_interval]]]]
;;

let get_req_platforms train_list t s =
  let train = List.nth train_list t in
  if train.req_platforms = [] then
    []
  else
    List.nth train.req_platforms s
;;

let make_list_mem_expr varname int_list =
  match int_list with
    [] -> List [Sym "and"]
  | [n] -> List [Sym "="; Sym varname; Int n]
  | n::ns ->
     List.fold_left
       (fun expr n ->
         List [Sym "or"; expr; List [Sym "="; Sym varname; Int n]])
       (List [Sym "="; Sym varname; Int n]) ns
;;

(* capacity *)

let make_assertion_capacity_platform options station_module_list map_module_to_trains train_list =
  cons
    (Sym "and")
    (List
       (List.fold_left
          (fun acc (m:rw_module) ->
            let capacity = station_capacity m in
            let ps = get_train_infos map_module_to_trains m.id in
            List.fold_left
              (fun acc (t, s) ->
                 let platform = platform_var_name t s in
                 let req_platforms = get_req_platforms train_list t s in
                 if req_platforms = [] then
                   List [Sym "<="; Int 1; Sym platform]::
                     List [Sym "<="; Sym platform; Int capacity]::acc
                 else
                   (make_list_mem_expr platform req_platforms)::acc)
               acc ps)
          [] station_module_list))
;;

let make_assertion_capacity_station options module_list map_module_to_trains =
  cons
    (Sym "and")
    (List
       (List.fold_left
          (fun acc (m:rw_module) ->
            let ps = get_train_infos map_module_to_trains m.id in
            (cons
               (Sym "and")
               (List
                  (List.map
                     (fun ss ->
                       match ss with
                         [(t0, s0); (t1, s1)] ->
                         let platform0 = platform_var_name t0 s0 in
                         let platform1 = platform_var_name t1 s1 in
                         let enter_time0 = var_name t0 s0
                         and exit_time0 = var_name t0 (s0+1)
                         and enter_time1 = var_name t1 s1
                         and exit_time1 = var_name t1 (s1+1)
                         in
                         List [Sym "or";
                               List [Sym "not"; List [Sym "="; Sym platform0; Sym platform1]];
                               make_non_collision options
                                                  (Sym enter_time0) (Sym exit_time0)
                                                  (Sym enter_time1) (Sym exit_time1)]
                       | _ -> raise (RWError "make_assertion_capacity_station: invalid comb"))
                     (comb ps 2))))::acc)
          [] module_list))
;;

let make_assertion_capacity_module options module_list map_module_to_trains =
  cons
    (Sym "and")
    (List
       (List.fold_left
          (fun acc (m:rw_module) ->
            let ps = get_train_infos map_module_to_trains m.id in
            (match m.capacity with
               Capacity c ->
               let subs = comb ps (c + 1) in
               (* for all subsets of trains which size = capacity + 1 *)
               (cons
                  (Sym "and")
                  (List
                     (List.map
                        (fun sub ->
                          (* some pair in sub *)
                          cons
                            (Sym "or")
                            (List
                               (List.map
                                  (fun ss ->
                                    match ss with
                                      [(t0, s0); (t1, s1)] ->
                                      let enter_time0 = var_name t0 s0
                                      and exit_time0 = var_name t0 (s0+1)
                                      and enter_time1 = var_name t1 s1
                                      and exit_time1 = var_name t1 (s1+1)
                                      in
                                      make_non_collision options
                                                         (Sym enter_time0) (Sym exit_time0)
                                                         (Sym enter_time1) (Sym exit_time1)
                                    | _ -> raise (RWError "invalid subs"))
                                  (comb sub 2))))
                        subs)))::acc
             | _ -> acc))
          [] module_list))
;;

let make_assertion_self_overtaking options module_list map_module_to_trains =
  cons
    (Sym "and")
    (List
       (List.fold_left
          (fun acc (m:rw_module) ->
            let ps = get_train_infos map_module_to_trains m.id in
            (cons
               (Sym "and")
               (List
                  (List.map
                     (fun (t, s) ->
                       let enter_time = var_name t s
                       and exit_time = var_name t (s+1) in
                       make_non_self_overtaking options (Sym enter_time) (Sym exit_time))
                     ps)))::acc)
          [] module_list))
;;

let make_assertion_capacity options module_list map_module_to_trains train_list =
  if !Getopt.option_calc_platform_by_z3 then
    (* calculate platform No. at stations by Z3 *)
    (*
    let (station_module_list, interstation_module_list) =
      divide_list (fun (m:rw_module) -> m.station) module_list
    in
     *)
    List [Sym "and";
(*
          make_assertion_capacity_platform options station_module_list map_module_to_trains train_list;
          make_assertion_capacity_station options station_module_list map_module_to_trains;
          make_assertion_capacity_module options interstation_module_list map_module_to_trains; 
*)
          make_assertion_capacity_platform options module_list map_module_to_trains train_list;
          make_assertion_capacity_station options module_list map_module_to_trains]
         (*
          ;
          make_assertion_self_overtaking options module_list map_module_to_trains]
          *)
  else
    List [Sym "and";
          make_assertion_capacity_module options module_list map_module_to_trains;
          make_assertion_self_overtaking options module_list map_module_to_trains]
;;


(* interval *)
let make_assertion_interval options module_list train_map map_module_to_trains =
  let get_link_id t s =
    let train = IntMap.find t train_map in
    List.nth train.route s
  in
  (* for each module *)
  cons
    (Sym "and")
    (List
       (List.map
          (fun (m:rw_module) ->
            let ps =
              try
                IntMap.find m.id map_module_to_trains
              with
                Not_found -> []
            in
            (* for each link *)
            cons
              (Sym "and")
              (List
                 (List.map
                    (fun link_id ->
                      (* collect vars (train-pos) related to this link *)
                      let ps2 =
                        List.append
                          (List.filter (fun (t, s) -> link_id = get_link_id t s) ps)
                          (List.map (fun (t, s) -> (t, s+1))
                                    (List.filter (fun (t, s) -> link_id = get_link_id t (s+1)) ps)) in
                      (* for each pair of trains *)
                      cons
                        (Sym "and")
                        (List
                           (List.map
                              (fun ss ->
                                match ss with
                                  [(t0, s0); (t1, s1)] ->
                                  let tm0 = var_name t0 s0
                                  and tm1 = var_name t1 s1 in
                                  make_non_collision options
                                                     (Sym tm0) (List [Sym "+"; Sym tm0; Int (m.interval)])
                                                     (Sym tm1) (List [Sym "+"; Sym tm1; Int (m.interval)])
                                | _ -> raise (RWError "invalid subs"))
                              (comb ps2 2))))
                    m.links)))
          module_list))
;;

(* fifo *)
let make_assertion_fifo options module_list train_map map_module_to_trains =
  let get_link_id t s =
    let train = IntMap.find t train_map in
    List.nth train.route s
  in
  (* for each module *)
  cons
    (Sym "and")
    (List
       (List.map
          (fun (m:rw_module) ->
            let ps =
              try
                IntMap.find m.id map_module_to_trains
              with
                Not_found -> []
            in
            (* for each entry in fifos *)
            cons
              (Sym "and")
              (List
                 (List.map
                    (fun (link_in, link_out) ->
                      (* collect trains match to this link pair *)
                      let ps2 =
                        List.filter
                          (fun (t, s) ->
                             link_in = get_link_id t s &&
                             link_out = get_link_id t (s+1))
(*                              (link_in = get_link_id t s &&
                               link_out = get_link_id t (s+1)) ||
                              (link_out = get_link_id t s &&
                                 link_in = get_link_id t (s+1))) *)
                          ps in
                      (* for each pair of trains *)
                      cons
                        (Sym "and")
                        (List
                           (List.map
                              (fun ss ->
                                match ss with
                                  [(t0, s0); (t1, s1)] ->
                                  let enter_time0 = var_name t0 s0
                                  and exit_time0 = var_name t0 (s0+1)
                                  and enter_time1 = var_name t1 s1
                                  and exit_time1 = var_name t1 (s1+1) in
                                  make_non_overtaking options
                                                      (Sym enter_time0) (Sym exit_time0)
                                                      (Sym enter_time1) (Sym exit_time1)
                                | _ -> raise (RWError "invalid subs"))
                              (comb ps2 2))))
                    m.fifos)))
          module_list))
;;

(* singletracks *)
let make_assertion_singletracks options module_list train_map map_module_to_trains =
  let get_link_id t s =
    let train = IntMap.find t train_map in
    List.nth train.route s
  in
  (* for each module *)
  cons
    (Sym "and")
    (List
       (List.map
          (fun (m:rw_module) ->
            let ps =
              try
                IntMap.find m.id map_module_to_trains
              with
                Not_found -> []
            in
            (* for each entry in singletracks *)
            cons
              (Sym "and")
              (List
                 (List.map
                    (fun (link_in, link_out) ->
                      (* collect trains match to this link pair *)
                      let ps2 =
                        List.filter
                          (fun (t, s) ->
                            (link_in = get_link_id t s &&
                               link_out = get_link_id t (s+1)) ||
                              (link_out = get_link_id t s &&
                                 link_in = get_link_id t (s+1)))
                          ps in
                      (* for each pair of trains *)
                      cons
                        (Sym "and")
                        (List
                           (List.map
                              (fun ss ->
                                match ss with
                                  [(t0, s0); (t1, s1)] ->
                                  let enter_time0 = var_name t0 s0
                                  and exit_time0 = var_name t0 (s0+1)
                                  and enter_time1 = var_name t1 s1
                                  and exit_time1 = var_name t1 (s1+1) in
                                  List [Sym "or";
                                        List [Sym "="; Int (get_link_id t0 s0); Int (get_link_id t1 s1)];
                                        make_non_collision options
                                                           (Sym enter_time0) (Sym exit_time0)
                                                           (Sym enter_time1) (Sym exit_time1)]
                                | _ -> raise (RWError "invalid subs"))
                              (comb ps2 2))))
                    m.singletracks)))
          module_list))
;;

(* req_time *)
let make_assertion_stop_time options module_list train_map map_module_to_trains =
  (* for each module *)
  cons
    (Sym "and")
    (List
       (List.map
          (fun (m:rw_module) ->
            (* for each train *)
            cons
              (Sym "and")
              (List
                 (List.map
                    (fun (t, s) ->
                      let enter_time = var_name t s
                      and exit_time = var_name t (s+1)
                      and (mn, mx_opt) = (* req_time *)
                        let train = IntMap.find t train_map in
                        let req_times =
                          match train.req_times with
                            None -> raise (RWError "default req_time is not set properly")
                          | Some rt -> rt in
                        match List.nth req_times s with
                          None -> raise (RWError "default req_time is not set properly")
                        | Some (mn, mx_opt) -> (mn, mx_opt)
                      in
                      match mx_opt with
                        None ->
                         (match options.repeat_interval_opt with  (* stay-time must be less than RI  *)
                           None ->
                           List [Sym "<="; List [Sym "+"; Sym enter_time; Int mn]; Sym exit_time]
                          | Some repeat_interval ->
                            List [Sym "and";
                               List [Sym "<="; List [Sym "+"; Sym enter_time; Int mn]; Sym exit_time];
                               List [Sym "<="; Sym exit_time; List [Sym "+"; Sym enter_time; Int repeat_interval]]])

                      | Some mx ->
                         (match options.repeat_interval_opt with  (* stay-time must be less than RI  *)
                           None ->
                           List [Sym "and";
                               List [Sym "<="; List [Sym "+"; Sym enter_time; Int mn]; Sym exit_time];
                               List [Sym "<="; Sym exit_time; List [Sym "+"; Sym enter_time; Int mx]]]
                         | Some repeat_interval ->
                            let mx2 = select_min mx repeat_interval in
                            List [Sym "and";
                               List [Sym "<="; List [Sym "+"; Sym enter_time; Int mn]; Sym exit_time];
                               List [Sym "<="; Sym exit_time; List [Sym "+"; Sym enter_time; Int mx2]]]))
                  
                    (try
                       IntMap.find m.id map_module_to_trains
                     with
                       Not_found -> []))))
          module_list))
;;

  
let make_assertion_start_time_between train_list =
  cons
    (Sym "and")
    (List
       (List.fold_left
          (fun acc train ->
            match train.start_time_between with
              None -> acc
            | Some (a, b) ->
               (List [Sym "and";
                      List [Sym ">="; Sym (var_name train.id 0); Int a];
                      List [Sym "<="; Sym (var_name train.id 0); Int b]])::acc)
          [] train_list))
;;

let make_assertion_max_total_time train_list =
  cons
    (Sym "and")
    (List
       (List.fold_left
          (fun acc train ->
            let n = List.length train.route in
            match train.max_total_time with
              None -> acc
            | Some time ->
               (List [Sym "<=";
                      List [Sym "-";
                            Sym (var_name train.id (n-1));
                            Sym (var_name train.id 0)];
                      Int time])::acc)
          [] train_list))
;;

let make_assertion_max_total_time_untils train_list =
  cons
    (Sym "and")
    (List
       (List.fold_left
          (fun acc train ->
            (List.fold_left
               (fun acc (s, m) ->
                 (List [Sym "<=";
                        List [Sym "-";
                              Sym (var_name train.id s);
                              Sym (var_name train.id 0)];
                        Int m])::acc)
               acc train.max_total_time_untils))
          [] train_list))
;;

let rec simplify_assertion x =
  match x with
    List [Sym "not"; y] ->
    let y' = simplify_assertion y in
    (match y' with
       Sym "true" -> Sym "false"
     | Sym "false" -> Sym "true"
     | _ -> List [Sym "not"; y'])
  | List [Sym "and"] -> Sym "true"
  | List ((Sym "and")::xs) ->
     let xs' = List.map simplify_assertion xs in
     let xs2 = List.filter
                 (fun x ->
                   match x with
                     Sym "true" -> false
                   | _ -> true)
                 xs' in
     if xs2 = [] then
       Sym "true"
     else
       List ((Sym "and")::xs2)
  | List [Sym "or"] -> Sym "false"
  | List ((Sym "or")::xs) -> List ((Sym "or")::(List.map simplify_assertion xs))
  | _ -> x
;;

let make_z3_model module_list train_list options map_link_to_modules train_map map_module_to_trains =
  let declaration_vars = make_declaration_vars map_link_to_modules train_list in
  let init_vars = make_init_vars train_list options.max_time_opt in
  let assertion_capacity =
    make_assertion_capacity options module_list map_module_to_trains train_list
  and assertion_interval =
    make_assertion_interval options module_list train_map map_module_to_trains
  and assertion_fifo =
    make_assertion_fifo options module_list train_map map_module_to_trains
  and assertion_singletracks =
    make_assertion_singletracks options module_list train_map map_module_to_trains
  and assertion_stop_time =
    make_assertion_stop_time options module_list train_map map_module_to_trains
  and assertion_start_time_between =
    make_assertion_start_time_between train_list
  and assertion_max_total_time =
    make_assertion_max_total_time train_list
  and assertion_max_total_time_untils =
    make_assertion_max_total_time_untils train_list
  in
  let assertion =
    List [Sym "assert";
          simplify_assertion
            (cons (Sym "and")
                  (List [assertion_capacity;
                         assertion_interval;
                         assertion_fifo;
                         assertion_singletracks;
                         assertion_stop_time;
                         assertion_start_time_between;
                         assertion_max_total_time;
                         assertion_max_total_time_untils]))] in
  List.concat
    [declaration_vars;
     init_vars;
     [assertion;
      List [Sym "check-sat"];
      List [Sym "get-model"]]]
;;

(*
=> rsexpr
sat
(model 
  (define-fun x_1_2 () Int
    20)
  (define-fun x_0_2 () Int
    80)
  (define-fun x_1_1 () Int
    0)
  (define-fun x_1_0 () Int
    (- 60))
  (define-fun x_0_1 () Int
    60)
  (define-fun x_0_0 () Int
    0)
)
*)
let retrieve_z3_result fn =
  let uch = Ungetch.open_in fn in
  let res = read_rsexpr uch in
  if res = Sym "sat" then
    let model = read_rsexpr uch in
    (Ungetch.close_in uch; model)
  else
    (Ungetch.close_in uch; raise Unsat)
;;

let split cs =
  let rec f rs xs = function
      [] -> List.rev ((List.rev xs)::rs)
    | c::cs ->
       if is_digit c then
         f rs (c::xs) cs
       else
         if xs == [] then
           f rs [] cs
         else
           f ((List.rev xs)::rs) [] cs
  in
  f [] [] cs
;;

let get_train_id_n_seq_id vname =
  match List.map int_of_string (List.map char_list_to_string (split (string_to_char_list vname))) with
    [train_id; seq_id] -> (train_id, seq_id)
  | _ -> raise Invalid_rsexpr
;;

let explode_vname vname = 
  let re = Str.regexp "\\(p\\|x\\)_\\([0-9]+\\)_\\([0-9]+\\)" in
  if Str.string_match re vname 0 then
    let tag = Str.matched_group 1 vname in
    let train_id = int_of_string (Str.matched_group 2 vname) in
    let seq_id = int_of_string (Str.matched_group 3 vname) in
    (tag, train_id, seq_id)
  else
    raise (RWError (Printf.sprintf "unknown variable: %s" vname))
;;

(*
  result rsexpr => ((train_id, seq_id, time) list,  (train_id, seq_id, platform) list)

  (define-fun x_1_2 () Int 20)
  (define-fun x_1_2 () Int (- 20))
*)
let analyze_model model =
  match model with
    List (Sym "model"::xs) ->
    let rec g ts ps = function
        [] -> (List.rev ts, List.rev ps)
      | x::xs ->
         let (vname, value) =
           match x with
             List [Sym "define-fun"; Sym vname; List []; Sym "Int"; Int value] ->
             (vname, value)
           | List [Sym "define-fun"; Sym vname; List []; Sym "Int";
                   List [Sym "-";  Int value]] ->
              (vname, -value)
           | _ -> raise Invalid_rsexpr in
         let (tag, train_id, seq_id) = explode_vname vname in
         if tag = "x" then
           g ((train_id, seq_id, value)::ts) ps xs
         else
           g ts ((train_id, seq_id, value)::ps) xs
    in
    g [] [] xs
  | _ -> raise Invalid_rsexpr
;;

(*
  (train_id, seq_id, time) list
  =>
  map: train_id => time list
*)
let classify_result_time rs =
  let map =
    List.fold_left
      (fun map (train_id, seq_id, time) ->
        IntMap.add train_id
                   ((seq_id, time)::(intmap_lookup map train_id []))
                   map)
      IntMap.empty rs in
  (* sort by seq_id and distill times *)
  IntMap.fold
    (fun train_id ps map ->
      let ts = List.map snd (List.sort (fun (s0, v0) (s1, v1) -> s0 - s1) ps) in
      IntMap.add train_id ts map)
    map IntMap.empty
;;

(*
  (train_id, seq_id, platform)
  =>
  map: train_id => map: seq_id => platform
*)
let classify_result_platform ps =
  List.fold_left
    (fun platform_map (train_id, seq_id, platform) ->
      IntMap.add train_id
                 (IntMap.add seq_id
                             (platform - 1)
                             (intmap_lookup platform_map train_id IntMap.empty))
                 platform_map)
    IntMap.empty ps
;;

let execute_z3 rsexpr_list =
  let z3fn =
    if !Getopt.option_leave_z3_files = "" then
      Filename.temp_file ~temp_dir:"./" "tmp-rws-" "z3in"
    else
      Printf.sprintf "%s.z3in" !Getopt.option_leave_z3_files in
  let z3outfn =
    if !Getopt.option_leave_z3_files = "" then
      Filename.temp_file ~temp_dir:"./" "tmp-rws-" "z3out"
    else
      Printf.sprintf "%s.z3out" !Getopt.option_leave_z3_files in

  let fn =
    if !Getopt.option_reuse_z3_files = "" then
      let ch = open_out z3fn in
      List.iter (write_rsexpr ch) rsexpr_list;
      close_out ch;
      let cmdline =
        if !Getopt.option_smt_solver = "z3" then
          Printf.sprintf "z3 -smt2 %s > %s" z3fn z3outfn
        else if !Getopt.option_smt_solver = "cvc4" then
          Printf.sprintf "cvc4 --lang smt2 --output-lang smt2 --rewrite-divk --produce-models %s > %s" z3fn z3outfn
        else
          raise (RWError ("unknown SMT solver: " ^ !Getopt.option_smt_solver))
      in
      let z3_start_time = Unix.gettimeofday () in
      let _ = Sys.command cmdline in
      let z3_end_time = Unix.gettimeofday () in
      Printf.printf "%s execution time = %f sec\n"
                    !Getopt.option_smt_solver
                    (z3_end_time -. z3_start_time);
      z3outfn
    else
      !Getopt.option_reuse_z3_files
  in
  let model = retrieve_z3_result fn in
  (if !Getopt.option_leave_z3_files = "" && !Getopt.option_reuse_z3_files = "" then
     (Sys.remove z3fn;
      Sys.remove z3outfn));
  let (ts, ps) = analyze_model model in
  let train_time_map = classify_result_time ts in
  let platform_map = classify_result_platform ps in
  (train_time_map, platform_map)
;;

(**********************************************************************
   consistency check
 *)

let check_model_consistency_module module_list =
  List.iter
    (fun (m:rw_module) ->
      if m.links = [] then
        raise (RWError (Printf.sprintf "module %d has no links" m.id));
      if m.capacity = CapacityNone || (m.station && m.capacity = CapacityInfinity) then
        raise (RWError (Printf.sprintf "module %d has no capacity" m.id)))
    module_list
;;

let check_model_consistency_module_link_pairs module_list link_rel asor name =
  List.iter
    (fun (m:rw_module) ->
      List.iter
        (fun (p, q) ->
          if not (IntMap.mem p link_rel) then
            raise (RWError (Printf.sprintf "no link_id %d specified in %s at module %d" p name m.id));
          if not (IntMap.mem q link_rel) then
            raise (RWError (Printf.sprintf "no link_id %d specified in %s at module %d" q name m.id)))
        (asor m))
    module_list
;;

let check_model_consistency_module_fifos module_list link_rel =
  check_model_consistency_module_link_pairs
    module_list link_rel
    (fun (m:rw_module) -> m.fifos)
    "fifos"
;;

let check_model_consistency_module_singletracks module_list link_rel =
  check_model_consistency_module_link_pairs
    module_list link_rel
    (fun (m:rw_module) -> m.singletracks)
    "singletracks"
;;

let check_model_consistency_module_links map_link_to_modules =
  IntMap.iter
    (fun link_id ms ->
      let n = List.length ms in
      if n > 2 then
        raise (RWError (Printf.sprintf "link_id %d appears more than 2 times at modules %s"
                                       link_id
                                       (print_string_list print_string_int
                                                          (List.map (fun (m:rw_module) -> m.id) ms)))))
    map_link_to_modules
;;

let check_model_consistency_modules module_list train_list map_link_to_modules link_rel =
  check_model_consistency_module module_list;
  check_model_consistency_module_fifos module_list link_rel;
  check_model_consistency_module_singletracks module_list link_rel;
  if not !Getopt.option_suppress_consistency_module_links then
    check_model_consistency_module_links map_link_to_modules;
;;

let check_model_consistency_train train_list =
  List.iter
    (fun train ->
      match train.start_time with
        None -> ()
      | Some st ->
         (match train.end_time with
            None -> ()
          | Some et ->
             if st > et then
               raise (RWError (Printf.sprintf "start_time %d > end_time %d in train %d" st et train.id))))
    train_list
;;

let check_model_consistency_train_route train_list link_rel =
  List.iter
    (fun train ->
      (* check every link_id in route is in link_rel *)
      List.iter
        (fun link_id ->
          if not (IntMap.mem link_id link_rel) then 
            raise (RWError (Printf.sprintf "no link_id %d specified in the route of the train %d" link_id train.id)))
        train.route;
      (* check every pair of link_ids in route is connected *)
      List.iter
        (fun seq_id ->
          let link_id0 = List.nth train.route seq_id
          and link_id1 = List.nth train.route (seq_id + 1) in
          let ls = IntMap.find link_id0 link_rel in
          if link_id0 <> link_id1 && not (List.mem link_id1 ls) then
            raise (RWError (Printf.sprintf "no path (%d, %d) specified in the train %d" link_id0 link_id1 train.id)))
        (interval 0 ((List.length train.route) - 1)))
    train_list
;;

let check_model_consistency_train_req_times train_list =
  List.iter
    (fun train ->
      let req_times =
        match train.req_times with
          None -> raise (RWError "default req_times is not set properly")
        | Some rt -> rt in
      if List.length req_times + 1 <> List.length train.route then
        raise (RWError (Printf.sprintf "length of req_times differs from length of route - 1 in train %d" train.id));
      List.iter
        (fun req_time ->
          match req_time with
            None -> ()
          | Some (mn, mx_opt) ->
             (match mx_opt with
                None -> ()
              | Some mx ->
                 if mn > mx then
                   raise (RWError (Printf.sprintf "min %d > max %d in the req_times of the train %d" mn mx train.id))))
        (match train.req_times with
           None -> raise (RWError "default req_times is not set")
         | Some rt -> rt))
    train_list
;;

let check_model_consistency_train_req_platforms module_list train_list map_link_to_modules =
  List.iter
    (fun train ->
      if train.req_platforms <> [] &&
           List.length train.req_platforms + 1 <> List.length train.route then
        raise (RWError
                 (Printf.sprintf
                    "length of req_platforms differs from length of route - 1 in train %d"
                    train.id));
      List.iter
        (fun seq_id ->
          let m = find_module map_link_to_modules seq_id train.route in
          match m.capacity with
            Capacity cap ->
            let platform_no_list = List.nth train.req_platforms seq_id in
            if List.exists
                 (fun platform_no -> platform_no <= 0 || platform_no > cap)
                 platform_no_list
            then
              raise (RWError
                       (Printf.sprintf
                          "train %d: req_platforms %d: out of capacity"
                          train.id seq_id))
            | _ -> ())
        (interval 0 (List.length train.req_platforms)))
    train_list
;;

let check_model_consistency_trains module_list train_list map_link_to_modules link_rel =
  check_model_consistency_train train_list;
  check_model_consistency_train_route train_list link_rel;
  check_model_consistency_train_req_times train_list;
  check_model_consistency_train_req_platforms module_list train_list map_link_to_modules;
;;

let check_model_consistency module_list train_list map_link_to_modules link_rel =
  check_model_consistency_modules module_list train_list map_link_to_modules link_rel;
  check_model_consistency_trains module_list train_list map_link_to_modules link_rel
;;

(*
  find min and max time
  train_time_map : map: train_id => time list (seq_id order)
  => (min_time, max_time)
*)
let find_min_n_max_time train_time_map =
  if IntMap.is_empty train_time_map then
    raise (RWError "find_min_n_max_time: train_time_map is empty")
  else
    let (train_id, ts) = IntMap.choose  train_time_map in
    if ts = [] then
      raise (RWError "find_min_n_max_time: null time_list found")
    else
      let mn = List.nth ts 0
      and mx = List.nth ts (List.length ts - 1) in
      IntMap.fold
        (fun train_id ts (mn, mx) ->
          if ts = [] then
            raise (RWError "find_min_n_max_time: null time_list found")
          else
            let mn2 = List.nth ts 0
            and mx2 = List.nth ts (List.length ts - 1) in
            (min mn mn2, max mx mx2))
        train_time_map (mn, mx)
;;

(* set defaults to req_times *)
let patch_default_req_times map_link_to_modules train_list =
  List.map
    (fun train ->
      let req_times' =
        map_with_index
          (fun seq_id req_time ->
            match req_time with
              None ->
              let m = find_module map_link_to_modules seq_id train.route in
              (match m.req_time with
                 None ->
                 raise (RWError
                          (Printf.sprintf
                             "train %d: %d-th req_time (module %d) is not specified"
                             train.id seq_id m.id))
               | Some r -> Some r)
            | Some r -> Some r)
          (match train.req_times with
             None -> List.map (fun k -> None) (interval 0 ((List.length train.route) - 1))
           | Some rt -> rt) in
      { train with req_times = Some req_times' })
    train_list
;;

(* req_time (mn, mx) -> (mn, None) *)
let suppress_req_times_max map_link_to_modules train_list =
  List.map
    (fun train ->
      let req_times' =
        List.map
          (fun req_time ->
            match req_time with
              None -> None
            | Some (mn, mx) -> Some (mn, None))
          (match train.req_times with
             None -> raise (RWError "invalid req_times")
           | Some rt -> rt)
      in
      { train with req_times = Some req_times' })
    train_list
;;

let arrange_req_times map_link_to_modules train_list =
  let train_list' = patch_default_req_times map_link_to_modules train_list in
  if !Getopt.option_ignore_req_time_max then
    suppress_req_times_max map_link_to_modules train_list'
  else
    train_list'
;;

let process_model module_list train_list options =
  (* make internal data structures *)
  let map_link_to_modules = make_map_link_to_modules module_list in
  let train_list = arrange_req_times map_link_to_modules train_list in
  let train_map = make_train_map train_list in
  let link_rel = make_link_rel module_list in
  (*
  IntMap.iter
    (fun link_id ls ->
      Printf.printf "%d -> %s\n" link_id (print_string_list print_string_int ls))
    link_rel;
   *)
  (* consistency check *)
  check_model_consistency module_list train_list map_link_to_modules link_rel;
  let map_module_to_trains = make_map_module_to_train_pos map_link_to_modules train_list in
  (* make assertions *)
  let rsexpr_list = make_z3_model module_list train_list options map_link_to_modules train_map map_module_to_trains in
  (* exec z3 *)
  let (train_time_map, platform_map) = execute_z3 rsexpr_list in
  let (min_time, max_time) = find_min_n_max_time train_time_map in
  Printf.printf "min_time = %d\nmax_time = %d\n" min_time max_time;
  (train_map, map_link_to_modules, map_module_to_trains, train_time_map, platform_map, min_time, max_time)
;;

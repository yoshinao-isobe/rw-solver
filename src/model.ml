(*
  Copyright (C) 2018
  AIST Program-IP No. H30PRO-2219
  National Institute of Advanced Industrial Science and Technology (AIST),
  East Japan Railway Company (JR-East)
*)

exception RWError of string

type capacity_spec = CapacityNone | CapacityInfinity | Capacity of int

type rw_module = {
    id : int;
    capacity : capacity_spec;
    interval : int;
    links : int list;                (* link-id list*)
    fifos : (int * int) list;        (* list of pairs of link-ids *)
    singletracks : (int * int) list;      (* list of pairs of link-ids *)
    station : bool;
    name : string option;
    req_time : (int * int option) option
  }

type rw_train = {
    id : int;
    route : int list;                              (* link-id list *)
    req_times : (int * int option) option list option;    (* req_time list, (min, max), #route = #req_time + 1 *)
    req_platforms : int list list;
    start_time : int option;
    end_time : int option;
    start_time_between : (int * int) option;
    max_total_time : int option;
    max_total_time_untils : (int * int) list;
}

(*
type model_element = Module of rw_module | Train of rw_train | MaxTime of int | RepeatInterval of int
 *)

type model_parameter_value = ModelParameterInt of int | ModelParameterString of string;;

module StringMap =
  Map.Make (struct
             type t = string
             let compare = String.compare
           end);;

type model_element = {
    module_list : rw_module list;
    train_list : rw_train list;
    max_time_opt : int option;
    repeat_interval_opt : int option;
    parameter_map : model_parameter_value StringMap.t;
}

let default_module = {
    id = -1;
    capacity = CapacityNone;
    interval = 0;
    links = [];
    fifos = [];
    singletracks = [];
    station = false;
    name = None;
    req_time = None;
  }

let default_train = {
    id = -1;
    route = [];
    req_times = None;
    req_platforms = [];
    start_time = None;
    end_time = None;
    start_time_between = None;
    max_total_time = None;
    max_total_time_untils = [];
  }

let default_model_element = {
    module_list = [];
    train_list = [];
    max_time_opt = None;
    repeat_interval_opt = None;
    parameter_map = StringMap.empty;
}

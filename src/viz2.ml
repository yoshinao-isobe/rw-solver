(*
  Copyright (C) 2018
  AIST Program-IP No. H30PRO-2219
  National Institute of Advanced Industrial Science and Technology (AIST),
  East Japan Railway Company (JR-East)
*)

open Model
open Utils
open Solver
open Viz

let select_color k =
  List.nth color_list (k mod (List.length color_list))
;;

let select_module_color cr m =
  if m.station then
    if m.id mod 2 = 0 then
      Cairo.set_source_rgb cr 0.0 0.98 0.98
    else
      Cairo.set_source_rgb cr 0.1 1.0 0.1
  else
    if m.id mod 2 = 0 then
      Cairo.set_source_rgb cr 0.95 0.95 0.95
    else
      Cairo.set_source_rgb cr 0.97 0.97 0.97
;;

let module_title (m:rw_module) =
  match m.name with
    None ->
    if m.station then
      Printf.sprintf "S%d" m.id
    else
      Printf.sprintf "M%d" m.id
  | Some name -> name
;;

let calc_max_title_width module_list surface =
  let cr = Cairo.create surface in
  Cairo.select_font_face cr "Consolas" Cairo.FONT_SLANT_NORMAL Cairo.FONT_WEIGHT_NORMAL;
  Cairo.set_font_size cr !font_size_station;
  (* calc title width *)
  let max_title_width =
    List.fold_left
      (fun mx (m:rw_module) ->
        let title = module_title m in
        let ext = Cairo.text_extents cr title in
        max mx ext.Cairo.text_width)
      0.0 module_list in
  Cairo.show_page cr;
  Cairo.surface_flush surface;
  Cairo.surface_finish surface;
  max_title_width
;;

let floor_div x y = if x >= 0 then x / y else x / y - 1;;
    
let draw_train_lines cr train_list train_time_map map_link_to_modules mm pm ox oy min_time xscale
                     repeat_flag repeat_interval k_th_interval ofs repeat_count =
  List.iter
    (fun train ->
      List.iter
        (fun seq_id ->
          let time_list =
            try IntMap.find train.id train_time_map with
              _ -> raise (RWError "time not found")
          in
          let enter_time = List.nth time_list seq_id in
          let exit_time = List.nth time_list (seq_id + 1) in
          let x0 = ox +. float_of_int (enter_time - min_time) *. xscale in
          let x1 = ox +. float_of_int (exit_time - min_time) *. xscale in
          let m = find_module map_link_to_modules seq_id train.route in
          let (y, h) = IntMap.find m.id mm in

          let (r, g, b) = select_color train.id in
          Cairo.set_source_rgb cr r g b;

          let (y0, y1, pw) =
            if m.station then
              let platform = IntMap.find seq_id (IntMap.find train.id pm) in
              let dy = !train_top_margin +. (float_of_int platform) *. !train_offset in
              (oy +. y +. dy, oy +. y +. dy, !pen_width_station)
            else
              if seq_id > 0 then
                let m' = find_module map_link_to_modules (seq_id-1) train.route in
                if m'.id < m.id then
                  (oy +. y, oy +. y +. h, !pen_width_rail)
                else
                  (oy +. y +. h, oy +. y, !pen_width_rail)
              else if seq_id < (List.length train.route) - 2 then
                let m' = find_module map_link_to_modules (seq_id+1) train.route in
                if m.id < m'.id then
                  (oy +. y, oy +. y +. h, !pen_width_rail)
                else
                  (oy +. y +. h, oy +. y, !pen_width_rail)
              else
                (oy +. y +. h *. 0.5, oy +. y +. h *. 0.5, !pen_width_rail) in
          (* next module *)
          (if seq_id < (List.length train.route) - 2 then
             let m' = find_module map_link_to_modules (seq_id+1) train.route in
             let (y', h') = IntMap.find m'.id mm in
             let y2opt =
               if m'.station then
                 let platform' = IntMap.find (seq_id+1) (IntMap.find train.id pm) in
                 let dy' = !train_top_margin +. (float_of_int platform') *. !train_offset in
                 Some (oy +. y' +. dy')
               else
                 if m.id + 1 < m'.id then
                   Some (oy +. y')
                 else if m.id - 1 > m'.id then
                   Some (oy +. y' +. h')
                 else if m.station && m.id < m'.id then
                   Some (oy +. y')
                 else if m.station && m.id > m'.id then
                   Some (oy +. y' +. h')
                 else
                   None in
             match y2opt with
               None -> ()
             | Some y2 ->
                Cairo.set_dash cr !pen_dash 0.0;
                Cairo.set_line_width cr !pen_width_dash;
                Cairo.move_to cr x1 y1;
                Cairo.line_to cr x1 y2;
                Cairo.stroke cr;
                Cairo.set_dash cr [||] 0.0
          );

          (* print the slot-number of this line *)
          (* slot_offset *)
          
          if ((not m.station) && (!Getopt.option_calc_platform_by_z3) && (!Getopt.option_prt_slot_no)) then
            (let slot_offset = 20.0 in
             let xos = (if (y1 < y0) then (-1.0 *. slot_offset -. !font_size_time) else slot_offset) in
              Cairo.move_to cr (((x0 +. x1)/. 2.0)+. xos) ((y0 +. y1)/. 2.0);
              let linenum = IntMap.find seq_id (IntMap.find train.id pm) in
              let ln = string_of_int (linenum+1) in
              Cairo.show_text cr ln);
          
          Cairo.set_line_width cr pw;
          Cairo.move_to cr x0 y0;
          Cairo.line_to cr x1 y1;
          Cairo.stroke cr;
          Cairo.arc cr x0 y0 !circle_raidus 0.0 6.283;
          Cairo.fill cr;
          Cairo.set_source_rgb cr 1.0 1.0 1.0;
          Cairo.arc cr x1 y1 !circle_raidus 0.0 6.283;
          Cairo.fill cr;
          Cairo.set_source_rgb cr r g b;
          Cairo.set_line_width cr !pen_width_rail;
          Cairo.arc cr x1 y1 !circle_raidus 0.0 6.283;
          Cairo.stroke cr;

          let (nt, xt) =
            if repeat_flag then
              (repeat_interval * k_th_interval
               + (enter_time - min_time) mod repeat_interval + min_time,
               repeat_interval * k_th_interval
               + (exit_time - min_time) mod repeat_interval + min_time)
            else
              (enter_time, exit_time) in
          let s0 = Printf.sprintf "%d" nt in
          let s1 = Printf.sprintf "%d" xt in
(*          Cairo.set_source_rgb cr 0.0 0.0 0.0; *)
          let ext0 = Cairo.text_extents cr s0 in
          let tw0 = ext0.Cairo.text_width in
          if y0 = y1 then
            Cairo.move_to cr (x0 -. tw0 -. !font_size_time *. 1.0) (y0 +. !font_size_time *. 0.3)
          else if y0 < y1 then
            Cairo.move_to cr (x0 -. tw0 -. !font_size_time *. 0.5) (y0 +. !font_size_time *. 1.2)
          else
            Cairo.move_to cr (x0 -. tw0 -. !font_size_time *. 0.5) (y0 -. !font_size_time *. 0.8);
          if not repeat_flag ||
               (floor_div (enter_time - min_time) repeat_interval) - repeat_count + ofs + 1 = k_th_interval then
            Cairo.show_text cr s0;
          Cairo.move_to cr (x1 +. !font_size_time *. 1.4) (y1 +. !font_size_time *. 0.3);
          if not repeat_flag ||
               (floor_div (exit_time - min_time) repeat_interval) - repeat_count + ofs + 1 = k_th_interval then
            Cairo.show_text cr s1;
        (*            Cairo.show_text cr "AAA"; *)
        )
        (interval 0 ((List.length train.route) - 1)))
    train_list
;;

let draw_repeat_interval_borders cr ox oy total_height xscale repeat_interval repeat_count =
  let rri = (float_of_int repeat_interval) *. xscale in
  Cairo.set_source_rgb cr 1.0 0.65 0.0;
  for i = 0 to repeat_count - 1 do
    Cairo.set_dash cr !pen_dash 0.0;
    Cairo.set_line_width cr !pen_width_dash;
    Cairo.move_to cr (ox +. rri *. float_of_int (i + 1)) oy;
    Cairo.line_to cr (ox +. rri *. float_of_int (i + 1)) (oy +. total_height);
    Cairo.stroke cr;
    Cairo.set_dash cr [||] 0.0;
  done
;;

let draw surface mm ox oy module_width module_list train_time_map min_time
         xscale yscale map_link_to_modules pm train_list options total_height
         repeat_flag repeat_interval repeat_count =
  let cr = Cairo.create surface in
  Cairo.select_font_face cr "Consolas" Cairo.FONT_SLANT_NORMAL Cairo.FONT_WEIGHT_NORMAL;
  Cairo.set_font_size cr !font_size_station;

  (* fill module background *)
  let w = match options.repeat_interval_opt with
         None -> module_width
       | Some repeat_interval -> (float_of_int (repeat_interval * repeat_count)) *. xscale in
  List.iter
    (fun (m:rw_module) ->
      let (y, h) = IntMap.find m.id mm in
      Cairo.rectangle cr ox (oy +. y) w h;
      select_module_color cr m;
      Cairo.fill cr;
      (* module title *)
      let title =
        match m.name with
          None -> Printf.sprintf "M%d" m.id
         | Some name -> name
      in
      let ext = Cairo.text_extents cr title in
      let yt = oy +. y +. 0.5 *. (h +. ext.Cairo.text_height) in
      Cairo.set_source_rgb cr 0.0 0.0 0.0;
      Cairo.move_to cr !left_margin yt;
      Cairo.show_text cr title
    )
    module_list;

  Cairo.set_font_size cr !font_size_time;

  (* train *)
  (if not repeat_flag then
     (draw_train_lines cr train_list train_time_map map_link_to_modules mm pm ox oy min_time xscale
                       repeat_flag repeat_interval 0 0 1;
      (match options.repeat_interval_opt with
         None -> ()
       | Some repeat_interval ->
          draw_repeat_interval_borders cr ox oy total_height xscale repeat_interval repeat_count
     ))
   else
     (let rri = (float_of_int repeat_interval) *. xscale in
      for i = 0 to repeat_count - 1 do
        Cairo.rectangle cr (ox +. rri *. (float_of_int i)) oy rri total_height;
         Cairo.clip cr;
        for j = 0 to repeat_count * 2 do
          draw_train_lines cr train_list train_time_map map_link_to_modules mm pm
                           (ox +. (float_of_int (j - repeat_count + 1)) *. rri) oy min_time xscale
                           repeat_flag repeat_interval i j repeat_count
        done;
        Cairo.reset_clip cr
      done;
      draw_repeat_interval_borders cr ox oy total_height xscale repeat_interval repeat_count
  ));

  Cairo.show_page cr;
  Cairo.surface_flush surface;
  Cairo.surface_finish surface
;;

(*
  module_list : rw_module list
  railway_min_dist_list : int list (only for moudles which station flag is off)
  train_time_list : (module_id, enter_time, exit_time, platform) list
*)
let draw_diagram base_name module_list train_list train_map map_link_to_modules map_module_to_trains
                 train_time_map platform_map min_time max_time options
                 xscale yscale =
  set_viz_parameters options;
  let map_line_module_to_width = seek_min_req_time module_list train_map map_module_to_trains in
  (* mm : map: module_id => (pos, height) *)
  let (total_height, mm) = calc_module_height_n_diagram_height module_list map_line_module_to_width yscale in
  let module_width = (float_of_int (max_time - min_time)) *. xscale in

  (* calc repeat count from module_width and repeat_interval *)
  let (repeat_interval, repeat_count) = 
    match options.repeat_interval_opt with
      None -> (0, 1)
    | Some repeat_interval ->
       let rc = int_of_float (module_width /. ((float_of_int repeat_interval) *. xscale)) in
       if float_of_int (rc * repeat_interval) *. xscale < module_width then
         (repeat_interval, rc + 1)
       else
         (repeat_interval, rc) in


  (* pm : map: train_id => seq_id => platform_no *)
  let pm =
    if !Getopt.option_calc_platform_by_z3 then
      platform_map
    else
      assign_platform module_list map_module_to_trains train_time_map
                      (options.repeat_interval_opt <> None) repeat_interval
  in
  let fn =
    if !Getopt.option_output_filename = "" then
      if !Getopt.option_output_type = Getopt.PDF then
        Printf.sprintf "%s.pdf" base_name
      else
        Printf.sprintf "%s.svg" base_name 
    else
      !Getopt.option_output_filename
  in

  (* In order to make a surface for the diagram, the width of the diagram is needed.
     However to calculate the width, the width of the title part is needed and to measure it,
     a surface is needed. To solve this cycle, firstly create a dummy surface.
   *)
  let max_title_width =
    if !Getopt.option_output_type = Getopt.PDF then
      let ch = open_out fn in
      let surface = Cairo_pdf.surface_create_for_channel ch 100.0 100.0 in
      let max_title_width = calc_max_title_width module_list surface in
      close_out ch;
      max_title_width
    else
      let ch = open_out fn in
      let surface = Cairo_svg.surface_create_for_channel ch 100.0 100.0 in
      let max_title_width = calc_max_title_width module_list surface in
      close_out ch;
      max_title_width
  in

  let ox = !left_margin +. max_title_width +. !module_title_width in
  let oy = !top_margin in

  let diag_width = !left_margin +. max_title_width +. !module_title_width +. module_width +. !right_margin in
  let diag_height = !top_margin +. total_height +. !bottom_margin in
  (* surface N.B.: the types of surfaces of Cairo_pdf and Cairo_svg are different. *)
  (if !Getopt.option_output_type = Getopt.PDF then
     let ch = open_out fn in
     let surface = Cairo_pdf.surface_create_for_channel ch diag_width diag_height in
     draw surface mm ox oy module_width module_list train_time_map min_time xscale yscale map_link_to_modules pm train_list options total_height false repeat_interval repeat_count;
     close_out ch
   else
     let ch = open_out fn in
     let surface = Cairo_svg.surface_create_for_channel ch diag_width diag_height in
     draw surface mm ox oy module_width module_list train_time_map min_time xscale yscale map_link_to_modules pm train_list options total_height false repeat_interval repeat_count;
     close_out ch);

  (* In the case that the repeat_interval is specified, make another diagram *)
  (match options.repeat_interval_opt with
     None -> ()
   | Some _ ->
      (* make filename *)
      let fn =
        if !Getopt.option_output_filename = "" then
          if !Getopt.option_output_type = Getopt.PDF then
            Printf.sprintf "%s_ri.pdf" base_name
          else
            Printf.sprintf "%s_ri.svg" base_name 
        else
          !Getopt.option_output_filename in
      let diag_width = !left_margin +. max_title_width +. !module_title_width
                       +. float_of_int (repeat_count * repeat_interval) *. xscale +. !right_margin in
      if !Getopt.option_output_type = Getopt.PDF then
        let ch = open_out fn in
        let surface = Cairo_pdf.surface_create_for_channel ch diag_width diag_height in
        draw surface mm ox oy module_width module_list train_time_map min_time xscale yscale map_link_to_modules pm train_list options total_height true repeat_interval repeat_count;
        close_out ch
      else
        let ch = open_out fn in
        let surface = Cairo_svg.surface_create_for_channel ch diag_width diag_height in
        draw surface mm ox oy module_width module_list train_time_map min_time xscale yscale map_link_to_modules pm train_list options total_height true repeat_interval repeat_count;
        close_out ch;
  )
;;

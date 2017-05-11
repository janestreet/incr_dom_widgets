open! Core_kernel
open! Import
open Js_of_ocaml

module Focus_dir = struct
  type t = Prev | Next [@@deriving sexp, compare]
end

module Margin = struct
  type t =
    { top : int
    ; left : int
    ; bottom : int
    ; right : int
    }
  [@@deriving fields, compare, sexp_of]

  let uniform margin = { top = margin; left = margin; bottom = margin; right = margin }

  let none = uniform 0

  let create ?(top=0) ?(left=0) ?(bottom=0) ?(right=0) () = { top; left; bottom; right }
end

module Scroll_region = struct
  type t = Window | Element of Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t

  module Id = struct
    type t = Window | Element of string [@@deriving compare, sexp]
  end

  let of_id : Id.t -> t option = function
    | Window     -> Some Window
    | Element id -> Option.map (Dom_html.getElementById_opt id) ~f:(fun el -> Element el)
end

module Float_type = struct
  type t = None | Edge | Px_from_edge of int [@@deriving compare, sexp]

  let compute_offset t ~get_float_elem_size =
    match t with
    | None -> 0
    | Edge -> Option.value (get_float_elem_size ()) ~default:0
    | Px_from_edge px ->
      Option.value_map (get_float_elem_size ()) ~f:(fun size -> size + px) ~default:0

  let is_floating = function
    | None -> false
    | Edge | Px_from_edge _ -> true
end

module Scroll_result = struct
  type t = [ `Scrolled | `Didn't_scroll ]

  let combine t1 t2 =
    match t1, t2 with
    | `Didn't_scroll, `Didn't_scroll -> `Didn't_scroll
    | `Scrolled, _ | _, `Scrolled    -> `Scrolled

  let scrolled = function
    | `Scrolled      -> true
    | `Didn't_scroll -> false
end

module Scroll = struct
  module Dir = struct
    type t = Horizontal | Vertical
  end

  let scroll ?(in_=Scroll_region.Window) (dir:Dir.t) shift =
    if shift <> 0
    then (
      begin
        match in_, dir with
        | Window    , Horizontal -> Dom_html.window##scrollBy shift 0
        | Window    , Vertical   -> Dom_html.window##scrollBy 0 shift
        | Element el, Horizontal -> el##.scrollLeft := el##.scrollLeft + shift
        | Element el, Vertical   -> el##.scrollTop  := el##.scrollTop  + shift
      end;
      `Scrolled
    )
    else `Didn't_scroll
  ;;

  let adjust_margins ~start_margin ~end_margin ~scroll_region_start ~scroll_region_end
        ~elem_start ~elem_end =
    let unused_viewport_space =
      (scroll_region_end - scroll_region_start) - (elem_end - elem_start)
    in
    let total_margin = start_margin + end_margin in
    let scale_down m = m * unused_viewport_space / total_margin in
    if unused_viewport_space < 0
    then (0, 0)
    else if unused_viewport_space < total_margin
    then (scale_down start_margin, scale_down end_margin)
    else (start_margin, end_margin)

  let overflow_past_start ~scroll_region_start ~start_margin ~elem_start =
    let start_boundary = scroll_region_start + start_margin in
    Option.some_if (elem_start < start_boundary) (elem_start - start_boundary)
  ;;

  let overflow_past_end ~scroll_region_end ~end_margin ~elem_end =
    let end_boundary = scroll_region_end - end_margin in
    Option.some_if (elem_end > end_boundary) (elem_end - end_boundary)
  ;;

  let scroll_into_region ?in_ dir ~start_margin ~end_margin ~scroll_region_start
        ~scroll_region_end ~elem_start ~elem_end =
    let start_margin, end_margin =
      adjust_margins ~start_margin ~end_margin ~scroll_region_start ~scroll_region_end
        ~elem_start ~elem_end
    in
    let overflow_past_start =
      overflow_past_start ~scroll_region_start ~start_margin ~elem_start
    in
    let overflow_past_end =
      overflow_past_end ~scroll_region_end ~end_margin ~elem_end
    in
    let shift =
      match overflow_past_start, overflow_past_end with
      | None, None          -> 0
      | Some shift, None
      | Some shift, Some _
      | None, Some shift    -> shift
    in
    scroll ?in_ dir shift
  ;;

  let is_in_region ~start_margin ~end_margin ~scroll_region_start ~scroll_region_end
        ~elem_start ~elem_end =
    let start_margin, end_margin =
      adjust_margins ~start_margin ~end_margin ~scroll_region_start ~scroll_region_end
        ~elem_start ~elem_end
    in
    Option.is_none (overflow_past_start ~scroll_region_start ~start_margin ~elem_start) &&
    Option.is_none (overflow_past_end ~scroll_region_end ~end_margin ~elem_end)
  ;;

  let get_elem_start ~scroll_region_start ~position = scroll_region_start + position
  let get_position ~scroll_region_start ~elem_start = elem_start - scroll_region_start

  let scroll_to_position ?in_ dir ~position ~scroll_region_start ~elem_start =
    let target_elem_start = get_elem_start ~scroll_region_start ~position in
    let shift = elem_start - target_elem_start in
    scroll ?in_ dir shift
  ;;

  let scroll_to_position_and_into_region ?in_ dir ~position ~start_margin ~end_margin
        ~scroll_region_start ~scroll_region_end ~elem_start ~elem_end =
    let target_elem_start = get_elem_start ~scroll_region_start ~position in
    let shift = elem_start - target_elem_start in
    let scroll_to_position = scroll ?in_ dir shift in
    let target_elem_end = target_elem_start + elem_end - elem_start in
    let scroll_into_region =
      scroll_into_region ?in_ dir ~start_margin ~end_margin ~scroll_region_start
        ~scroll_region_end ~elem_start:target_elem_start ~elem_end:target_elem_end
    in
    Scroll_result.combine scroll_to_position scroll_into_region
  ;;
end

let move_focus map focus (dir:Focus_dir.t) =
  match focus with
  | None ->
    (* If there's no focus, we grab the extreme element, depending on the direction of
       movement. *)
    begin match dir with
    | Prev -> Map.max_elt map
    | Next -> Map.min_elt map
    end
  | Some key ->
    (* If we are focused, then just move to the next key in the map. *)
    let dir = match dir with
      | Prev -> `Less_than
      | Next -> `Greater_than
    in
    Map.closest_key map dir key
;;

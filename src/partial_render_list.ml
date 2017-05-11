open! Core_kernel
open! Import
open Util

include Partial_render_list_intf

module Make (Key : Key) = struct
  module Key = Key

  module Height_cache = struct
    type t = { cache: int Key.Map.t
             ; height_guess: int }
    [@@deriving fields, compare, sexp_of]

    let empty ~height_guess = { cache = Key.Map.empty; height_guess }
  end

  module Heights = struct
    include Splay_tree.Make_with_reduction (Key) (Int) (struct
        type key = Key.t
        type data = int (* height *)
        type accum = int
        let identity = 0
        let singleton ~key:_ ~data = data
        let combine left right = left + right
      end)

    (** Returns the row (if any) that is at the specified height *)
    let find_by_height (heights:t) height =
      search heights ~f:(fun ~left ~right:_ ->
        if height < left then `Left else `Right
      )
      |> Option.map ~f:fst

    (** The cumulative height of all the rows *)
    let height = accum
  end

  type 'v t = { heights: Heights.t (** Acceleration structure for height queries *)
              ; render_range: Key.t Interval.t (** Section of keys to put in DOM *)
              ; rows_to_render:'v Key.Map.t
              ; measurements: Measurements.t option
              (** The height cache is stashed here after trimming so that it can be
                  accessed later in measure_heights. This way the app doesn't have to
                  store it in its derived model and pass it back to us. The app still
                  stores the height cache in its model, it just doesn't also have to store
                  a trimmed version in its derived model.*)
              ; height_cache: Height_cache.t
              } [@@deriving fields]

  (** How many extra rows will be rendered outside of visible range. Must be even to
      preserve parity for alternating row colours. *)
  let render_width = 6

  let visible_range
        ~(measurements:Measurements.t option Incr.t)
        ~(heights:Heights.t Incr.t)
        ~(rows:_ Key.Map.t Incr.t)
    =
    let%map measurements = measurements
    and heights = heights
    and rows = rows
    in
    match measurements with
    | None -> Interval.Empty
    | Some { list_rect; view_rect } ->
      let module Rect = Js_misc.Rect in
      (* The top of the viewport, as measured from the top of the table body. Note that
         the top of tbody_rect is measured against the viewport, and so is a negative
         number when you're scrolled into the middle of the table.  *)
      let scroll_top = -(Rect.top list_rect) in
      (* The height of the table, which excludes the height of the header *)
      let scroll_bot = scroll_top + Rect.height view_rect in
      let key_top =
        match Heights.find_by_height heights scroll_top with
        | Some x -> Some x
        | None -> Map.min_elt rows |> Option.map ~f:fst
      in
      let key_bot =
        match Heights.find_by_height heights scroll_bot with
        | Some x -> Some x
        | None -> Map.max_elt rows |> Option.map ~f:fst
      in
      let visible_range : _ Interval.t =
        if scroll_top >= Heights.height heights then Empty
        else (
          match key_top, key_bot with
          | None,_ | _, None -> Empty
          | Some top, Some bot -> Range (top,bot)
        )
      in
      visible_range
  ;;

  let create ~rows ~height_cache ~measurements =
    (* Removes elements from the cache that are no longer in the set of all data so it
       doesn't grow monotonically even while rows are removed. *)
    let trimmed_height_cache =
      Incr.Map.merge rows (height_cache >>| Height_cache.cache)
        (* Efficiency optimization, we don't care if the rows change, only the heights *)
        ~data_equal_left:(fun _ _ -> true)
        ~f:(fun ~key:_ v ->
          match v with
          | `Left _ | `Right _ -> None
          | `Both (_,h) -> Some h)
    in
    let row_heights =
      let%bind height_guess = height_cache >>| Height_cache.height_guess in
      Incr.Map.merge rows trimmed_height_cache
        ~data_equal_left:(fun _ _ -> true)
        ~f:(fun ~key:_ data ->
          match data with
          | `Left _ -> Some height_guess
          | `Both (_, height) -> Some height
          | `Right _ -> None)
    in
    let heights =
      Incr.Map.unordered_fold row_heights
        ~init:Heights.empty
        ~f:(fun ~key ~data acc -> Heights.set acc ~key ~data)
        ~f_inverse:(fun ~key ~data:_ acc -> Heights.remove acc key)
    in
    let just_keys =
      Incr.Map.mapi ~data_equal:(fun _ _ -> true)
        rows ~f:(fun ~key:_ ~data:_ -> ())
    in
    let visible_range = visible_range ~measurements ~heights ~rows in
    let render_range =
      let%map visible_range = visible_range
      and just_keys = just_keys
      and heights = heights
      in
      (* Hack to make CSS-based alternating row colours with :nth-of-type(odd) continue to
         work. Ensures that the parity of the number of tr elements before a given element
         is preserved even with partial rendering by sometimes rendering an extra element.
         This is way easier for clients than rolling their own alternating colours. CSS
         frameworks like Bootstrap which provide alternating colours also continue to
         work. *)
      let parity_fix key =
        let num_before = Heights.rank heights key in
        num_before % 2
      in
      let rec move start n get_next =
        if n <= 0 then start
        else (
          match get_next start with
          | None -> start
          | Some next -> move next (n - 1) get_next
        )
      in
      let move start direction amount =
        move start amount (fun x ->
          Map.closest_key just_keys direction x
          |> Option.map ~f:fst)
      in
      match (visible_range : _ Interval.t) with
      | Empty -> Interval.Empty
      | Range (top,bot) ->
        Interval.Range ( move top `Less_than (render_width + parity_fix top)
                       , move bot `Greater_than render_width )
    in
    let rows_to_render =
      let sub_range =
        match%map render_range with
        | Empty -> None
        | Range (x,y) -> Some (x,y)
      in
      Incr.Map.subrange rows sub_range
    in
    let%map heights = heights
    and rows_to_render = rows_to_render
    and render_range = render_range
    and trimmed_height_cache = trimmed_height_cache
    and height_cache = height_cache
    and measurements = measurements
    in
    let height_cache = { height_cache with cache = trimmed_height_cache } in
    { heights; render_range; rows_to_render; measurements; height_cache }
  ;;

  let spacer_heights t =
    let%map render_range = t >>| render_range and heights = t >>| heights in
    match (render_range : _ Interval.t)  with
    | Empty -> (0, Heights.height heights)
    | Range (min_key, max_key) ->
      let { Heights.Partition.lt; gt; _ } =
        Heights.partition heights ~min_key ~max_key
      in
      (Heights.height lt, Heights.height gt)
  ;;

  let call_scroll_function t ~key ~f =
    Option.bind t.measurements ~f:(fun { Measurements. list_rect; view_rect } ->
      let before, at, (_ : Heights.t) = Heights.split t.heights key in
      Option.map at ~f:(fun height ->
        let elem_start = Heights.height before + list_rect.top in
        f ~scroll_region_start:view_rect.top
          ~scroll_region_end:view_rect.bottom
          ~elem_start
          ~elem_end:(elem_start + height)
      )
    )
  ;;

  let scroll_into_scroll_region ?in_ t ~top_margin ~bottom_margin ~key =
    let f =
      Scroll.scroll_into_region ?in_ Vertical ~start_margin:top_margin
        ~end_margin:bottom_margin
    in
    Option.value (call_scroll_function t ~key ~f) ~default:`Didn't_scroll
  ;;

  let scroll_to_position ?in_ t ~position ~key =
    let f ~scroll_region_start ~scroll_region_end:_ ~elem_start ~elem_end:_ =
      Scroll.scroll_to_position ?in_ Vertical ~position ~scroll_region_start ~elem_start
    in
    Option.value (call_scroll_function t ~key ~f) ~default:`Didn't_scroll
  ;;

  let scroll_to_position_and_into_region ?in_ t ~position ~top_margin ~bottom_margin
        ~key =
    let f =
      Scroll.scroll_to_position_and_into_region ?in_ Vertical ~position
        ~start_margin:top_margin ~end_margin:bottom_margin
    in
    Option.value (call_scroll_function t ~key ~f) ~default:`Didn't_scroll
  ;;

  let is_in_region t ~top_margin ~bottom_margin ~key =
    let f = Scroll.is_in_region ~start_margin:top_margin ~end_margin:bottom_margin in
    call_scroll_function t ~key ~f
  ;;

  let get_position t ~key =
    let f ~scroll_region_start ~scroll_region_end:_ ~elem_start ~elem_end:_ =
      Scroll.get_position ~scroll_region_start ~elem_start
    in
    call_scroll_function t ~key ~f
  ;;

  let update_cache cache ~key height =
    match height with
    | None -> cache
    | Some height ->
      (* Optimization: Don't bother adding measured height to [height_cache] if it
         equals the existing height for that key. *)
      if [%compare.equal:int option] (Map.find cache key) (Some height)
      then cache
      else (Map.add cache ~key ~data:height)

  let measure_heights_simple t ~measure =
    let cache =
      Map.fold t.rows_to_render
        ~init:t.height_cache.cache
        ~f:(fun ~key ~data:_ cache -> update_cache cache ~key (measure key))
    in
    { Height_cache.cache; height_guess = t.height_cache.height_guess }
  ;;

  type 'm measure_heights_acc =
    { cache   : int Key.Map.t
    ; prev    : 'm option
    ; current : (Key.t * 'm option) option
    }

  let measure_heights t ~measure_row ~get_row_height =
    let update_cache cache ~current ~prev ~next =
      match current with
      | None -> cache
      | Some (key, curr) -> update_cache cache ~key (get_row_height ~prev ~curr ~next)
    in
    let cache =
      let { cache; prev; current } =
        Map.fold t.rows_to_render
          ~init:{ cache = t.height_cache.cache; prev = None; current = None }
          ~f:(fun ~key:next_key ~data:_ { cache; prev; current } ->
            let next = measure_row next_key in
            { cache = update_cache cache ~current ~prev ~next
            ; prev = Option.bind current ~f:Tuple2.get2
            ; current = Some (next_key, next)
            }
          )
      in
      update_cache cache ~current ~prev ~next:None
    in
    { t.height_cache with cache }
  ;;
end

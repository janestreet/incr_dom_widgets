open! Core_kernel
open! Import
open Vdom

include Table_intf

let cutoff compare x =
  Incr.set_cutoff x (Incr.Cutoff.of_compare compare);
  x

module Make (Row_id : Id) (Column_id : Id) (Sort_spec : Sort_spec) = struct
  include Util

  module Row_id = Row_id
  module Column_id = Column_id

  module Sort_spec = Sort_spec
  module Sort_key  = Sort_spec.Sort_key
  module Sort_dir  = Sort_spec.Sort_dir

  module Sort_criteria = struct
    type t =
      { column_id : Column_id.t
      ; dir : Sort_dir.t
      } [@@deriving fields, compare, sexp]
  end

  module Column = struct
    type 'a t =
      { header: Node.t
      ; group: string option
      ; sort_by: ('a -> Sort_key.t) option
      } [@@deriving fields]

    let create ?group ?sort_by ~header () =
      { header
      ; group
      ; sort_by
      }
  end

  module Row_node_spec = Row_node_spec

  module Visibility_info = struct
    type t =
      { tbody_rect: int Js_misc.Rect.t
      ; view_rect: int Js_misc.Rect.t
      } [@@deriving compare, sexp, fields]
  end

  module Key = struct
    module T = struct
      type t =
        { sort_spec: Sort_spec.t option
        ; row_id: Row_id.t
        } [@@deriving sexp, fields]

      let sort_key t = Option.map t.sort_spec ~f:fst
      let sort_dir t = Option.map t.sort_spec ~f:snd

      (** The comparison function here is written so that any two keys with the same
          sort_dir sort according to that sort_dir; but keys with different sort_dirs just
          have a stable relation between them. This allows us to have one Key type that is
          used by all the different sorting situations, without needing to change
          comparators. *)
      let compare t1 t2 =
        match t1.sort_spec, t2.sort_spec with
        | None, None -> Row_id.compare t1.row_id t2.row_id
        | Some _, None -> -1
        | None, Some _ -> 1
        | Some spec1, Some spec2 ->
          Comparable.lexicographic
            [ (fun (_key1, dir1) (_key2, dir2) -> Sort_dir.compare dir1 dir2)
            ; (fun (key1, dir1) (key2, _dir2) ->
                 Sort_spec.compare
                   ~cmp_row_id:Row_id.compare
                   (key1, t1.row_id)
                   (key2, t2.row_id)
                   dir1
              )
            ]
            spec1
            spec2

      let create sort_spec row_id = { sort_spec; row_id }
    end
    include T
    include Comparable.Make(T)

    let sort spec ~(rows : _ Row_id.Map.t Incr.t) =
      let sort_spec row =
        Option.bind spec ~f:(fun (column, sort_dir) ->
          Option.map (Column.sort_by column) ~f:(fun sort_by -> sort_by row, sort_dir)
        )
      in
      Incr.Map.unordered_fold rows
        ~init:Map.empty
        ~add:(fun ~key:row_id ~data acc ->
          let key = create (sort_spec data) row_id in
          Map.add acc ~key ~data)
        ~remove:(fun ~key:row_id ~data acc ->
          let key = create (sort_spec data) row_id in
          Map.remove acc key)
    ;;
  end

  module Html_id = struct
    type t = string [@@deriving compare, sexp]

    (* This module avoids using [sprintf] for performance reasons *)

    let table         table_id = "table-" ^ Table_id.to_string table_id
    let tbody         table_id = table table_id ^ "-body"
    let thead         table_id = table table_id ^ "-header"
    let column_group  table_id = table table_id ^ "-column-group"
    let column_header table_id = table table_id ^ "-column-header"

    let column_header_cell table_id column_id =
      table table_id ^ "-header-cell-" ^ Column_id.to_string column_id

    let row table_id row_id =
      table table_id ^ "-row-" ^ Row_id.to_string row_id

    let cell_of_parts row_html_id column_id_str =
      row_html_id ^ "-col-" ^ column_id_str

    let cell table_id row_id column_id =
      cell_of_parts (row table_id row_id) (Column_id.to_string column_id)

    let spacer key = "spacer-" ^ key
  end

  module Row_view = Partial_render_list.Make(Key)

  module Model = struct
    type t = { id: Table_id.t (** To avoid DOM id collisions. Never changes. *)
             (** Settings from client. Never change *)
             ; float_header: Float_type.t
             ; float_first_col: Float_type.t
             ; scroll_margin: Margin.t
             ; scroll_region : Scroll_region.Id.t
             (** UI state. Changed by user during app usage *)
             ; focus_row: Row_id.t option
             ; focus_col: Column_id.t option
             ; sort_criteria: Sort_criteria.t option
             (** Info measured from render. Changes each render. *)
             ; height_cache: Row_view.Height_cache.t
             ; visibility_info: Visibility_info.t option
             ; col_group_row_height: int
             ; tbody_html_id : Html_id.t
             ; thead_html_id : Html_id.t
             ; column_group_html_id : Html_id.t
             ; column_header_html_id : Html_id.t
             }
    [@@deriving fields, compare, sexp_of]

    let create ~scroll_margin ~scroll_region ~float_header ~float_first_col ~height_guess
          ?id ?initial_sort ?initial_focus_row ?initial_focus_col () =
      let id =
        match id with
        | Some id -> id
        | None    -> Table_id.create ()
      in
      { id
      ; float_header
      ; float_first_col
      ; scroll_margin
      ; scroll_region
      ; focus_row = initial_focus_row
      ; focus_col = initial_focus_col
      ; sort_criteria = initial_sort
      ; height_cache = Row_view.Height_cache.empty ~height_guess
      ; visibility_info = None
      ; col_group_row_height = 0
      ; tbody_html_id = Html_id.tbody id
      ; thead_html_id = Html_id.thead id
      ; column_group_html_id = Html_id.column_group id
      ; column_header_html_id = Html_id.column_header id
      }

    let sort_dir t = Option.map t.sort_criteria ~f:Sort_criteria.dir
    let sort_column t = Option.map t.sort_criteria ~f:Sort_criteria.column_id

    let set_sort_criteria = Field.fset Fields.sort_criteria

    let cycle_sorting t column_id ~next_dir =
      let get_sort_criteria prev_dir =
        Option.map (next_dir prev_dir) ~f:(fun dir -> { Sort_criteria. dir; column_id })
      in
      let sort_criteria =
        match t.sort_criteria with
        | None -> get_sort_criteria None
        | Some { dir = prev_dir; column_id = prev_column_id } ->
          if Column_id.equal prev_column_id column_id
          then (get_sort_criteria (Some prev_dir))
          else (get_sort_criteria None)
      in
      { t with sort_criteria }

    let get_tbody_rect t =
      Option.map t.visibility_info ~f:Visibility_info.tbody_rect

    let get_focus_rect t =
      let open Option.Let_syntax in
      let%bind row_id = t.focus_row and col_id = t.focus_col in
      let focus_id = Html_id.cell t.id row_id col_id in
      let%map focus_elem = Dom_html.getElementById_opt focus_id in
      Js_misc.client_rect_of_element focus_elem
  end

  module Derived_model = struct
    type 'a t =
      { rows: 'a Row_id.Map.t
      ; sorted_rows: 'a Key.Map.t
      ; columns: (Column_id.t * 'a Column.t) Int.Map.t
      ; column_num_lookup: int Column_id.Map.t
      ; sort_column: 'a Column.t option
      ; row_view: 'a Row_view.t
      ; scroll_region : Scroll_region.t option
      ; has_col_groups : bool
      ; floating_col : Column_id.t option
      } [@@deriving fields]

    let create m ~rows ~(columns : (Column_id.t * _ Column.t) list Incr.t) =
      let scroll_region =
        (* This needs to be mapped on [m] and not [m.scroll_region] so that it refires on
           every stabilize and can actually find the element after it is drawn. *)
        let%map m = m in
        Scroll_region.of_id (Model.scroll_region m)
      in
      let sort_column =
        let%map column_id =
          m >>| Model.sort_column |> cutoff [%compare: Column_id.t option]
        and columns = columns
        in
        Option.bind column_id ~f:(fun column_id ->
          List.find_map columns ~f:(fun (id, c) ->
            if [%compare.equal:Column_id.t] id column_id
            then (Some c) else None
          )
        )
      in
      let sorted_rows =
        let%bind column = sort_column
        and sort_dir = m >>| Model.sort_dir |> cutoff [%compare: Sort_dir.t option]
        in
        let sort_spec = Option.both column sort_dir in
        Key.sort sort_spec ~rows
      in
      let measurements =
        let%map visibility_info = m >>| Model.visibility_info in
        Option.map visibility_info ~f:(fun {Visibility_info.tbody_rect; view_rect; _} ->
          {Partial_render_list.Measurements.list_rect = tbody_rect; view_rect})
      in
      let floating_col =
        let%map float_first_col = m >>| Model.float_first_col
        and columns = columns
        in
        if Float_type.is_floating float_first_col
        then (Option.map (List.hd columns) ~f:fst)
        else None
      in
      let height_cache = m >>| Model.height_cache in
      let column_num_lookup =
        let%map columns = columns in
        Column_id.Map.of_alist_exn (List.mapi columns ~f:(fun i (col_id, _) -> col_id, i))
      in
      let has_col_groups =
        let%map columns = columns in
        List.exists columns ~f:(fun (_, col) -> Option.is_some col.group)
      in
      let columns =
        let%map columns = columns in
        Int.Map.of_alist_exn (List.mapi columns ~f:(fun i col -> i, col))
      in
      let%map row_view = Row_view.create ~rows:sorted_rows ~height_cache ~measurements
      and rows = rows
      and sorted_rows = sorted_rows
      and columns = columns
      and column_num_lookup = column_num_lookup
      and sort_column = sort_column
      and scroll_region = scroll_region
      and floating_col = floating_col
      and has_col_groups = has_col_groups
      in
      { rows
      ; sorted_rows
      ; columns
      ; column_num_lookup
      ; sort_column
      ; row_view
      ; scroll_region
      ; has_col_groups
      ; floating_col
      }
    ;;
  end

  module Action = struct
    type t =
      | Sort_column_clicked of Column_id.t
      | Move_focus_row of Focus_dir.t
      | Move_focus_col of Focus_dir.t
      | Set_focus_row of Row_id.t option
      | Set_focus_col of Column_id.t option
    [@@deriving sexp, compare]

    let move_focus_row dir = Move_focus_row dir
    let move_focus_col dir = Move_focus_col dir

    let set_focus_row row_id = Set_focus_row row_id
    let set_focus_col col_id = Set_focus_col col_id
  end

  type 'a row_renderer
    =  row_id:Row_id.t
    -> row:'a Incr.t
    -> Row_node_spec.t Incr.t

  let current_key (m : Model.t) (d : _ Derived_model.t) ~row_id =
    let open Option.Let_syntax in
    let%map row = Row_id.Map.find d.rows row_id in
    let sort_key =
      Option.bind d.sort_column ~f:(fun column ->
        Option.map (Column.sort_by column) ~f:(fun sort_by -> sort_by row)
      )
    in
    let sort_spec = Option.both sort_key (Model.sort_dir m) in
    Key.create
      sort_spec
      row_id
  ;;

  let move_focus_row m (d : _ Derived_model.t) ~dir =
    let focus_row =
      let open Option.Let_syntax in
      let focus_key =
        let%bind row_id = m.Model.focus_row in
        current_key m d ~row_id
      in
      let%map ({row_id; _}, _) = Util.move_focus d.sorted_rows focus_key dir in
      row_id
    in
    if Option.is_some focus_row
    then { m with focus_row }
    else m
  ;;

  let move_focus_col m (d : _ Derived_model.t) ~dir =
    let focus_col =
      let open Option.Let_syntax in
      let focus_key =
        let%bind col_id = m.Model.focus_col in
        Map.find d.column_num_lookup col_id
      in
      let%map (_, (col_id, _)) = Util.move_focus d.columns focus_key dir in
      col_id
    in
    if Option.is_some focus_col
    then { m with focus_col }
    else m
  ;;

  let set_focus_row (m : Model.t) row_id =
    if [%compare.equal:Row_id.t option] m.focus_row row_id
    then m
    else { m with focus_row = row_id }
  ;;

  let set_focus_col (m : Model.t) col_id =
    if [%compare.equal:Column_id.t option] m.focus_col col_id
    then m
    else { m with focus_col = col_id }
  ;;

  (* Possible offset due to floating header *)
  let get_top_margin_offset (m : Model.t) =
    let get_float_elem_size () =
      Option.map (Dom_html.getElementById_opt m.thead_html_id)
        ~f:(fun el -> Js_misc.viewport_rect_of_element el |> Js_misc.Rect.height)
    in
    Float_type.compute_offset m.float_header ~get_float_elem_size
  ;;

  (* Possible offset due to floating first column *)
  let get_left_margin_offset (m : Model.t) (d : _ Derived_model.t) ~is_floating_col =
    if is_floating_col
    then 0
    else (
      let get_float_elem_size () =
        let open Option.Let_syntax in
        let%bind (_, (first_column_id, _)) = Map.min_elt d.columns in
        let%map el =
          Dom_html.getElementById_opt (Html_id.column_header_cell m.id first_column_id)
        in
        Js_misc.viewport_rect_of_element el |> Js_misc.Rect.width
      in
      Float_type.compute_offset m.float_first_col ~get_float_elem_size
    )
  ;;

  let call_row_scroll_function m d ~row_id ~f =
    Option.map (current_key m d ~row_id) ~f:(fun key -> f d.row_view ~key)
  ;;

  let is_floating_col (d : _ Derived_model.t) column_id =
    [%compare.equal: Column_id.t option] d.floating_col (Some column_id)
  ;;

  let call_col_scroll_function ?f_if_currently_floating (m : Model.t) ~column_id ~f
        ~is_floating_col =
    let open Option.Let_syntax in
    let%map cell_rect =
      let%map header_cell =
        Dom_html.getElementById_opt (Html_id.column_header_cell m.id column_id)
      in
      Js_misc.viewport_rect_of_element header_cell
    and { tbody_rect; view_rect } = m.visibility_info
    in
    let elem_start, elem_end, is_currently_floating =
      if not is_floating_col
      then (Js_misc.Rect.left cell_rect, Js_misc.Rect.right cell_rect, false)
      else begin
        let left = Js_misc.Rect.left tbody_rect in
        let width = Js_misc.Rect.width cell_rect in
        let is_currently_floating =
          match Float_type.px_from_edge m.float_first_col with
          | None    -> false
          | Some px ->
            let float_pos_left  = view_rect.left + px in
            let float_pos_right = float_pos_left + width in
            tbody_rect.left  <= float_pos_left &&
            tbody_rect.right >= float_pos_right
        in
        left, left + width, is_currently_floating
      end
    in
    let f =
      match is_currently_floating, f_if_currently_floating with
      | true, Some f'      -> f'
      | false, _ | _, None -> f
    in
    f ~scroll_region_start:view_rect.left
      ~scroll_region_end:view_rect.right
      ~elem_start
      ~elem_end
  ;;

  let scroll_row_into_scroll_region (m:Model.t) (d : _ Derived_model.t) row_id =
    let top_margin_offset = get_top_margin_offset m in
    let f =
      Row_view.scroll_into_scroll_region
        ?in_:d.scroll_region
        ~top_margin:(m.scroll_margin.top + top_margin_offset)
        ~bottom_margin:m.scroll_margin.bottom
    in
    Option.value (call_row_scroll_function m d ~row_id ~f) ~default:`Didn't_scroll
  ;;

  let scroll_col_into_scroll_region (m:Model.t) (d:_ Derived_model.t) column_id =
    let is_floating_col = is_floating_col d column_id in
    let left_margin_offset = get_left_margin_offset m d ~is_floating_col in
    let f =
      Scroll.scroll_into_region
        ?in_:d.scroll_region
        Horizontal
        ~start_margin:(m.scroll_margin.left + left_margin_offset)
        ~end_margin:m.scroll_margin.right
    in
    let f_if_currently_floating ~scroll_region_start:_ ~scroll_region_end:_ ~elem_start:_
          ~elem_end:_ =
      `Didn't_scroll
    in
    Option.value
      (call_col_scroll_function m ~column_id ~f ~f_if_currently_floating ~is_floating_col)
      ~default:`Didn't_scroll
  ;;

  let scroll_row_to_position ?keep_in_scroll_region (m : Model.t) (d: _ Derived_model.t)
        row_id ~position =
    let f =
      match keep_in_scroll_region with
      | None    -> Row_view.scroll_to_position ?in_:d.scroll_region ~position
      | Some () ->
        let top_margin_offset = get_top_margin_offset m in
        Row_view.scroll_to_position_and_into_region
          ?in_:d.scroll_region
          ~position
          ~top_margin:(m.scroll_margin.top + top_margin_offset)
          ~bottom_margin:m.scroll_margin.bottom
    in
    Option.value (call_row_scroll_function m d ~row_id ~f) ~default:`Didn't_scroll
  ;;

  let scroll_col_to_position ?keep_in_scroll_region (m:Model.t) (d:_ Derived_model.t)
        column_id ~position =
    let is_floating_col = is_floating_col d column_id in
    let scroll_to_position ~scroll_region_start ~scroll_region_end:_
          ~elem_start ~elem_end:_ =
      Scroll.scroll_to_position
        ?in_:d.scroll_region
        Horizontal
        ~position
        ~scroll_region_start
        ~elem_start
    in
    let scroll_to_position_and_into_region =
      let left_margin_offset = get_left_margin_offset m d ~is_floating_col in
      Scroll.scroll_to_position_and_into_region
        ?in_:d.scroll_region
        Horizontal
        ~position
        ~start_margin:(m.scroll_margin.left + left_margin_offset)
        ~end_margin:m.scroll_margin.right
    in
    let f, f_if_currently_floating =
      match keep_in_scroll_region with
      | None    -> scroll_to_position                , None
      | Some () -> scroll_to_position_and_into_region, Some scroll_to_position
    in
    Option.value
      (call_col_scroll_function m ~column_id ~f ~is_floating_col ?f_if_currently_floating)
      ~default:`Didn't_scroll
  ;;

  let row_is_in_scroll_region (m : Model.t) (d : _ Derived_model.t) row_id =
    let top_margin_offset = get_top_margin_offset m in
    let f =
      Row_view.is_in_region
        ~top_margin:(m.scroll_margin.top + top_margin_offset)
        ~bottom_margin:m.scroll_margin.bottom
    in
    Option.join (call_row_scroll_function m d ~row_id ~f)
  ;;

  let col_is_in_scroll_region (m : Model.t) (d : _ Derived_model.t) column_id =
    let is_floating_col = is_floating_col d column_id in
    let left_margin_offset = get_left_margin_offset m d ~is_floating_col in
    let f =
      Scroll.is_in_region
        ~start_margin:(m.scroll_margin.left + left_margin_offset)
        ~end_margin:m.scroll_margin.right
    in
    let f_if_currently_floating ~scroll_region_start:_ ~scroll_region_end:_ ~elem_start:_
          ~elem_end:_ =
      true
    in
    call_col_scroll_function m ~column_id ~f ~f_if_currently_floating ~is_floating_col
  ;;

  let get_row_position (m:Model.t) (d:_ Derived_model.t) row_id =
    let f = Row_view.get_position in
    Option.join (call_row_scroll_function m d ~row_id ~f)
  ;;

  let get_col_position (m:Model.t) (d: _ Derived_model.t)
        column_id =
    let is_floating_col = is_floating_col d column_id in
    let f ~scroll_region_start ~scroll_region_end:_ ~elem_start ~elem_end:_ =
      Scroll.get_position ~scroll_region_start ~elem_start
    in
    call_col_scroll_function m ~column_id ~f ~is_floating_col
  ;;

  let scroll_focus_into_scroll_region (m:Model.t) d =
    let row_scroll =
      Option.value_map m.focus_row ~default:`Didn't_scroll
        ~f:(scroll_row_into_scroll_region m d)
    in
    let col_scroll =
      Option.value_map m.focus_col ~default:`Didn't_scroll
        ~f:(scroll_col_into_scroll_region m d)
    in
    Scroll_result.combine row_scroll col_scroll
  ;;

  let scroll_focus_to_position ?keep_in_scroll_region (m:Model.t) d ~position:(x, y) =
    let row_scroll =
      Option.value_map m.focus_row ~default:`Didn't_scroll
        ~f:(scroll_row_to_position ?keep_in_scroll_region m d ~position:y)
    in
    let col_scroll =
      Option.value_map m.focus_col ~default:`Didn't_scroll
        ~f:(scroll_col_to_position ?keep_in_scroll_region m d ~position:x)
    in
    Scroll_result.combine row_scroll col_scroll
  ;;

  let focus_is_in_scroll_region (m : Model.t) d =
    let row = Option.bind m.focus_row ~f:(row_is_in_scroll_region m d) in
    let col =
      Option.bind m.focus_col ~f:(col_is_in_scroll_region m d)
    in
    match row, col with
    | None, None       -> None
    | None, Some b
    | Some b, None     -> Some b
    | Some b1, Some b2 -> Some (b1 && b2)
  ;;

  let get_focus_position (m : Model.t) (d : _ Derived_model.t) =
    Option.bind m.focus_col ~f:(get_col_position m d),
    Option.bind m.focus_row ~f:(get_row_position m d)
  ;;

  let find_row_by_position (m : Model.t) (d : _ Derived_model.t) position =
    let open Option.Let_syntax in
    let%map { Visibility_info. tbody_rect; _ } = m.visibility_info in
    let position = position - Js_misc.Rect.top tbody_rect in
    if position < 0
    then `Before
    else (
      match Row_view.find_by_position d.row_view ~position with
      | Some { Key. row_id; _ } -> `At row_id
      | None                    -> `After
    )
  ;;

  let find_col_by_position (m : Model.t) (d : _ Derived_model.t) position =
    let open Option.Let_syntax in
    let result =
      List.fold_until (Map.data d.columns) ~init:true ~f:(fun is_first (col_id, _) ->
        let col_header_rect =
          let html_id = Html_id.column_header_cell m.id col_id in
          let%map elem = Dom_html.getElementById_opt html_id in
          Js_misc.viewport_rect_of_element elem
        in
        match col_header_rect with
        | None      -> Stop None
        | Some rect ->
          if is_first && position < Js_misc.Rect.left rect
          then (Stop (Some `Before))
          else if position <= Js_misc.Rect.right rect
          then (Stop (Some (`At col_id)))
          else (Continue false)
      )
    in
    match result with
    | Stopped_early result -> result
    | Finished      false  -> Some `After
    | Finished      true   ->
      let%map { Visibility_info. tbody_rect; _ } = m.visibility_info in
      if position < Js_misc.Rect.left tbody_rect
      then `Before
      else `After
  ;;

  let on_display ~(old:Model.t) (model:Model.t) d =
    if old.focus_row <> model.focus_row || old.focus_col <> model.focus_col
    then (
      let maybe_scroll x f =
        Option.iter x ~f:(fun x -> ignore (f model d x : Scroll_result.t))
      in
      maybe_scroll model.focus_row scroll_row_into_scroll_region ;
      maybe_scroll model.focus_col scroll_col_into_scroll_region ;
    )
  ;;

  let sort_column_clicked =
    Model.cycle_sorting ~next_dir:Sort_dir.next
  ;;

  let apply_action m d (action : Action.t) =
    match action with
    | Sort_column_clicked column_id -> sort_column_clicked m column_id
    | Move_focus_row dir -> move_focus_row m d ~dir
    | Move_focus_col dir -> move_focus_col m d ~dir
    | Set_focus_row row_id -> set_focus_row m row_id
    | Set_focus_col col_id -> set_focus_col m col_id
  ;;

  (** returns the element associated with the row id in question  *)
  let find_row_element table_id row_id =
    Dom_html.getElementById_opt (Html_id.row table_id row_id)

  let update_col_group_row_height (m : Model.t) (d : _ Derived_model.t) =
    let has_floating_header () = Float_type.is_floating m.float_header in
    let height =
      if not (d.has_col_groups && has_floating_header ())
      then None
      else (
        let open Option.Let_syntax in
        let%map column_group  = Dom_html.getElementById_opt (Html_id.column_group m.id)
        and     column_header = Dom_html.getElementById_opt (Html_id.column_header m.id)
        in
        (* We don't use [Js_misc.viewport_rect_of_element] here so that we can round down
           instead of rounding to the nearest interger. This reduces jitter. *)
        let column_group_top  = column_group##getBoundingClientRect##.top  in
        let column_header_top = column_header##getBoundingClientRect##.top in
        int_of_float (column_header_top -. column_group_top)
      )
    in
    match height with
    | None -> m
    | Some height ->
      if height <> m.col_group_row_height
      then { m with col_group_row_height = height }
      else m

  (** Computes and updates the heights of all rows that are currently rendered *)
  let update_heights (m : Model.t) (d : _ Derived_model.t) =
    let height_cache =
      Row_view.measure_heights
        d.row_view
        ~measure_row:(fun key ->
          Option.map (find_row_element m.id key.row_id) ~f:(fun el ->
            let rect = Js_misc.viewport_rect_of_element el in
            Js_misc.Rect.top rect, Js_misc.Rect.bottom rect
          )
        )
        ~get_row_height:(fun ~prev ~curr ~next ->
          Option.map curr ~f:(fun (curr_top, curr_bottom) ->
            let with_top_margin =
              Option.map (Option.map prev ~f:Tuple2.get2)
                ~f:(fun prev_bottom -> curr_bottom - prev_bottom)
            in
            let with_bottom_margin =
              Option.map (Option.map next ~f:Tuple2.get1)
                ~f:(fun next_top -> next_top - curr_top)
            in
            match with_top_margin, with_bottom_margin with
            | Some h1, Some h2 -> (h1 + h2) / 2
            | Some h, None
            | None, Some h     -> h
            | None, None       -> curr_bottom - curr_top
          )
        )
    in
    { m with height_cache }

  let update_visibility m d =
    let m = update_col_group_row_height m d in
    let m = update_heights m d in
    match d.scroll_region, Dom_html.getElementById_opt m.tbody_html_id with
    | None, _ | _, None ->
      (* If the app doesn't render either table but calls update_visibility, do nothing *)
      m
    | Some scroll_region, Some tbody ->
      let view_rect =
        match scroll_region with
        | Window     -> Js_misc.client_rect ()
        | Element el -> Js_misc.client_rect_of_element el
      in
      let visibility_info =
        Some { Visibility_info.
               tbody_rect = Js_misc.client_rect_of_element tbody
             ; view_rect
             }
      in
      if [%compare.equal: Visibility_info.t option] visibility_info m.visibility_info
      then m
      else { m with visibility_info }
  ;;

  let px_of_int x =
    Int.to_string x ^ "px"

  let spacer ~key =
    let id_attr = Attr.id (Html_id.spacer key) in
    stage (fun height ->
      [Node.tr ~key [ id_attr; Attr.style [ "height", px_of_int height ]] []]
    )

  let sticky_pos side (pos:Float_type.t Incr.t) =
    let%map pos = pos >>| Float_type.px_from_edge in
    Option.map pos ~f:(fun pos -> side, pos)

  let finalize_sticky_pos sticky_pos =
    Option.map sticky_pos ~f:(fun (side, pos) -> side, sprintf "%dpx" pos)

  let sticky_style ~sticky_pos ~z_index =
    let sticky_style =
      if List.is_empty sticky_pos
      then []
      else (("position", "sticky") :: sticky_pos)
    in
    let z_index_style = "z-index", Int.to_string z_index in
    z_index_style :: sticky_style

  let view_header ?override_on_click ~inject ~columns ~top_sticky_pos ~left_sticky_pos m =
    let get_sticky_attrs ~top_sticky_pos =
      let first_cell =
        sticky_style ~sticky_pos:(List.filter_opt [ left_sticky_pos; top_sticky_pos ])
          ~z_index:3
        |> Attr.style
      in
      let default =
        sticky_style ~sticky_pos:(Option.to_list top_sticky_pos) ~z_index:2
        |> Attr.style
      in
      first_cell, default
    in
    let header_nodes =
      let%map sort_criteria = m >>| Model.sort_criteria
      and id = m >>| Model.id
      and columns = columns
      and top_sticky_pos =
        match top_sticky_pos with
        | None           -> Incr.return None
        | Some (dir, px) ->
          let%map col_group_row_height = m >>| Model.col_group_row_height in
          finalize_sticky_pos (Some (dir, px + col_group_row_height))
      in
      let first_cell_sticky_attr, default_sticky_attr =
        get_sticky_attrs ~top_sticky_pos
      in
      (List.mapi (Map.data columns) ~f:(fun i (key, data) ->
         let sticky_attr =
           if i = 0
           then [ first_cell_sticky_attr ]
           else [ default_sticky_attr    ]
         in
         let dir =
           Option.bind sort_criteria ~f:(fun { Sort_criteria. column_id; dir } ->
             Option.some_if (Column_id.equal key column_id) dir
           )
         in
         let sort_direction_indicator =
           match Sort_dir.indicator dir with
           | None -> ""
           | Some ind -> sprintf " %s" ind
         in
         let sort_direction_classes =
           List.filter_opt
             [ Option.map dir ~f:(fun _ -> "sorted")
             ; Sort_dir.class_ dir
             ]
         in
         let on_click =
           Option.value_map
             (Column.sort_by data)
             ~default:[]
             ~f:(fun _ ->
               [ Attr.on_click (fun ev ->
                   match override_on_click with
                   | Some on_click -> on_click key ev
                   | None          -> inject (Action.Sort_column_clicked key)
                 )
               ]
             )
         in
         let attrs =
           [ Attr.id (Html_id.column_header_cell id key)
           ; Attr.classes ("column-header" :: sort_direction_classes)
           ]
           @ on_click
           @ sticky_attr
         in
         Node.th attrs [ data.Column.header; Node.text sort_direction_indicator]))
    in
    let group_nodes =
      let top_sticky_pos = finalize_sticky_pos top_sticky_pos in
      let first_cell_sticky_attr, default_sticky_attr =
        get_sticky_attrs ~top_sticky_pos
      in
      let%map columns = columns in
      let groups = List.map (Map.data columns) ~f:(fun c -> (snd c).Column.group) in
      if List.for_all groups ~f:Option.is_none
      then None
      else begin
        let grouped =
          List.groupi groups ~break:(fun i x y ->
            (i = 1 && Option.is_some left_sticky_pos)
            || not (Option.equal String.equal x y)
          )
        in
        List.mapi grouped ~f:(fun i l ->
          let sticky_attr =
            if i = 0
            then [ first_cell_sticky_attr ]
            else [ default_sticky_attr    ]
          in
          let text, class_ =
            match Option.join (List.hd l) with
            | None   -> ( [], "column-group-empty" )
            | Some s -> ( [Node.text s], "column-group-full" )
          in
          Node.th
            ([ Attr.classes [ "column-group"; class_ ]
             ; Attr.create "colspan" (Int.to_string (List.length l))
             ]
             @ sticky_attr
            )
            text
        )
        |> Option.some
      end
    in
    let group_attrs =
      let%map column_group_html_id = m >>| Model.column_group_html_id in
      [ Attr.id column_group_html_id ]
    in
    let header_attrs =
      let%map column_header_html_id = m >>| Model.column_header_html_id in
      [ Attr.id column_header_html_id ]
    in
    let%map group_nodes  = group_nodes
    and     header_nodes = header_nodes
    and     group_attrs  = group_attrs
    and     header_attrs = header_attrs
    in
    [ Option.map group_nodes ~f:(fun n -> Node.tr group_attrs n)
    ; Some (Node.tr header_attrs header_nodes)
    ]
    |> List.filter_opt

  type row_html_ids =
    { row_html_id : Html_id.t
    ; cell_html_ids : Html_id.t list
    }

  let view_rendered_rows ~table_id ~column_ids ~row_view ~render_row ~left_sticky_pos =
    let non_sticky_style = sticky_style ~sticky_pos:[] ~z_index:1 in
    let sticky_style =
      sticky_style ~sticky_pos:(Option.to_list left_sticky_pos) ~z_index:2
    in
    let%bind column_ids = column_ids in
    let column_id_strs = List.map column_ids ~f:Column_id.to_string in
    (* Annotate each row with its html ids - we do this because the string conversions can
       be expensive, and don't need to be re-done every time a row's incremental fires. *)
    let rows_to_render_with_html_ids =
      Incr.Map.unordered_fold (row_view >>| Row_view.rows_to_render)
        ~init:Key.Map.empty
        ~add:(fun ~key ~data:row acc ->
          let row_html_id = Html_id.row table_id key.row_id in
          let cell_html_ids =
            List.map column_id_strs ~f:(Html_id.cell_of_parts row_html_id)
          in
          Map.add acc ~key ~data:({ row_html_id; cell_html_ids }, row)
        )
        ~remove:(fun ~key ~data:_ acc -> Map.remove acc key)
        ~update:(fun ~key ~old_data:_ ~new_data:row acc ->
          Map.change acc key ~f:(Option.map ~f:(Tuple2.map_snd ~f:(fun _ -> row)))
        )
    in
    Incr.Map.mapi' rows_to_render_with_html_ids
      ~f:(fun ~key ~data ->
        let%bind { row_html_id; cell_html_ids } = data >>| fst in
        let%map { Row_node_spec. row_attrs; cells } =
          render_row ~row_id:key.row_id ~row:(data >>| snd)
        in
        let cells =
          List.zip_exn cell_html_ids cells
          |> List.mapi ~f:(fun i (cell_html_id, { Row_node_spec.Cell. attrs; node }) ->
            let sticky_style = if i = 0 then sticky_style else non_sticky_style in
            let attrs =
              attrs.other_attrs
              @ [ Attr.style (attrs.style @ sticky_style)
                ; Attr.id cell_html_id
                ]
            in
            Node.td attrs [ node ]
          )
        in
        let row_attrs = Row_node_spec.Attrs.to_vdom_attrs row_attrs in
        Node.tr ~key:row_html_id (row_attrs @ [ Attr.id row_html_id ]) cells
      )

  let view ?override_header_on_click m d ~render_row ~inject ~attrs =
    let spacer_before = unstage (spacer ~key:"before") in
    let spacer_after  = unstage (spacer ~key:"after")  in
    let columns = d >>| Derived_model.columns in
    let column_ids =
      let%map column_ids = d >>| Derived_model.columns in
      List.map (Map.data column_ids) ~f:fst
    in
    let row_view = d >>| Derived_model.row_view in
    let%bind table_id = m >>| Model.id
    and      top_sticky_pos  = sticky_pos "top"  (m >>| Model.float_header)
    and      left_sticky_pos = sticky_pos "left" (m >>| Model.float_first_col)
    in
    let left_sticky_pos = finalize_sticky_pos left_sticky_pos in
    let%map header =
      view_header ?override_on_click:override_header_on_click ~inject ~columns
        ~top_sticky_pos ~left_sticky_pos m
    and rendered_rows =
      view_rendered_rows ~table_id ~column_ids ~row_view ~render_row ~left_sticky_pos
    and before_height, after_height = Row_view.spacer_heights row_view
    in
    Node.table attrs
      [ Node.thead
          [ Attr.id (Html_id.thead table_id)
          ; Attr.style ([ "background-color", "inherit" ])
          ]
          header
      ; Node.tbody [Attr.id (Html_id.tbody table_id)]
          (spacer_before before_height
           @ Map.data rendered_rows
           @ spacer_after after_height)
      ]
  ;;
end

module Default_sort_spec = struct
  module Sort_key = struct
    type t =
      | String of string
      | Float of float
      | Integer of Int63.t
      | Null
    [@@deriving compare, sexp]
  end

  module Sort_dir = struct
    type t = Ascending | Descending [@@deriving sexp, compare]

    let next = function
      | None            -> Some Ascending
      | Some Ascending  -> Some Descending
      | Some Descending -> None

    let indicator =
      Option.map ~f:(function
        | Ascending -> "▲"
        | Descending -> "▼"
      )

    let class_ =
      Option.map ~f:(function
        | Ascending -> "sorted-asc"
        | Descending -> "sorted-desc"
      )

    let sign = function
      | Ascending -> 1
      | Descending -> -1
  end

  type t = Sort_key.t * Sort_dir.t [@@deriving sexp, compare]

  let compare ~cmp_row_id (k1, r1) (k2, r2) dir =
    match (k1:Sort_key.t), (k2:Sort_key.t) with
    (** Always sort nulls last regardless of the sort direction *)
    | Null, _ | _, Null -> Sort_key.compare k1 k2
    | _, _ ->
      let dir_sign = Sort_dir.sign dir in
      let key_cmp =  Sort_key.compare k1 k2 * dir_sign in
      if key_cmp <> 0 then key_cmp else (cmp_row_id r1 r2 * dir_sign)
end

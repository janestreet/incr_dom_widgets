open! Core_kernel
open! Import

(** [Sort_key] determines what values the rows are sorted on. *)
module type Sort_key = sig
  (** [t] is the type of the keys used to sort rows. Each row maps to a key of type [t]
      based on the contents of the row and the current column being sorted on. *)
  type t [@@deriving sexp]
end

(** [Sort_dir] determines the different ways in which the rows can be sorted
    (e.g. ascending and descending). *)
module type Sort_dir = sig
  type t [@@deriving sexp, compare]

  (** [next] cycles through sort directions. This is used to determine how to
      update the sort direction when a header is clicked on. *)
  val next : t option -> t option

  (** [indicator] defines the symbol to display in each header (e.g. "▲" for ascending).
      The header of the column currently sorted on in dir [sort_dir] will have indicator
      [indicator (Some dir)]. All other headers will have indicator [indicator None]. *)
  val indicator : t option -> string option

  (** [class_] defines the css class of each header, to allow for additional style rules.
      The header of the column currently sorted on in dir [sort_dir] will have class
      [class_ (Some dir)]. All other headers will have class [class_ None]. *)
  val class_ : t option -> string option
end

(** [Sort_spec] defines how rows are sorted in the table. *)
module type Sort_spec = sig
  module Sort_key : Sort_key
  module Sort_dir : Sort_dir

  type t = (Sort_key.t * Sort_dir.t) [@@deriving sexp, compare]

  (** [compare] provides a comparison function between rows' [Sort_key.t] and [Row_id.t],
      taking [Sort_dir.t] into account. This is used to sort rows of the table. *)
  val compare
    :  cmp_row_id:('row_id -> 'row_id -> int)
    -> (Sort_key.t * 'row_id)
    -> (Sort_key.t * 'row_id)
    -> Sort_dir.t
    -> int
end

include Util

module type Id = sig
  type t [@@deriving sexp]
  include Comparable.S with type t := t
end

module type S = sig
  module Row_id : Id

  (** This isn't just an int so that if an app adds columns the sort is maintained
      properly by the app's own classification instead of index. But it can be an int.*)
  module Column_id : Id

  module Sort_spec : Sort_spec

  module Sort_key = Sort_spec.Sort_key
  module Sort_dir = Sort_spec.Sort_dir

  module Sort_criteria : sig
    type t =
      { column_id : Column_id.t
      ; dir : Sort_dir.t
      }
    [@@deriving fields, compare, sexp]
  end

  module Html_id : sig
    (** HTML element ids *)
    val table              : Table_id.t ->                            string
    val tbody              : Table_id.t ->                            string
    val thead              : Table_id.t ->                            string
    val column_group       : Table_id.t ->                            string
    val column_header      : Table_id.t ->                            string
    val column_header_cell : Table_id.t ->             Column_id.t -> string
    val row                : Table_id.t -> Row_id.t ->                string
    val cell               : Table_id.t -> Row_id.t -> Column_id.t -> string
  end

  module Column : sig
    type 'a t

    val create
      (** optionally render a row above the headers with their group names (similar to
          catalog). columns with the same group must be adjacent to be grouped together *)
      :  ?group:string
      (** used to extract a sortable value for this column from a row. *)
      -> ?sort_by:('a -> Sort_key.t)
      (** rendered at the top of the column.
          this node is wrapped in a <th> node with other attributes *)
      -> header:Vdom.Node.t
      -> unit
      -> 'a t

    val group : _ t -> string option
    val sort_by : 'a t -> ('a -> Sort_key.t) option
    val header : _ t -> Vdom.Node.t
  end

  (** This is the key used for sorting functionality. Apps don't need to touch this to use
      this widget, it is only exposed in case apps need to do something involving sorting
      in the same way as the table will. *)
  module Key : sig
    type t [@@deriving sexp]

    val sort_spec : t -> Sort_spec.t option
    val sort_key  : t -> Sort_key.t  option
    val sort_dir  : t -> Sort_dir.t  option
    val row_id    : t -> Row_id.t

    include Comparable.S with type t := t

    val create
      :  Sort_spec.t option
      -> Row_id.t
      -> t

    (** Sorts a map of rows in the same way as using the table's built in sort *)
    val sort
      :  ('a Column.t * Sort_dir.t) option
      -> rows:'a Row_id.Map.t Incr.t
      -> 'a Map.t Incr.t
  end

  module Model : sig
    type t [@@deriving compare, sexp_of]

    val create
      (** How far scroll_to and focus moving should keep the row from the [scroll_region] edge *)
      :  scroll_margin:Margin.t
      (** Element to scroll in scroll_to and focus moves *)
      -> scroll_region:Scroll_region.Id.t
      (** Whether to float the table header fixed to the top or to a specified position on
          scrolling *)
      -> float_header:Float_type.t
      -> float_first_col:Float_type.t
      (** Estimated height of a normal row *)
      -> height_guess:int
      (** Id of the table.  This must be a fresh id - one that has not been passed to
          [Model.create] before - or behavior is undefined.  It maybe be useful to provide
          your own id here if you need access to the id before you create its associated
          [Model.t]. *)
      -> ?id:Table_id.t
      (** The column and sort direction that the table should be (initially) sorted by.
          Sorting can be changed later via clicking on column headers. If [initial_sort]
          is not specified, then the table is sorted by [Row_id]. *)
      -> ?initial_sort:Sort_criteria.t
      -> ?initial_focus_row:Row_id.t
      -> ?initial_focus_col:Column_id.t
      -> unit
      -> t

    val id : t -> Table_id.t

    val focus_row : t -> Row_id.t option
    val focus_col : t -> Column_id.t option

    val sort_criteria : t -> Sort_criteria.t option
    val sort_column : t -> Column_id.t option
    val sort_dir : t -> Sort_dir.t option

    val set_sort_criteria : t -> Sort_criteria.t option -> t

    (** [cycle_sorting] computes and sets new sort criteria based on the current criteria.
        If the given column id is equal to the current sort column, the sort direction is
        updated by calling [next_dir] on the current sort direction.  Otherwise, the sort
        column is set to the given column id, and the sort direction is computed by
        calling [next_dir] on [None]. *)
    val cycle_sorting
      :  t
      -> Column_id.t
      -> next_dir:(Sort_dir.t option -> Sort_dir.t option)
      -> t
  end

  module Derived_model : sig
    type 'a t

    val create
      :  Model.t Incr.t
      -> rows:'a Row_id.Map.t Incr.t
      (** This is a list and not a map so the app can decide order *)
      -> columns:(Column_id.t * 'a Column.t) list Incr.t
      -> 'a t Incr.t

    val sorted_rows : 'a t -> 'a Key.Map.t
    val sort_column : 'a t -> 'a Column.t option
    val scroll_region : _ t -> Scroll_region.t option
  end

  module Action : sig
    type t [@@deriving sexp, compare]

    (** Moves the current focus in a given direction. If there is no focus it focuses the
        top or bottom row on moving down and up respectively. *)
    val move_focus_row : Focus_dir.t -> t
    val move_focus_col : Focus_dir.t -> t

    val set_focus_row : Row_id.t option -> t
    val set_focus_col : Column_id.t option -> t
  end

  (** [current_key m d row_id] returns [row_id]'s [Key.t] associated with the current sort
      criteria of [m]. Returns [None] if [row_id] does not exist in [d] *)
  val current_key
    :  Model.t
    -> _ Derived_model.t
    -> row_id:Row_id.t
    -> Key.t option

  (** Functions [scroll_*_into_scroll_region] and [scroll_*_to_position] will always work if
      the row heights in the model are correct (either because the row height estimate is
      correct for all rows, or because all rows have already been rendered and measured).
      If the row heights in the model are off, it may take multiple iterations of calling
      the scroll function and then remeasuring row heights in [update_visibility] before
      the specified element is successfully scrolled to its target. *)

  val scroll_row_into_scroll_region
    :  Model.t
    -> _ Derived_model.t
    -> Row_id.t
    -> Scroll_result.t

  val scroll_col_into_scroll_region
    :  Model.t
    -> _ Derived_model.t
    -> Column_id.t
    -> Scroll_result.t

  val scroll_focus_into_scroll_region
    :  Model.t
    -> _ Derived_model.t
    -> Scroll_result.t

  val scroll_row_to_position
    :  ?keep_in_scroll_region:unit
    -> Model.t
    -> _ Derived_model.t
    -> Row_id.t
    -> position:int
    -> Scroll_result.t

  val scroll_col_to_position
    :  ?keep_in_scroll_region:unit
    -> Model.t
    -> _ Derived_model.t
    -> Column_id.t
    -> position:int
    -> Scroll_result.t

  val scroll_focus_to_position
    :  ?keep_in_scroll_region:unit
    -> Model.t
    -> _ Derived_model.t
    -> position:(int * int)
    -> Scroll_result.t

  (** Functions [*_is_in_scroll_region] and [get_*_position] return [None] if the specified
      element is not found (e.g. there is no focus, or there is no row/column with the
      given id), or if the visibility measurements are not yet available. *)

  val row_is_in_scroll_region
    :  Model.t
    -> _ Derived_model.t
    -> Row_id.t
    -> bool option

  val col_is_in_scroll_region
    :  Model.t
    -> _ Derived_model.t
    -> Column_id.t
    -> bool option

  val focus_is_in_scroll_region
    :  Model.t
    -> _ Derived_model.t
    -> bool option

  val get_row_position
    :  Model.t
    -> _ Derived_model.t
    -> Row_id.t
    -> int option

  val get_col_position
    :  Model.t
    -> _ Derived_model.t
    -> Column_id.t
    -> int option

  val get_focus_position
    :  Model.t
    -> _ Derived_model.t
    -> int option * int option

  (** Used for scrolling to rows/columns upon focusing them *)
  val on_display
    :  old:Model.t
    -> Model.t
    -> _ Derived_model.t
    -> unit

  (** Used to handle sort column clicking *)
  val apply_action : Model.t -> _ Derived_model.t -> Action.t -> Model.t

  val set_focus_row : Model.t -> Row_id.t option -> Model.t
  val set_focus_col : Model.t -> Column_id.t option -> Model.t

  (** Measures rows, table and viewport *)
  val update_visibility : Model.t -> _ Derived_model.t -> Model.t

  (** When constructing the row [Vdom.Node.t] (most likely using function [Vdom.Node.tr]),
      it is important to pass in the argument [~key:id]. Otherwise scrolling may have
      unexpected behavior. *)
  type 'a row_renderer
    =  row_id:Row_id.t
    -> row:'a Incr.t
    -> Row_node_spec.t Incr.t

  (** Returns a full partially-rendered <table> node with header. [render_row] function
      should render <tr> nodes. *)
  val view
    :  ?override_header_on_click:(Column_id.t -> Dom_html.mouseEvent Js.t -> Vdom.Event.t)
    -> Model.t Incr.t
    -> 'a Derived_model.t Incr.t
    -> render_row:'a row_renderer
    -> inject:(Action.t -> Vdom.Event.t)
    -> attrs:Vdom.Attr.t list
    -> Vdom.Node.t Incr.t
end

module type Table = sig
  (** The Table widget is used to create incremental partially-rendered tables. It
      provides sorting, focus and (possibly floating) table header support.

      Behind the scenes it uses [Partial_render_list] so it works with very large tables
      (e.g 10,000 rows). *)

  module type S = S
  module type Id = Id
  module Focus_dir = Focus_dir
  module Margin = Margin
  module Scroll_region = Scroll_region
  module Float_type = Float_type
  module Make (Row_id : Id) (Column_id : Id) (Sort_spec : Sort_spec)
    : (S with module Row_id = Row_id
          and module Column_id = Column_id
          and module Sort_spec = Sort_spec)
end

open Virtual_dom

(** A [Keyboard_event_handler.t] is a collections of actions (commands and disabled keys)
    that can be used to handle keyboard events and to produce help text.
*)

module Condition : sig
  (** A [Condition.t] is a condition based on a keyboard event (which importantly includes
      the event's focus).  This is intended to be used in keyboard event handlers that
      look at the keyboard event to determine whether or not to take a certain action.

      E.g. if a user presses 'j' while focused on a table, the event handler might handle
      this by moving the user's focus down by one row in the table, but if the user
      presses 'j' while typing into a text box, the event handler would ignore the event.
  *)

  type t = Keyboard_event.t -> bool

  val true_  : t
  val false_ : t

  val not_ : t -> t
  val and_ : t -> t -> t
  val or_  : t -> t -> t

  (** [has_text_input_target] returns true if the event target is a text input or textarea
      element *)
  val has_text_input_target : t

  (** [has_form_element_target] returns true if the event target is part of a form *)
  val has_form_element_target : t

  (** [has_target_id] returns true if the event target has the given id *)
  val has_target_id : id:string -> t
end

module Handler : sig
  (** A [Handler.t] handles a keyboard event by returning a [Vdom.Event.t]. These should
      be used as building blocks for keyboard event handlers, for instance to handle a
      specific set of keys.
  *)

  type t = Keyboard_event.t -> Vdom.Event.t [@@deriving sexp]

  (** [with_prevent_default t] handles the event using handler [t], and additionally
      prevents the default handler. *)
  val with_prevent_default : t -> t

  (** [only_handle_if ?prevent_default cond t] handles the event using handler [t] if
      [cond ev] evaluates to true (and additionally prevent the default handler if
      [prevent_default] is passed in), and does nothing otherwise. *)
  val only_handle_if
    :  ?prevent_default : unit
    -> Condition.t
    -> t
    -> t

  (** [handle_by_case] is similar to [only_handle_if], but allows the user to provide a
      list of handlers and their respective conditions instead of a single one. If
      multiple handlers' conditions evaluate to true, the first one is used. *)
  val handle_by_case
    :  ?prevent_default : unit
    -> (Condition.t * t) list
    -> t
end

module Command : sig
  type t =
    { keys        : Keystroke.t list
    ; description : string
    ; group       : Grouped_help_text.Group_name.t option
    ; handler     : Handler.t
    }

  val get_help_text : t -> Help_text.Command.t
end

module Action : sig
  type t =
    | Command      of Command.t
    (** A disabled key is essentially a command that prevents the default handler of
        that key (by returning [Event.Prevent_default]), and does nothing else.
        Users can choose to omit disabled keys from the help menu. *)
    | Disabled_key of Keystroke.t
  [@@deriving sexp, variants]

  val get_help_text : t -> Help_text.Command.t
end

type t [@@deriving sexp_of]

val empty : t

(** [of_action_list_exn] and [of_command_list_exn] create a keyboard event handler from a
    list of actions. If the same key appears in multiple actions, an exception is
    raised. *)
val of_action_list_exn  : Action.t  list -> t
val of_command_list_exn : Command.t list -> t

(** [add_action_exn], [add_command_exn], and [add_disabled_key_exn] add a new action to a
    keyboard event handler. If any key from the new action already exists in the handler,
    an exception is raised. *)
val add_action_exn       : t -> Action.t    -> t
val add_command_exn      : t -> Command.t   -> t
val add_disabled_key_exn : t -> Keystroke.t -> t

(** [set_action], [set_command], and [set_disabled_key] are similar to their respective
    [*_exn] functions, but if a key from the new action already exists in the handler,
    that key's action is updated to the new action. *)
val set_action       : t -> Action.t    -> t
val set_command      : t -> Command.t   -> t
val set_disabled_key : t -> Keystroke.t -> t

(** [merge_exn t1 t2] and [merge t1 t2] create a new keyboard event handler containing the
    actions from both [t1] and [t2]. If there is a duplicate key between [t1] and [t2],
    [merge_exn] raises an exception while [merge] uses the action from [t2]. *)
val merge_exn : t -> t -> t
val merge     : t -> t -> t

val handle_event : t -> Keyboard_event.t -> Vdom.Event.t option

val get_help_text
  :  ?include_disabled_keys : unit
  -> t
  -> Help_text.t

val get_grouped_help_text
  :  ?include_disabled_keys : unit
  -> t
  -> default_group          : Grouped_help_text.Group_name.t
  -> Grouped_help_text.t

val get_grouped_help_text_exn
  :  ?include_disabled_keys : unit
  -> t
  -> Grouped_help_text.t

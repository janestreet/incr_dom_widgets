open! Core_kernel
open Import
open Dom_html

module Keyboard_code = Keyboard_code

val key : keyboardEvent Js.t -> Keyboard_code.t

val ctrl  : keyboardEvent Js.t -> bool
val alt   : keyboardEvent Js.t -> bool
val shift : keyboardEvent Js.t -> bool
val meta  : keyboardEvent Js.t -> bool

(** [match_modifiers] evaluates a [keyboardEvent]'s modifiers vs the function's
    arguments. If an argument is not specified then that modifier is not evaluated. *)
val match_modifiers
  :  ?ctrl:bool
  -> ?alt:bool
  -> ?shift:bool
  -> ?meta:bool
  -> keyboardEvent Js.t
  -> bool

val no_modifiers : keyboardEvent Js.t -> bool

val map
  :  keyboardEvent Js.t
  -> f:([`Ctrl    of bool]
        * [`Alt   of bool]
        * [`Shift of bool]
        * [`Meta  of bool]
        * Keyboard_code.t
        -> 'a)
  -> 'a

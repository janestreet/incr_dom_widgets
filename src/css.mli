open Import

include (module type of Incr_dom_widgets_common.Css)

val to_attr : t -> Vdom.Attr.t

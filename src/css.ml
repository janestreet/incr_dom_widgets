open Import

include Incr_dom_widgets_common.Css

let to_attr t = Vdom.Attr.style (to_string_list t)

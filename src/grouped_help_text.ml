open Core_kernel
open Import

module Group_name = String

module View_spec = struct
  type t =
    { core_spec  : Help_text.View_spec.t
    ; group_name : Group_name.t -> Vdom.Node.t
    }

  let plain =
    { core_spec  = Help_text.View_spec.plain
    ; group_name = Vdom.Node.text
    }

  let with_classes ~group_name_class ~key_class ~plain_text_class =
    let text_div class_ text =
      let open Vdom in
      Node.div [ Attr.class_ class_ ] [ Node.text text ]
    in
    { core_spec  = Help_text.View_spec.with_classes ~key_class ~plain_text_class
    ; group_name = text_div group_name_class
    }
end

module Command = Help_text.Command

type t =
  { groups      : Help_text.t Group_name.Map.t
  ; group_order : Group_name.t list
  }
[@@deriving sexp, compare]

let empty      = { groups = Group_name.Map.empty ; group_order = [] }
let is_empty t = Map.is_empty t.groups

let of_group_list_exn group_list =
  { groups      = Group_name.Map.of_alist_exn group_list
  ; group_order = List.map group_list ~f:fst
  }

let add_group_exn t group_name commands =
  { groups      = Map.add_exn t.groups ~key:group_name ~data:commands
  ; group_order = t.group_order @ [ group_name ]
  }

let of_command_list command_list =
  let groups, group_order =
    List.fold command_list ~init:(Group_name.Map.empty, [])
      ~f:(fun (groups, group_order) (group_name, command) ->
        let group_order =
          if Map.mem groups group_name
          then group_order
          else (group_name :: group_order)
        in
        let groups =
          Map.update groups group_name ~f:(fun commands ->
            let commands = Option.value commands ~default:[] in
            command :: commands
          )
        in
        groups, group_order
      )
  in
  { groups      = Map.map groups
                    ~f:(fun commands -> Help_text.of_command_list (List.rev commands))
  ; group_order = List.rev group_order
  }

let add_command t group_name command =
  let group_order =
    if Map.mem t.groups group_name
    then t.group_order
    else (t.group_order @ [ group_name ])
  in
  let groups =
    Map.update t.groups group_name ~f:(fun help_text ->
      let help_text = Option.value help_text ~default:Help_text.empty in
      Help_text.add_command help_text command
    )
  in
  { groups ; group_order }

let groups t =
  List.map t.group_order ~f:(fun group_name ->
    group_name, Map.find_exn t.groups group_name
  )

let commands t =
  List.concat_map (groups t) ~f:(fun (group_name, help_text) ->
    List.map (Help_text.commands help_text) ~f:(fun command -> group_name, command)
  )

let view t (view_spec:View_spec.t) =
  let open Vdom in
  let rows =
    List.concat_map (groups t) ~f:(fun (group_name, help_text) ->
      let group_name_row =
        Node.tr []
          [ Node.td
              [ Attr.create "colspan" "2"
              ; Css.(text_align `Center |> to_attr)
              ]
              [ view_spec.group_name group_name ]
          ]
      in
      group_name_row :: Help_text.view_rows help_text view_spec.core_spec
    )
  in
  Vdom.Node.table [] rows

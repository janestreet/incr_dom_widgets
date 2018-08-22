open Incr_dom

let () =
  Start_app.component (module App) ~initial_model:(App.initial_model ())

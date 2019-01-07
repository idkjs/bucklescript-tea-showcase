open Tea
open Html

type msg =
  | GotHomeMsg of Page.Home.msg
  | GotSchoolsCreateMsg of Page.SchoolsCreate.msg
  | GotSchoolsMsg of Page.Schools.msg
  | OnUrlChange of Web.Location.location
[@@bs.deriving accessors]

type model =
  | NotFound
  | Home of Page.Home.model
  | SchoolsCreate of Page.SchoolsCreate.model
  | Schools of Page.Schools.model
[@@bs.deriving accessors]

let init_new_route location =
  match Routes.location_to_route location with
  | Some Routes.Home ->
      Page.Home.init () |> Util.map_update_sub home gotHomeMsg
  | Some (Routes.Schools {tab}) ->
      Page.Schools.init ~current_tab:tab location ()
      |> Util.map_update_sub schools gotSchoolsMsg
  | Some Routes.SchoolsCreate ->
      Page.SchoolsCreate.init ()
      |> Util.map_update_sub schoolsCreate gotSchoolsCreateMsg
  | None -> (NotFound, Cmd.none)

let init () location = init_new_route location

let update model msg =
  match (msg, model) with
  | OnUrlChange location, _ -> init_new_route location
  | GotHomeMsg sub_msg, Home model ->
      Page.Home.update model sub_msg |> Util.map_update_sub home gotHomeMsg
  | GotSchoolsCreateMsg sub_msg, SchoolsCreate model ->
      Page.SchoolsCreate.update model sub_msg
      |> Util.map_update_sub schoolsCreate gotSchoolsCreateMsg
  | GotSchoolsMsg sub_msg, Schools model ->
      Page.Schools.update model sub_msg
      |> Util.map_update_sub schools gotSchoolsMsg
  | _ -> (model, Cmd.none)

let view_button title msg = button [onClick msg] [text title]

let view model =
  match model with
  | Home model -> App.map gotHomeMsg @@ Page.Home.view model
  | SchoolsCreate model ->
      App.map gotSchoolsCreateMsg @@ Page.SchoolsCreate.view model
  | Schools model -> App.map gotSchoolsMsg @@ Page.Schools.view model
  | NotFound -> noNode

let subscriptions _module = Sub.none

let shutdown _module = Cmd.none

let main =
  Navigation.navigationProgram onUrlChange
    {init; update; view; subscriptions; shutdown}

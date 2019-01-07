open Tea

type model = {key: string; location: Web.Location.location; tabs: string list}

type msg =
  | ClearTabLabels
  | OnUrlChange of Web.Location.location
  | SetTab of string
  | SetTabLabels of string list
  | UpdateTabLabel of int * string

let init ?(tabs = []) ?(key = "tab") location () = {key; location; tabs}

let update model = function
  | ClearTabLabels -> ({model with tabs= []}, Cmd.none)
  | OnUrlChange location -> ({model with location}, Cmd.none)
  | SetTab label -> (
      let new_location =
        {model.location with search= "?" ^ model.key ^ "=" ^ label}
      in
      match new_location |> Routes.location_to_route with
      | None -> (model, Cmd.none)
      | Some route ->
          let url = route |> Routes.route_to_href in
          (model, Navigation.newUrl url) )
  | SetTabLabels tabs -> ({model with tabs}, Cmd.none)
  | UpdateTabLabel (i, label) ->
      ( { model with
          tabs=
            Belt.List.splitAt model.tabs i
            |. Belt.Option.map (fun (head, tail) ->
                   head @ [label]
                   @ (Belt.List.drop tail 1 |. Belt.Option.getWithDefault [])
               )
            |. Belt.Option.getWithDefault model.tabs }
      , Cmd.none )

type nested_msg = [`GotTabsMsg of msg]

let nested_update model = function
  | `GotTabsMsg sub_msg ->
      update model sub_msg
      |> Util.map_snd (fun cmd -> Cmd.map (fun msg -> `GotTabsMsg msg) cmd)

type schools_tab = [`UNIVERSITY | `OVERSEAS]

type schools_options = {tab: schools_tab option}

let school_tab_of_string : string -> schools_tab option = function
  | "university" -> Some `UNIVERSITY
  | "overseas" -> Some `OVERSEAS
  | _ -> None

let school_tab_to_string : schools_tab -> string = function
  | `UNIVERSITY -> "university"
  | `OVERSEAS -> "overseas"

type route = Home | Schools of schools_options | SchoolsCreate
[@@bs.deriving accessors]

let route_to_href (route : route) : string =
  let parts, search_dict =
    match route with
    | Home -> ([||], Js.Dict.empty ())
    | Schools {tab} -> (
        ( [|"schools"|]
        , match tab with
          | None -> Js.Dict.empty ()
          | Some `UNIVERSITY -> Js.Dict.fromList [("school-type", "university")]
          | Some `OVERSEAS -> Js.Dict.fromList [("school-type", "overseas")] )
        )
    | SchoolsCreate -> ([|"schools"; "new"|], Js.Dict.empty ())
  in
  let search =
    Js.Dict.entries search_dict
    |> Js.Array.map (fun (fst, snd) -> fst ^ "=" ^ snd)
    |> Js.Array.joinWith "&"
  in
  "/"
  ^ Js.Array.joinWith "/" parts
  ^ if String.length search = 0 then "" else "?" ^ search

let location_to_route (location : Web.Location.location) : route option =
  let parts, search_dict = Router.parse_location location in
  match parts with
  | [||] | [|""|] -> Some Home
  | [|"schools"|] ->
      Some
        (Schools
           { tab=
               Js.Dict.get search_dict "school-type"
               |. Belt.Option.flatMap school_tab_of_string })
  | [|"schools"; "new"|] -> Some SchoolsCreate
  | _ -> None

let href (route : route) : 'a Vdom.property =
  Tea.Html.href @@ route_to_href route

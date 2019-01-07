type url_request = Internal of string | External of string
[@@bs.deriving accessors]

let parse_location ({pathname; search} : Web.Location.location) :
    string array * string Js.Dict.t =
  let parts = Js.String.sliceToEnd ~from:1 pathname |> Js.String.split "/" in
  let search_dict =
    Js.String.sliceToEnd ~from:1 search
    |> Js.String.split "&"
    |. Belt.Array.map (Js.String.split "=")
    |. Belt.Array.keepMap (function
         | [|key|] when String.length key > 0 -> Some (key, "")
         | [|key; value|] when String.length key > 0 -> Some (key, value)
         | _ -> None )
    |> Js.Dict.fromArray
  in
  (parts, search_dict)

type target = {host: string; href: string}

let onClick ?(key = "") mgs =
  let open Tea.Json in
  let decoder =
    Decoder.Decoder
      (fun value ->
        match Web.Json.classify value with
        | JSONObject event ->
            Js.Dict.get event "target"
            |. Belt.Option.flatMap (fun target ->
                   let decoder =
                     Decoder.map2
                       (fun host href -> {host; href})
                       (Decoder.field "host" Decoder.string)
                       (Decoder.field "href" Decoder.string)
                   in
                   Tea.Result.ok @@ Decoder.decodeValue decoder target )
            |. Util.ok_or "Invalid event"
        | _ -> Tea.Result.Error "Invalid event" )
  in
  Tea.Html.onWithOptions ~key "click"
    {Tea.Html.defaultOptions with preventDefault= true}
  @@ ( decoder
     |> Decoder.andThen (fun {host; href} ->
            let run = Util.(Decoder.succeed % mgs) in
            let location = Tea.Navigation.getLocation () in
            if location.host = host then run @@ Internal href
            else run @@ External href ) )

external window_open :
     Web.Window.t
  -> string
  -> ([ `blank[@bs.as "_blank"]
      | `parent[@bs.as "_parent"]
      | `self[@bs.as "_self"]
      | `top[@bs.as "_top"] ][@bs.string])
  -> unit
  = "open"
  [@@bs.send]

let open_tab url =
  Tea_cmd.call (fun _enqueue ->
      window_open Web.Window.window url `blank ;
      Tea.Navigation.notifyUrlChange () )

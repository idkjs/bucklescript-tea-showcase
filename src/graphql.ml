exception Graphql_error of string

let graphql_server_url = "http://52.195.3.82/"

let request query =
  let open Tea.Http in
  let open Web.XMLHttpRequest in
  let body =
    Js.Dict.fromList
      [("query", Js.Json.string query##query); ("variables", query##variables)]
    |> Js.Json.object_ |> Js.Json.stringify |. StringBody
  in
  let headers = [Header ("content-type", "application/json")] in
  request
    { body
    ; expect= expectString
    ; headers
    ; method'= "POST"
    ; timeout= None
    ; url= graphql_server_url
    ; withCredentials= false }

let send_query query =
  let open Bs_fetch in
  let open Js.Promise in
  let body =
    Js.Dict.fromList
      [("query", Js.Json.string query##query); ("variables", query##variables)]
    |> Js.Json.object_ |> Js.Json.stringify |> BodyInit.make
  in
  let headers =
    HeadersInit.makeWithArray [|("content-type", "application/json")|]
  in
  fetchWithInit graphql_server_url
  @@ RequestInit.make ~method_:Post ~body ~headers ()
  |> then_ (fun response ->
         if not @@ Response.ok response then
           reject
           @@ Graphql_error ("Request failed: " ^ Response.statusText response)
         else
           Response.json response
           |> then_ (fun data ->
                  let obj =
                    Js.Json.decodeObject data
                    |. Belt.Option.flatMap
                         Util.(
                           (flip Belt.Option.map) query##parse
                           % (flip Js.Dict.get) "data")
                  in
                  match obj with
                  (* Due to the eager nature of Js.Promise.reject we can't use Belt.Option.mapWithDefault here *)
                  | None -> reject @@ Graphql_error "Response is not an object"
                  | Some obj -> resolve obj ) )

let decode parser data =
  data |. Util.Json.parse
  |. Belt.Option.flatMap Js.Json.decodeObject
  |. Belt.Option.flatMap
       Util.((flip Belt.Option.map) parser % (flip Js.Dict.get) "data")

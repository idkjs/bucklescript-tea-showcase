exception Graphql_error(string);

let graphql_server_url = "http://52.195.3.82/";

let request = query => {
  open Tea.Http;
  open Web.XMLHttpRequest;
  let body =
    (
      Js.Dict.fromList([
        ("query", Js.Json.string(query##query)),
        ("variables", query##variables),
      ])
      |> Js.Json.object_
      |> Js.Json.stringify
    )
    ->StringBody;

  let headers = [
    [@implicit_arity] Header("content-type", "application/json"),
  ];
  request({
    body,
    expect: expectString,
    headers,
    method': "POST",
    timeout: None,
    url: graphql_server_url,
    withCredentials: false,
  });
};

let send_query = query => {
  open Bs_fetch;
  open Js.Promise;
  let body =
    Js.Dict.fromList([
      ("query", Js.Json.string(query##query)),
      ("variables", query##variables),
    ])
    |> Js.Json.object_
    |> Js.Json.stringify
    |> BodyInit.make;

  let headers =
    HeadersInit.makeWithArray([|("content-type", "application/json")|]);

  fetchWithInit(graphql_server_url) @@
  RequestInit.make(~method_=Post, ~body, ~headers, ())
  |> then_(response =>
       if ((!) @@ Response.ok(response)) {
         reject @@
         Graphql_error("Request failed: " ++ Response.statusText(response));
       } else {
         Response.json(response)
         |> then_(data =>
              let obj =
                Js.Json.decodeObject(data)
                ->(
                    Belt.Option.flatMap(
                      Util.(
                        (flip(Belt.Option.map))(query##parse)
                        % (flip(Js.Dict.get))("data")
                      ),
                    )
                  );

              switch (obj) {
              /* Due to the eager nature of Js.Promise.reject we can't use Belt.Option.mapWithDefault here */
              | None => reject @@ Graphql_error("Response is not an object")
              | Some(obj) => resolve(obj)
              };
            );
       }
     );
};

let decode = (parser, data) =>
  data
  ->Util.Json.parse
  ->(Belt.Option.flatMap(Js.Json.decodeObject))
  ->(
      Belt.Option.flatMap(
        Util.(
          (flip(Belt.Option.map))(parser) % (flip(Js.Dict.get))("data")
        ),
      )
    );

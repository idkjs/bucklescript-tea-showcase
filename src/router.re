[@bs.deriving accessors]
type url_request =
  | Internal(string)
  | External(string);

let parse_location =
    ({pathname, search}: Web.Location.location)
    : (array(string), Js.Dict.t(string)) => {
  let parts =
    Js.String.sliceToEnd(~from=1, pathname) |> Js.String.split("/");
  let search_dict =
    (Js.String.sliceToEnd(~from=1, search) |> Js.String.split("&"))
    ->(Belt.Array.map(Js.String.split("=")))
    ->(
        Belt.Array.keepMap(
          fun
          | [|key|] when String.length(key) > 0 => Some((key, ""))
          | [|key, value|] when String.length(key) > 0 => Some((key, value))
          | _ => None,
        )
      )
    |> Js.Dict.fromArray;

  (parts, search_dict);
};

type target = {
  host: string,
  href: string,
};

let onClick = (~key="", mgs) => {
  open Tea.Json;
  let decoder =
    Decoder.Decoder(
      value =>
        switch (Web.Json.classify(value)) {
        | JSONObject(event) =>
          Js.Dict.get(event, "target")
          ->(
              Belt.Option.flatMap(target =>
                let decoder =
                  Decoder.map2(
                    (host, href) => {host, href},
                    Decoder.field("host", Decoder.string),
                    Decoder.field("href", Decoder.string),
                  );

                Tea.Result.ok @@ Decoder.decodeValue(decoder, target);
              )
            )
          ->(Util.ok_or("Invalid event"))
        | _ => Tea.Result.Error("Invalid event")
        },
    );

  Tea.Html.onWithOptions(
    ~key,
    "click",
    {...Tea.Html.defaultOptions, preventDefault: true},
  ) @@
  (
    decoder
    |> Decoder.andThen(({host, href}) =>
         let run = Util.(Decoder.succeed % mgs);
         let location = Tea.Navigation.getLocation();
         if (location.host == host) {
           run @@ Internal(href);
         } else {
           run @@ External(href);
         };
       )
  );
};

[@bs.send]
external window_open:
  (
    Web.Window.t,
    string,
    [@bs.string] [
      | [@bs.as "_blank"] `blank
      | [@bs.as "_parent"] `parent
      | [@bs.as "_self"] `self
      | [@bs.as "_top"] `top
    ]
  ) =>
  unit =
  "open";

let open_tab = url =>
  Tea_cmd.call(_enqueue => {
    window_open(Web.Window.window, url, `blank);
    Tea.Navigation.notifyUrlChange();
  });

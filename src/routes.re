type schools_tab = [ | `UNIVERSITY | `OVERSEAS];

type schools_options = {tab: option(schools_tab)};

let school_tab_of_string: string => option(schools_tab) = (
  fun
  | "university" => Some(`UNIVERSITY)
  | "overseas" => Some(`OVERSEAS)
  | _ => None:
    string => option(schools_tab)
);

let school_tab_to_string: schools_tab => string = (
  fun
  | `UNIVERSITY => "university"
  | `OVERSEAS => "overseas":
    schools_tab => string
);

[@bs.deriving accessors]
type route =
  | Home
  | Schools(schools_options)
  | SchoolsCreate;

let route_to_href = (route: route): string => {
  let (parts, search_dict) =
    switch (route) {
    | Home => ([||], Js.Dict.empty())
    | Schools({tab}) => (
        [|"schools"|],
        switch (tab) {
        | None => Js.Dict.empty()
        | Some(`UNIVERSITY) =>
          Js.Dict.fromList([("school-type", "university")])
        | Some(`OVERSEAS) => Js.Dict.fromList([("school-type", "overseas")])
        },
      )
    | SchoolsCreate => ([|"schools", "new"|], Js.Dict.empty())
    };

  let search =
    Js.Dict.entries(search_dict)
    |> Js.Array.map(((fst, snd)) => fst ++ "=" ++ snd)
    |> Js.Array.joinWith("&");

  "/"
  ++ Js.Array.joinWith("/", parts)
  ++ (
    if (String.length(search) == 0) {
      "";
    } else {
      "?" ++ search;
    }
  );
};

let location_to_route = (location: Web.Location.location): option(route) => {
  let (parts, search_dict) = Router.parse_location(location);
  switch (parts) {
  | [||]
  | [|""|] => Some(Home)
  | [|"schools"|] =>
    Some(
      Schools({
        tab:
          Js.Dict.get(search_dict, "school-type")
          ->(Belt.Option.flatMap(school_tab_of_string)),
      }),
    )
  | [|"schools", "new"|] => Some(SchoolsCreate)
  | _ => None
  };
};

let href = (route: route): Vdom.property('a) =>
  Tea.Html.href @@ route_to_href(route);

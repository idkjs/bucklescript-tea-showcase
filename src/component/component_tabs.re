open Tea;

type model = {
  key: string,
  location: Web.Location.location,
  tabs: list(string),
};

type msg =
  | ClearTabLabels
  | OnUrlChange(Web.Location.location)
  | SetTab(string)
  | SetTabLabels(list(string))
  | UpdateTabLabel(int, string);

let init = (~tabs=[], ~key="tab", location, ()) => {key, location, tabs};

let update = model =>
  fun
  | ClearTabLabels => ({...model, tabs: []}, Cmd.none)
  | OnUrlChange(location) => ({...model, location}, Cmd.none)
  | SetTab(label) => {
      let new_location = {
        ...model.location,
        search: "?" ++ model.key ++ "=" ++ label,
      };

      switch (new_location |> Routes.location_to_route) {
      | None => (model, Cmd.none)
      | Some(route) =>
        let url = route |> Routes.route_to_href;
        (model, Navigation.newUrl(url));
      };
    }
  | SetTabLabels(tabs) => ({...model, tabs}, Cmd.none)
  | [@implicit_arity] UpdateTabLabel(i, label) => (
      {
        ...model,
        tabs:
          Belt.List.splitAt(model.tabs, i)
          ->(
              Belt.Option.map(((head, tail)) =>
                head
                @ [label]
                @ Belt.List.drop(tail, 1)->(Belt.Option.getWithDefault([]))
              )
            )
          ->(Belt.Option.getWithDefault(model.tabs)),
      },
      Cmd.none,
    );

type nested_msg = [ | `GotTabsMsg(msg)];

let nested_update = model =>
  fun
  | `GotTabsMsg(sub_msg) =>
    update(model, sub_msg)
    |> Util.map_snd(cmd => Cmd.map(msg => `GotTabsMsg(msg), cmd));

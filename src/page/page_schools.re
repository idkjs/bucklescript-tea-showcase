open Tea;
open Html;
open Component;
open Partial;

module SchoolsQuery = [%graphql
  {|
  query Schools($school_type: SchoolType!) {
    schools(keyword: "", schoolType: $school_type) {
      id
      name
    }
  }
|}
];

type model = {
  current_tab: option(Routes.schools_tab),
  data: option(SchoolsQuery.t),
  location: Web.Location.location,
  tabs: Tabs.model,
};

let init = (~current_tab=None, location, ()) => {
  let school_type = current_tab->(Belt.Option.getWithDefault(`UNIVERSITY));
  let query = SchoolsQuery.make(~school_type, ());
  let cmd =
    Http.send(data => `ReceivedSchools(data)) @@ Graphql.request(query);

  (
    {
      current_tab,
      data: None,
      location,
      tabs:
        Tabs.init(
          ~key="school-type",
          ~tabs=["university", "overseas"],
          location,
          (),
        ),
    },
    cmd,
  );
};

type msg = [
  UrlRequest.msg
  | Tabs.nested_msg
  | `ReceivedSchools(Result.t(string, Tea.Http.error(string)))
];

let update = ({tabs} as model) =>
  fun
  | #UrlRequest.msg as msg => (model, UrlRequest.update(msg))
  | #Tabs.nested_msg as msg => {
      let (tabs, cmd) = Tabs.nested_update(tabs, msg);
      ({...model, tabs}, cmd);
    }
  | `ReceivedSchools(Result.Error(_)) => (model, Cmd.none)
  | `ReceivedSchools(Result.Ok(data)) => {
      let school_type =
        model.current_tab->(Belt.Option.getWithDefault(`UNIVERSITY));

      let query = SchoolsQuery.make(~school_type, ());
      let data = Graphql.decode(query##parse, data);
      ({...model, data}, Cmd.none);
    };

let view_button = (title, msg) => button([onClick(msg)], [text(title)]);

let view = ({current_tab, data, location}) =>
  div(
    [],
    [
      h1([], [text("Schools")]),
      div([], [text(location.search)]),
      br([]),
      switch (current_tab) {
      | None => noNode
      | Some(`UNIVERSITY) => div([], [text("university")])
      | Some(`OVERSEAS) => div([], [text("overseas")])
      },
      br([]),
      a(
        [
          Routes.href(Routes.Home),
          Router.onClick(url => `OnUrlRequest(url)),
        ],
        [text("Go to home")],
      ),
      br([]),
      a(
        [
          href("https://www.google.com"),
          Router.onClick(url => `OnUrlRequest(url)),
        ],
        [text("Go Google")],
      ),
      br([]),
      button(
        [onClick(`GotTabsMsg(Tabs.SetTab("university")))],
        [text("Universities")],
      ),
      button(
        [onClick(`GotTabsMsg(Tabs.SetTab("overseas")))],
        [text("Overseas")],
      ),
      br([]),
      div(
        [],
        data
        ->(
            Belt.Option.map(data =>
              data##schools
              ->Belt.List.fromArray
              ->(Belt.List.map(school => p([], [text(school##name)])))
            )
          )
        ->(Belt.Option.getWithDefault([p([], [text("loading...")])])),
      ),
    ],
  );

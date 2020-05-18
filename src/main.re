open Tea;
open Html;

[@bs.deriving accessors]
type msg =
  | GotHomeMsg(Page.Home.msg)
  | GotSchoolsCreateMsg(Page.SchoolsCreate.msg)
  | GotSchoolsMsg(Page.Schools.msg)
  | OnUrlChange(Web.Location.location);

[@bs.deriving accessors]
type model =
  | NotFound
  | Home(Page.Home.model)
  | SchoolsCreate(Page.SchoolsCreate.model)
  | Schools(Page.Schools.model);

let init_new_route = location =>
  switch (Routes.location_to_route(location)) {
  | Some(Routes.Home) =>
    Page.Home.init() |> Util.map_update_sub(home, gotHomeMsg)
  | Some(Routes.Schools({tab})) =>
    Page.Schools.init(~current_tab=tab, location, ())
    |> Util.map_update_sub(schools, gotSchoolsMsg)
  | Some(Routes.SchoolsCreate) =>
    Page.SchoolsCreate.init()
    |> Util.map_update_sub(schoolsCreate, gotSchoolsCreateMsg)
  | None => (NotFound, Cmd.none)
  };

let init = ((), location) => init_new_route(location);

let update = (model, msg) =>
  switch (msg, model) {
  | (OnUrlChange(location), _) => init_new_route(location)
  | (GotHomeMsg(sub_msg), Home(model)) =>
    Page.Home.update(model, sub_msg) |> Util.map_update_sub(home, gotHomeMsg)
  | (GotSchoolsCreateMsg(sub_msg), SchoolsCreate(model)) =>
    Page.SchoolsCreate.update(model, sub_msg)
    |> Util.map_update_sub(schoolsCreate, gotSchoolsCreateMsg)
  | (GotSchoolsMsg(sub_msg), Schools(model)) =>
    Page.Schools.update(model, sub_msg)
    |> Util.map_update_sub(schools, gotSchoolsMsg)
  | _ => (model, Cmd.none)
  };

let view_button = (title, msg) => button([onClick(msg)], [text(title)]);

let view = model =>
  switch (model) {
  | Home(model) => App.map(gotHomeMsg) @@ Page.Home.view(model)
  | SchoolsCreate(model) =>
    App.map(gotSchoolsCreateMsg) @@ Page.SchoolsCreate.view(model)
  | Schools(model) => App.map(gotSchoolsMsg) @@ Page.Schools.view(model)
  | NotFound => noNode
  };

let subscriptions = _module => Sub.none;

let shutdown = _module => Cmd.none;

let main =
  Navigation.navigationProgram(
    onUrlChange,
    {init, update, view, subscriptions, shutdown},
  );

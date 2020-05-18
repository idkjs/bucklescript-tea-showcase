open Tea;
open Html;
open Partial;

type model = {counter: int};

let init = () => ({counter: 0}, Cmd.none);

type msg = [ UrlRequest.msg | `Increment | `Decrement | `Reset | `Set(int)];

let update = ({counter} as model) =>
  fun
  | #UrlRequest.msg as msg => (model, UrlRequest.update(msg))
  | `Increment => ({counter: counter + 1}, Cmd.none)
  | `Decrement => ({counter: counter - 1}, Cmd.none)
  | `Reset => ({counter: 0}, Cmd.none)
  | `Set(counter) => ({counter: counter}, Cmd.none);

let view_button = (title, msg) => button([onClick(msg)], [text(title)]);

let view = ({counter}) =>
  div(
    [],
    [
      h1([], [text("Home")]),
      a(
        [
          Routes.href @@ Routes.Schools({tab: None}),
          Router.onClick(request => `OnUrlRequest(request)),
        ],
        [text("Go to schools")],
      ),
      br([]),
      a(
        [
          href("https://www.google.com"),
          Router.onClick(request => `OnUrlRequest(request)),
        ],
        [text("Go Google")],
      ),
      br([]),
      span(
        [style("text-weight", "bold")],
        [text(string_of_int(counter))],
      ),
      br([]),
      view_button("Increment", `Increment),
      br([]),
      view_button("Decrement", `Decrement),
      br([]),
      view_button("Set to 42", `Set(42)),
      br([]),
      if (counter != 0) {
        view_button("Reset", `Reset);
      } else {
        noNode;
      },
    ],
  );

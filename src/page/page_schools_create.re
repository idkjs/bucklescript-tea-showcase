open Tea;
open Html;

[@bs.deriving accessors]
type fields_msg =
  | Name(string)
  | Age(string)
  | Hobbies(int, string);

[@bs.deriving accessors]
type msg =
  | AddHobby
  | Fields(fields_msg);

type fields = {
  age: option(int),
  hobbies: list(string),
  name: string,
};

type model = {fields};

let init = () => (
  {
    fields: {
      age: None,
      hobbies: [""],
      name: "",
    },
  },
  Cmd.none,
);

let update = model =>
  fun
  | AddHobby => (
      {
        fields: {
          ...model.fields,
          hobbies: model.fields.hobbies @ [""],
        },
      },
      Cmd.none,
    )
  | Fields(Name(name)) => ({
                               fields: {
                                 ...model.fields,
                                 name,
                               },
                             }, Cmd.none)
  | Fields(Age(age)) => (
      {
        fields: {
          ...model.fields,
          age: Util.int_of_string_opt(age),
        },
      },
      Cmd.none,
    )
  | Fields([@implicit_arity] Hobbies(_i, _hobby)) => (
      {
        fields: {
          ...model.fields,
          hobbies: ["foo"],
        },
      },
      Cmd.none,
    );

let view = model =>
  Util.(
    div(
      [],
      [
        h1([], [text("Schools create")]),
        text(
          model.fields.age
          ->(Belt.Option.map(string_of_int))
          ->(Belt.Option.getWithDefault("no age")),
        ),
        text @@ model.fields.name,
        text @@
        model.fields.hobbies
        ->Belt.List.head
        ->(Belt.Option.getWithDefault("no hobbies")),
        form(
          [],
          [
            input'([onInput(fields % name)], []),
            input'([onInput(fields % age)], []),
            button(
              [onClick(addHobby), type'("button")],
              [text("add hobby")],
            ),
            div(
              [],
              model.fields.hobbies
              ->(
                  Belt.List.mapWithIndex((i, _) =>
                    input'(
                      [onInput(fields % hobbies(i))],
                      [text @@ string_of_int(i)],
                    )
                  )
                ),
            ),
          ],
        ),
      ],
    )
  );

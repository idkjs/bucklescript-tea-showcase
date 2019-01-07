open Tea
open Html

type fields_msg = Name of string | Age of string | Hobbies of int * string [@@bs.deriving accessors]

type msg = AddHobby | Fields of fields_msg [@@bs.deriving accessors]

type fields = {age: int option; hobbies: string list; name: string}

type model = {fields: fields}

let init () = ({fields= {age= None; hobbies= [""]; name= ""}}, Cmd.none)

let update model = function
  | AddHobby -> ({fields= {model.fields with hobbies= (model.fields.hobbies @ [""])}}, Cmd.none)
  | Fields (Name name) -> ({fields= {model.fields with name}}, Cmd.none)
  | Fields (Age age) ->
      ({fields= {model.fields with age= Util.int_of_string_opt age}}, Cmd.none)
  | Fields (Hobbies (_i, _hobby)) ->
      ({fields= {model.fields with hobbies= ["foo"]}}, Cmd.none)

let view model =
  Util.(
    div []
      [ h1 [] [text "Schools create"]
      ; text
          ( model.fields.age
          |. Belt.Option.map string_of_int
          |. Belt.Option.getWithDefault "no age" )
      ; text @@ model.fields.name
      ; text @@ ( model.fields.hobbies |. Belt.List.head |. Belt.Option.getWithDefault "no hobbies" )
      ; form []
          [ input' [onInput (fields % name)] []
          ; input' [onInput (fields % age)] []
          ; button [onClick addHobby; type' "button"] [text "add hobby"]
          ; div []
              ( model.fields.hobbies
              |. Belt.List.mapWithIndex (fun i _ ->
                     input' [onInput (fields % hobbies i)] [text @@ string_of_int i] ) ) ] ])

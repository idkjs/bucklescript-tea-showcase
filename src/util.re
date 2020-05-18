let ok_or = (opt: option('a), error: 'b): Tea.Result.t('a, 'b) =>
  switch (opt) {
  | Some(x) => Tea.Result.Ok(x)
  | None => Tea.Result.Error(error)
  };

let map_snd = (f, (fst, snd)) => (fst, f(snd));

let map_update_sub =
    (
      map_model: 'a => 'b,
      map_msg: 'c => 'd,
      (fst, snd): ('a, Tea.Cmd.t('c)),
    )
    : ('b, Tea.Cmd.t('d)) => (
  map_model(fst),
  Tea.Cmd.map(map_msg, snd),
);

let (%) = (f: 'b => 'c, g: 'a => 'b, x: 'a): 'c => f @@ g(x);

let const = (x: 'a, _: 'b): 'a => x;

let flip = (f: ('a, 'b) => 'c, x: 'b, y: 'a): 'c => f(y, x);

let int_of_string_opt = (s: string): option(int) =>
  try(int_of_string(s)->Some) {
  | _ => None
  };

module Json = {
  let parse = (s: string): option(Js.Json.t) =>
    try(Js.Json.parseExn(s)->Some) {
    | _ => None
    };
};

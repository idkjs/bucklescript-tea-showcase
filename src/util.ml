let ok_or (opt : 'a option) (error : 'b) : ('a, 'b) Tea.Result.t =
  match opt with Some x -> Tea.Result.Ok x | None -> Tea.Result.Error error

let map_snd f (fst, snd) = (fst, f snd)

let map_update_sub (map_model : 'a -> 'b) (map_msg : 'c -> 'd)
    ((fst, snd) : 'a * 'c Tea.Cmd.t) : 'b * 'd Tea.Cmd.t =
  (map_model fst, Tea.Cmd.map map_msg snd)

let ( % ) (f : 'b -> 'c) (g : 'a -> 'b) (x : 'a) : 'c = f @@ g x

let const (x : 'a) (_ : 'b) : 'a = x

let flip (f : 'a -> 'b -> 'c) (x : 'b) (y : 'a) : 'c = f y x

let int_of_string_opt (s : string) : int option =
  try int_of_string s |. Some with _ -> None

module Json = struct
  let parse (s : string) : Js.Json.t option =
    try Js.Json.parseExn s |. Some with _ -> None
end

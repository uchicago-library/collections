open Prelude

module type QUERYSTRING = sig
  module Field : Prelude.OrderedType
  module Dict : module type of Map.Make (Field)
  type value = string
  type t = value list Dict.t
  val to_list : 'a Dict.t -> (Field.t * 'a) list
  val of_list : (Field.t * 'a) list -> 'a Dict.t
end

module Querystring : QUERYSTRING
       with module Field = String
        and type value = string = struct
  module Field = String
  module Dict = Map.Make (Field)
  type value = string
  type t = value list Dict.t
  let to_list = Dict.to_list
  let of_list dct = Dict.of_list Dict.empty dct
end

module Fetch = struct
  let fetch url =
    let open Lwt.Infix in
    let stringify (_, body) = Cohttp_lwt.Body.to_string body in
    Cohttp_lwt_unix.Client.get (Uri.of_string url) >>= stringify
    |> Lwt_main.run
end




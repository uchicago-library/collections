open Prelude

module type PRINTING = sig
  val to_string : Json_repr.ezjsonm -> string
  val jprint : ?truncate:int -> Json_repr.ezjsonm -> unit
end

module Printing : PRINTING = struct
  let to_string json =
    let () = Json_repr.pp
               (module Json_repr.Ezjsonm)
               Format.str_formatter
               json
    in Format.flush_str_formatter ()

  let print_entire json =
    Format.printf
      "%a"
      (Json_repr.pp (module Json_repr.Ezjsonm))
      json

  let jprint ?truncate json =
    match truncate with
    | Some n -> Prelude.print
                @@ String.take n
                @@ to_string json
    | None -> print_entire json
end

include Printing

module Example = struct
  let json_string = {|{"name":"Watermelon","id":25,"family":"Cucurbitaceae","order":"Cucurbitales","genus":"Citrullus","nutritions":{"calories":30,"fat":0.2,"sugar":6.0,"carbohydrates":8.0,"protein":0.6}}|}

  let json = Ezjsonm.from_string json_string
end

         

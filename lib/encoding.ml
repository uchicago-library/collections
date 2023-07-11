module type PRINTING = sig
  val to_string : Json_repr.ezjsonm -> string
  val print : ?truncate:int -> Json_repr.ezjsonm -> unit
end


module Printing = struct
  let enc_to_schema enc =
    enc
    |> Json_encoding.schema
    |> Json_schema.to_json

  let to_string enc =
    enc
    |> enc_to_schema
    |> Json.Printing.to_string

  let print enc =
    enc
    |> enc_to_schema
    |> Json.Printing.print
end

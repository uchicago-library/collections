module Json = struct
  let to_string json =
    let () = Data_encoding.Json.pp
               Format.str_formatter
               json
    in Format.flush_str_formatter ()

  let print_entire json =
    Format.printf
      "%a"
      (Json_repr.pp (module Json_repr.Ezjsonm))
      json

  let print ?truncate json =
    match truncate with
    | Some n -> Prelude.print
                @@ Prelude.String.take n
                @@ to_string json
    | None -> print_entire json
end

module Json_string = struct
  let print_entire str =
    match Ezjsonm.from_string str with
    | exception _ -> Printf.printf "invalid json"
    | json -> Json.print_entire json

  let print ?truncate str =
    match truncate with
    | Some n -> Prelude.print
                @@ Prelude.String.take n str
    | None -> print_entire str
end

module Encoding = struct
  let enc_to_schema enc =
    enc
    |> Data_encoding.Json.schema
    |> Json_schema.to_json

  let to_string enc =
    enc
    |> enc_to_schema
    |> Data_encoding.Json.to_string

  let print ?truncate enc =
    enc
    |> enc_to_schema
    |> Json.print ?truncate
end

module Error = struct
  let to_string exn =
    let () = Data_encoding.Json.print_error
               Format.str_formatter
               exn
    in Format.flush_str_formatter ()
end

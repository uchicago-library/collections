open Prelude

module type PRINTING = sig
  val to_string : Json_repr.ezjsonm -> string
  val print : ?truncate:int -> Json_repr.ezjsonm -> unit
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

  let print ?truncate json =
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

  let marklogic_string = {|{"head":{"vars":["identifier","content","type","soundFile","contributors","creators","dates","description","languages","titles","accessRights","alternatives","locations","contentType","olacLinguisticDataType","dmaTitles","discourseType"]},"results":{"bindings":[{"identifier":{"type":"literal","value":"https://n2t.net/ark:61001/b2k40qk4wc8h"},"soundFile":{"type":"uri","value":"https://ark.lib.uchicago.edu/ark:61001/b2d75mb5kv6w"},"contributors":{"type":"literal","value":""},"creators":{"type":"literal","value":""},"dates":{"type":"literal","value":"2008-04-29|2008"},"languages":{"type":"literal","value":""},"titles":{"type":"literal","value":""},"alternatives":{"type":"literal","value":""},"locations":{"type":"literal","value":"(:unav)"},"dmaTitles":{"type":"literal","value":""}}]}}|}

  let marklogic = Ezjsonm.from_string marklogic_string
end

         

open Prelude
module D = Defaults
module R = Mattlude.Endofunctors.Result.Make (String)

module Params = struct
  include Url.Querystring

  let endpoint_name = "getResultsByKeyword"

  let make
        ?(collection=D.collection)
        ?identifier
        ?(search=D.search)
        () =
    let _ = identifier in
    let qs = [ ("collection", [collection]) ;
               ("search", [search]) ; ]
    in Dict.of_list Dict.empty qs

  let to_list = Dict.to_list 
end

let endpoint_name = Params.endpoint_name

include Utils.Fetcher (Params) (D)

module Parse = struct
  open Data_encoding
  open Utils.Encoding 

  module Encoding = struct
    let each_binding_enc =
      obj1 @@
        (req "resource" 
           (obj2 (req "type" string)
           (req "value" string)))
    let enc = bindings_to_enc each_binding_enc
  end

  let parse = trap Encoding.enc
  let schema = Utils.Schema.schema Encoding.enc
end

module Transform = struct
  let assoc_res key alist =
    Utils.Transform.assoc_res
      "empty string"
      key
      alist

  let get_noid url =
    match String.split ~sep:"/" url with
    | [] -> Error "empty string"
    | lst -> Ok (lst |> rev |> hd)

  let transform (_, results) =
    let open R in
    results
    |> assoc_res "bindings"
    >>| List.map (get_noid << snd)
    >>| Result.reduce
end

module Export = struct
  open Data_encoding

  module Encoding = struct
    let enc = list string
  end

  let un_result = function
    | Ok json -> `O [("ok", json)]
    | Error msg -> `O [("error", `String msg)]

  (* TODO: refactor this part *)
  let export json_result =
    let open R in
    json_result
    >>| Json.construct Encoding.enc
    |> un_result

  let schema = Utils.Schema.schema Encoding.enc
end

module Gimme = struct
  open Utils.Debug
  let gimme
        ?(debug=DebugOff)
        ?(group=D.group)
        ?(collection=D.collection)
        ?identifier
        ?(search=D.search)
        () =
    let open R in 
    let _ = identifier in
    match debug with
    | Curl ->
       Url.url ~group ~collection ~search ()
    | Raw ->
       Fetch.fetch ~group ~collection ~search ()
       >>= Utils.Ezjsonm.ezjsonm
       |> Export.un_result
       |> Printing.Json.to_string
    | DebugOff ->
       Fetch.fetch ~group ~collection ~search ()
       >>= Utils.Ezjsonm.ezjsonm
       >>= Parse.parse
       >>= Transform.transform
       |> Export.export
       |> Printing.Json.to_string
end
include Gimme

module Spec = struct
  let qs_fields = [ "group"; "collection"; "search" ]
  let spec = Utils.Spec.mk_spec qs_fields
end

module Subservice = struct
  let subservice cgi _ _ =
    let open Restful in
    let open Param in
    let ps = process cgi Spec.spec in
    let group = (value ps "group") in
    let collection = (value ps "collection") in
    let search = (value ps "search") in
    Content.write ~content_type:"text/plain" cgi
                  (gimme ~group ~collection ~search ())
end
include Subservice

module Schema = struct
  open Utils.Schema
  let input_schema = Parse.schema
  let output_schema = Export.schema

  let input_path =
    sprintf
      "%s/%s/%s.json"
      schemas_root
      "input"
      endpoint_name

  let output_path =
    sprintf
      "%s/%s/%s.json"
      schemas_root
      "output"
      endpoint_name

  let input_write () =
    writefile ~fn:input_path input_schema

  let output_write () =
    writefile ~fn:output_path output_schema
end

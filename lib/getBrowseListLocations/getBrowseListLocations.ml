open Prelude
module D = Defaults
module R = Mattlude.Endofunctors.Result.Make (String)

module Params = struct
  include Url.Querystring

  let endpoint_name = "getBrowseListLocations"

  let make
        ?(collection=D.collection)
        ?identifier
        ?search
        () =
    let _ = identifier in
    let _ = search in
    let qs = [ ("collection", [collection]) ]
    in Dict.of_list Dict.empty qs

  let to_list = Dict.to_list 
end

let endpoint_name = Params.endpoint_name

include Utils.Fetcher (Params) (D)

module Parse = struct
  open Data_encoding
  open Utils.Encoding 

  module Encoding = struct
    let bindings_enc =
      obj2
        (req "prefLabel" @@
           obj3
             (opt "xml:lang" string)
             (req "type" string)
             (req "value" string))
        (req "spatial" @@
           obj2
             (req "type" string)
             (req "value" string))
      
    let enc = bindings_to_enc bindings_enc
  end

  let parse = trap Encoding.enc
  let schema = Utils.Schema.schema Encoding.enc
end

module Transform = struct
  let assoc_res alist =
    Utils.Transform.assoc_res
      "error preprocessing Mark Logic response"
      alist

  (* TODO: fix_none will convert all empty language fields to English;
     we should confirm with the metadata people that that is
     correct. *)
  let fix_none ((opt,l1,value), keypair) =
    match opt with
    | None -> (("en",l1,value), keypair)
    | Some s -> (s,l1,value), keypair

  let fix_languages alist =
    let open List.Assoc in 
    let reducer acc ((lang_lang, _, lang_name),
                     (_, code)) =
      add code (lang_lang, lang_name) acc in
    coalesce (foldl reducer [] alist)

  let transform tup =
    let open R in
    tup
    |> snd
    |> assoc_res "bindings"
    >>| List.map fix_none
    >>| fix_languages
end


module Export = struct
  open Data_encoding

  module Encoding = struct
    let enc = assoc (assoc string)
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
        ?search
        () =
    let open R in 
    let _ = identifier in
    let _ = search in
    match debug with
    | Curl ->
       Url.url ~group ~collection ()
    | Raw ->
       Fetch.fetch ~group ~collection ()
       >>= Utils.Ezjsonm.ezjsonm
       |> Export.un_result
       |> Printing.Json.to_string
    | DebugOff ->
       Fetch.fetch ~group ~collection ()
       >>= Utils.Ezjsonm.ezjsonm
       >>= Parse.parse
       >>= Transform.transform
       |> Export.export
       |> Printing.Json.to_string
end
include Gimme

module Spec = struct
  let qs_fields = [ "group"; "collection" ]
  let spec = Utils.Spec.mk_spec qs_fields
end

module Subservice = struct
  let subservice cgi _ _ =
    let open Restful in
    let open Param in
    let ps = process cgi Spec.spec in
    let group = (value ps "group") in
    let collection = (value ps "collection") in
    Content.write ~content_type:"text/plain" cgi
                  (gimme ~group ~collection ())
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

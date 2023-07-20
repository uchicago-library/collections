open Prelude
module D = Defaults
module R = Mattlude.Endofunctors.Result.Make (String)

module Params = struct
  include Url.Querystring

  let endpoint_name = "getBrowseListLanguages"

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

  module Encoding = struct

    let vars_enc = assoc @@ list string

    let bindings_enc =
      assoc @@ list @@
        obj2
          (req "prefLabel" @@
             obj3
               (req "xml:lang" string)
               (req "type" string)
               (req "value" string))
          (req "code" @@
             obj2
               (req "type" string)
               (req "value" string))
      
    let head_enc = req "head" vars_enc
    let results_enc = req "results" bindings_enc
    let enc = obj2 head_enc results_enc
  end

  let ezjsonm json =
    match Ezjsonm.from_string json with
    | exception Ezjsonm.Parse_error (`Null,_)
                |  `O [("head",
                        `O [("vars",
                             `A [])]);
                       ("results",
                        `O [("bindings",
                             `A [])])] ->
       Error "empty result"
    | exception e ->
       Error (Printing.Error.to_string e)
    | success -> Ok success

  let parse = Result.trap
                Printing.Error.to_string
                (Json.destruct Encoding.enc)

  let schema = Utils.Schema.schema Encoding.enc
end

module Transform = struct
  let fix_languages alist =
    let open List.Assoc in 
    let reducer acc ((lang_lang, _, lang_name),
                     (_, code)) =
      add code (lang_lang, lang_name) acc in
    coalesce (foldl reducer [] alist)

  let transform (_, results) =
    let open R in
    let opt_to_res = function
      | Some bindings -> Ok bindings
      | None -> Error "key error"
    in
    let* bindings =
      opt_to_res (assoc_opt "bindings" results)
    in pure (fix_languages bindings)
end

module Export = struct
  open Data_encoding

  module Encoding = struct
    let enc = assoc (assoc string)
  end

  let un_result = function
    | Ok json -> `O [("ok", json)]
    | Error msg -> `O [("error", `String msg)]

  let export json_result =
    let open R in
    json_result
    >>| Json.construct Encoding.enc
    |> un_result

  module Example = struct
    
  end
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
    | DebugOff ->
       Fetch.fetch ~group ~collection ()
       |> Parse.ezjsonm
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
      (* cwd *)
      schemas_root
      "input"
      endpoint_name

  let output_path =
    sprintf
      "%s/%s/%s.json"
      (* cwd *)
      schemas_root
      "output"
      endpoint_name

  let input_write () =
    (* let cwd = Prelude.Unix.getcwd () in *)
    writefile ~fn:input_path input_schema

  let output_write () =
    (* let cwd = Prelude.Unix.getcwd () in *)
    writefile ~fn:output_path output_schema
end

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

include Api.Fetcher (Params) (D)

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
    | exception Ezjsonm.Parse_error (`Null,_) ->
       Error "empty result"
    | exception e ->
       Error (Printing.Error.to_string e)
    | success -> Ok success

  let parse = Result.trap
                Printing.Error.to_string
                (Json.destruct Encoding.enc)

  let schema = Printing.Encoding.to_string Encoding.enc
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
end

module Gimme = struct
  open Api.Debug

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
  let spec = Api.Spec.mk_spec qs_fields
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

let thing =
  `O
  [("head", `O [("vars", `A [`String "prefLabel"; `String "code"])]);
   ("results",
    `O
      [("bindings",
        `A
          [`O
             [("prefLabel",
               `O
                 [("xml:lang", `String "en"); ("type", `String "literal");
                  ("value", `String "Aguacateco")]);
              ("code",
               `O [("type", `String "literal"); ("value", `String "agu")])];
           `O
             [("prefLabel",
               `O
                 [("xml:lang", `String "en"); ("type", `String "literal");
                  ("value", `String "Akateko")]);
              ("code",
               `O [("type", `String "literal"); ("value", `String "knj")])];
           `O
             [("prefLabel",
               `O
                 [("xml:lang", `String "en"); ("type", `String "literal");
                  ("value", `String "Buglere")]);
              ("code",
               `O [("type", `String "literal"); ("value", `String "sab")])];
           `O
             [("prefLabel",
               `O
                 [("xml:lang", `String "en"); ("type", `String "literal");
                  ("value", `String "Central Guerrero Nahuatl")]);
              ("code",
               `O [("type", `String "literal"); ("value", `String "ngu")])];
           `O
             [("prefLabel",
               `O
                 [("xml:lang", `String "en"); ("type", `String "literal");
                  ("value", `String "Central Huasteca Nahuatl")]);
              ("code",
               `O [("type", `String "literal"); ("value", `String "nch")])];
           `O
             [("prefLabel",
               `O
                 [("xml:lang", `String "en"); ("type", `String "literal");
                  ("value", `String "Chol")]);
              ("code",
               `O [("type", `String "literal"); ("value", `String "ctu")])]])])]

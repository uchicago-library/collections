open Prelude
module D = Defaults
module R = Mattlude.Endofunctors.Result.Make (String)

module Params = struct
  include Url.Querystring

  let endpoint_name = "getItem"

  let ark_base = Url.BaseURL.Ark.ark_base

  let make
        ?(collection=D.collection)
        ?(identifier=D.identifier)
        ?search
        () =
    let _ = search in
    let qs = [ ("collection", [collection]) ;
               ("identifier", [ark_base identifier]) ; ]
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
      assoc (obj2
               (req "type" string)
               (req "value" string))

    let enc = bindings_to_enc bindings_enc
  end

  let parse = trap Encoding.enc
  let schema = Utils.Schema.schema Encoding.enc
end

module Supplements = struct
  let x = GetParent.endpoint_name
end

module Transform = struct

  let fix_bindings bindings =
    let f (key, (_, value)) =
      key, String.split ~sep:"|" value
    in
    map f (List.concat bindings)

  let add_restrictions restrz alist =
    alist @ ["accessRights", [restrz]]

  let catch_empty = function
    |  ("contributors", [])
       :: ("creators", [])
       :: ("dates", [])
       :: ("languages", [])
       :: ("titles", [])
       :: ("alternatives", [])
       :: ("locations", [])
       :: ("dmaTitles", [])
       :: _ ->
       Error "empty result"
    | other -> Ok other

  let restrictions = "Restricted"

  let transform (_, results) =
    let open R in
    (* todo: refactor opt_to_res *)
    let opt_to_res = function
      | Some bindings -> Ok bindings
      | None -> Error "key error"
    in
    let* bindings =
      opt_to_res (assoc_opt "bindings" results)
    in
    catch_empty
    @@ add_restrictions restrictions
    @@ fix_bindings bindings
end

module Export = struct
  open Data_encoding

  module Encoding = struct
    let enc = assoc (list string)
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
        ?(identifier=D.identifier)
        ?search
        () =
    let open R in 
    let _ = search in
    match debug with
    | Curl ->
       Url.url ~group ~collection ~identifier ()
    | Raw ->
       Fetch.fetch ~group ~collection ~identifier ()
       >>= Utils.Ezjsonm.ezjsonm
       |> Export.un_result
       |> Printing.Json.to_string
    | DebugOff ->
       Fetch.fetch ~group ~collection ~identifier ()
       >>= Utils.Ezjsonm.ezjsonm
       >>= Parse.parse
       >>= Transform.transform
       |> Export.export
       |> Printing.Json.to_string
end
include Gimme

module Spec = struct
  let qs_fields = [ "group"; "collection"; "identifier" ]
  let spec = Utils.Spec.mk_spec qs_fields
end

module Subservice = struct
  let subservice cgi _ _ =
    let open Restful in
    let open Param in
    let ps = process cgi Spec.spec in
    let group = (value ps "group") in
    let collection = (value ps "collection") in
    let identifier = (value ps "identifier") in
    Content.write ~content_type:"text/plain" cgi
                  (gimme ~group ~collection ~identifier ())
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

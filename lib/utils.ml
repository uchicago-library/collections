open Prelude

module type PARAMS = sig
  include Url.QUERYSTRING
  type value = string
  val endpoint_name : string
  val make : ?collection:string ->
             ?identifier:string ->
             ?search:string ->
             unit ->
             value list Dict.t
  val to_list : string list Dict.t ->
                (string * string list) list
end

module type DEFAULTS = sig
  val collection : string
  val group : string
  val identifier : string
  val search : string
end

module Fetcher (P : PARAMS) (D : DEFAULTS) = struct
  module Url = struct
    let make_api_string
          ?(group=D.group)
          ?(identifier=D.identifier)
          ?(search=D.search)
          api_name
          params =
      let _ = identifier in
      let _ = search in
      let qs_encode dct =
        dct 
        |> P.to_list
        |> Uri.encoded_of_query
      in
      let url_prefix =
        let open Url.BaseURL.MarkLogic in
        assemble_url_prefix ~group:group api_name
      in
      url_prefix ^ "&" ^ qs_encode params

    let url
          ?(group=D.group)
          ?(collection=D.collection)
          ?(identifier=D.identifier)
          ?(search=D.search)
          () =
      make_api_string
        ~group
        ~identifier
        ~search
        P.endpoint_name
        (P.make ~collection ())
  end

  module Fetch = struct

    let fetch_ocamlnet uri =
      match Uri.to_string uri |> Nethttp_client.Convenience.http_get_message with
      (* | exception Nethttp_client.Http_error (code, str) -> Error (`Http (code, (), str)) *)
      | call -> call # response_body # value

    let fetch
          ?(group=D.group)
          ?(collection=D.collection)
          ?(identifier=D.identifier)
          ?(search=D.search)
          () 
      = let url =
          Url.url ~group ~collection ~identifier ~search ()
        in fetch_ocamlnet (Uri.of_string url)
  end
end

module Spec = struct
  let mk_spec fields =
    let open Restful.Valid in
    let open Restful.Param in
    let each_field name = name, Mandatory, notblank
    in map each_field fields
end

module Debug = struct
  type t =
    | Curl
    | Raw
    | DebugOff
end

module Schema = struct
  let schemas_root = "schemas"
  let schema enc = Printing.Encoding.to_string enc
end

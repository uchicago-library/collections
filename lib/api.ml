module type PARAMS = sig
  include Url.QUERYSTRING
  val endpoint_name : string
  val make : ?collection:value ->
             ?identifier:value ->
             ?search:value ->
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
    let fetch_body url =
      let open Lwt.Infix in
      let stringify (_, body) =
        Cohttp_lwt.Body.to_string body
      in
      Cohttp_lwt_unix.Client.get (Uri.of_string url) >>= stringify
      |> Lwt_main.run

    let fetch
          ?(group=D.group)
          ?(collection=D.collection)
          ?(identifier=D.identifier)
          ?(search=D.search)
          () 
      = let url =
          Url.url ~group ~collection ~identifier ~search ()
        in fetch_body url
  end
end

module Debug = struct
  type t =
    | Curl
    | Raw
    | DebugOff
end

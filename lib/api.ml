let fetch url =
  let open Lwt.Infix in
  let stringify (_, body) = Cohttp_lwt.Body.to_string body in
  Cohttp_lwt_unix.Client.get (Uri.of_string url) >>= stringify
  |> Lwt_main.run

type gimme =
  ?group:string ->
  ?collection:string ->
  unit ->
  string

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
        ?(group=Defaults.group)
        api_name
        params =
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
          () =
      make_api_string
        ~group
        P.endpoint_name
        (P.make ~collection ())
  end

  module Fetch = struct
    let fetch
          ?(group=D.group)
          ?(collection=D.collection)
          () 
      = let url =
          Url.url ~group:group ~collection:collection ()
        in fetch url
  end
end

let mk_subservice (gimme : gimme) spec cgi _ _ =
  let open Restful in
  let open Param in
  let ps = process cgi spec in
  let group = (value ps "group") in
  let collection = (value ps "collection") in
  Content.write ~content_type:"text/plain" cgi
    (Printf.sprintf "%s" (gimme ~group ~collection ()))

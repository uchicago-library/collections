open Prelude

module type QUERYSTRING = sig
  module Field : Prelude.OrderedType
  module Dict : module type of Map.Make (Field)
  type value = string
  type t = value list Dict.t
  val to_list : 'a Dict.t -> (Field.t * 'a) list
  val of_list : (Field.t * 'a) list -> 'a Dict.t
end

module Querystring : QUERYSTRING
       with module Field = String
        and type value = string = struct
  module Field = String
  module Dict = Map.Make (Field)
  type value = string
  type t = value list Dict.t
  let to_list = Dict.to_list
  let of_list dct = Dict.of_list Dict.empty dct
end

module BaseURL = struct
  module MarkLogic = struct
    let ml_host = "http://marklogic.lib.uchicago.edu"
    let ml_port = 8031
    let ml_path = "main.xqy?query="

    let assemble_url_prefix_full host group api_name port path =
      let parts = [
          host;
          ":";
          string_of_int port;
          "/";
          group;
          "/";
          path;
          api_name;
        ]
      in
      String.join ~sep:"" parts

    let assemble_url_prefix ?(group=Defaults.group) api_name =
      assemble_url_prefix_full
        ml_host
        group
        api_name
        ml_port
        ml_path
  end

  module Ark = struct
    let ark_host = "https://ark.lib.uchicago.edu"
    let ark_path = "ark:61001"

    let assemble_url_prefix host path identifier =
      let parts = [
          host;
          "/";
          path;
          "/";
          identifier;
        ]
      in String.join ~sep:"" parts

    let ark_base = assemble_url_prefix ark_host ark_path
  end
end

let make_api_string ?(curl=true) ?(group=Defaults.group) api_name params =
  let open Querystring in
  let qs_encode dct =
    dct 
    |> to_list
    |> Uri.encoded_of_query
  in
  let url_prefix =
    let open BaseURL.MarkLogic in
    assemble_url_prefix ~group:group api_name
  in
  if curl
  then url_prefix ^ "&" ^ qs_encode params
  else url_prefix

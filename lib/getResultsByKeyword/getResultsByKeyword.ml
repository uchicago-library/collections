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

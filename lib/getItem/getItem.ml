open Prelude
module R = Mattlude.Endofunctors.Result.Make (String)

module D = struct
  include Defaults
  include Item
end

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
    let qs = [ ("collection", [collection]);
               ("identifier", [ark_base identifier]); ]
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
      array @@ assoc @@ obj2
                          (req "type" string)
                          (req "value" string)

    let enc = bindings_to_enc bindings_enc
  end

  let parse = trap Encoding.enc
  let schema = Utils.Schema.schema Encoding.enc
end

module Transform = struct
  let transform alist =
    let f (key, (_, value)) =
      key, String.split ~sep:"|" value
    in
    map f alist
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

  let schema = Utils.Schema.schema Encoding.enc
end

(* module Gimme = struct
 *   open Utils.Debug
 * 
 *   let gimme
 *         ?(debug=DebugOff)
 *         ?(group=D.group)
 *         ?(collection=D.collection)
 *         ?identifier
 *         ?search
 *         () =
 *     let open R in 
 *     let _ = identifier in
 *     let _ = search in
 *     match debug with
 *     | Curl ->
 *        Url.url ~group ~collection ()
 *     | Raw ->
 *        Fetch.fetch ~group ~collection ()
 *     | DebugOff ->
 *        Fetch.fetch ~group ~collection ()
 *        |> Utils.Ezjsonm.ezjsonm
 *        >>= Parse.parsed
 *        >>= Transform.transform
 *        |> Export.export
 *        |> Printing.Json.to_string
 * end
 * include Gimme *)

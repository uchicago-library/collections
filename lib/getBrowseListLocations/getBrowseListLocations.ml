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
  let fix_languages alist =
    let open List.Assoc in 
    let reducer acc ((lang_lang, _, lang_name),
                     (_, code)) =
      match lang_lang with
      | Some l -> add code (l, lang_name) acc 
      | None -> add code ("en", lang_name) acc
    in
    coalesce (foldl reducer [] alist)

  let transform (_, results) =
    let open R in
    (* todo: refactor opt_to_res *)
    let opt_to_res = function
      | Some bindings -> Ok bindings
      | None -> Error "key error"
    in
    let* bindings =
      opt_to_res (assoc_opt "bindings" results)
    in pure (fix_languages bindings)
end

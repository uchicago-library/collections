open Prelude
module D = Defaults
module R = Mattlude.Endofunctors.Result.Make (String)

module Params = struct
  include Url.Querystring

  let endpoint_name = "getParent"

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


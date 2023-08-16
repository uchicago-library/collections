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

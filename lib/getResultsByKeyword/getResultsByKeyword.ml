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
    let bindings_enc () =
      list @@ obj1 @@
        (req "resource" 
           (obj2 (req "type" string)
           (req "value" string)))
  end
                  
  let enc = bindings_to_enc bindings_enc
end

module Transform = struct

end

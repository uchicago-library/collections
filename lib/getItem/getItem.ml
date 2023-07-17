(* open Prelude *)

module D = struct
  include Defaults
  include Item
end

module Params = struct
  include Url.Querystring

  let make ?(identifier=D.identifier)
        ?(collection=D.collection) () =
    let qs = [ ("collection", [collection]);
               ("identifier", [identifier]); ]
    in Dict.of_list Dict.empty qs

  let to_list = Dict.to_list 
end


(* open Prelude *)

module D = struct
  include Defaults
  include Item
end

module Params = struct
  open Url.Querystring

  let make ?(identifier=D.identifier)
        ?(collection=D.collection) () =
    let qs = [ ("collection", [collection]);
               ("identifier", [identifier]); ]
    in of_list qs
end


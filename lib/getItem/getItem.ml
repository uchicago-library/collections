(* open Prelude *)

module D = struct
  include Defaults
  include Item
end

module Params = struct
  open Url

  let make ?(identifier=D.identifier)
        ?(collection=D.collection) () =
    let qs = [ ("collection", [collection]);
               ("identifier", [identifier]); ]
    in Querystring.of_list Querystring.empty qs
end


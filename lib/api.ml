open Prelude

module type QUERYSTRING = sig
  module Field : Prelude.OrderedType
  module Dict : module type of Map.Make (Field)
  type value = string
  type t = value list Dict.t
end

module Querystring : QUERYSTRING
       with module Field = String
        and type value = string = struct
  module Field = String
  module Dict = Map.Make (Field)
  type value = string
  type t = value list Dict.t
end



(* let make_api_string ?(curl=true) collection group api_name params =
 *   assert false *)


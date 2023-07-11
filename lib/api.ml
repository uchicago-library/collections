open Prelude

module Querystring_intf = struct
  module Field = String
  type value = string
  module Dict = Map.Make (Field)
  type t = value list Dict.t
end


let make_api_string ?(curl=true) collection group api_name params =
  assert false


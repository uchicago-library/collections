module type QUERYSTRING = sig
  module Field : Prelude.OrderedType
  module Dict : module type of Prelude.Map.Make (Field)
  type value
  type t
end

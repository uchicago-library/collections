module type QUERYSTRING = Url_intf.QUERYSTRING

module Querystring : QUERYSTRING
       with module Field = Prelude.String
        and type value = string

module BaseURL : sig
  module MarkLogic : sig
    val assemble_url_prefix : ?group:string -> string -> string
  end
  module Ark : sig
    val ark_base : string -> string  end
end

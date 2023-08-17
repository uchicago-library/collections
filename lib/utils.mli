module type PARAMS = sig
  include Url.QUERYSTRING
  type value = string
  val endpoint_name : string
  val make :
    ?collection:string ->
    ?identifier:string -> ?search:string -> unit -> string list Dict.t
  val to_list : string list Dict.t -> (string * string list) list
  end

module type DEFAULTS = sig
  val collection : string
  val group : string
  val identifier : string
  val search : string
end

module Fetcher :
functor (_ : PARAMS) (_ : DEFAULTS) -> sig
  module Url : sig
    val url :
      ?group:string ->
      ?collection:string ->
      ?identifier:string ->
      ?search:string ->
      unit ->
      string
  end
  module Fetch : sig
    val fetch :
      ?group:string ->
      ?collection:string ->
      ?identifier:string ->
      ?search:string ->
      unit ->
      (string, string) result
  end
end

module Spec : sig
  val mk_spec :
    'a list -> ('a * Restful.Param.mandopt * (string -> bool)) list
end

module Debug : sig
  type t = Curl | Raw | DebugOff
end

module Schema : sig
  val schemas_root : string
  val schema : 'a Data_encoding.Encoding.t -> string
end

module Encoding : sig
  val bindings_to_enc :
    'a Data_encoding.Encoding.t ->
    ((string * string list) list
     * (string * 'a list) list) Data_encoding.Encoding.t

  val trap : 'a Data_encoding.Encoding.t ->
             Data_encoding.Json.json ->
             ('a, string) result
end

module Ezjsonm : sig
  val ezjsonm : string -> ([> Ezjsonm.t ], string) result
end

module Transform : sig
  val assoc_res : 'a ->
                  'b ->
                  ('b * 'c) list ->
                  ('c, 'a) result
end

module type PARAMS = sig
  include Url.QUERYSTRING
  val endpoint_name : string
  val make :
    ?collection:value ->
    ?identifier:value -> ?search:value -> unit -> value list Dict.t
  val to_list : string list Dict.t -> (string * string list) list
  end

module type DEFAULTS =
  sig
    val collection : string
    val group : string
    val identifier : string
    val search : string
  end

module Fetcher :
  functor (P : PARAMS) (_ : DEFAULTS) ->
    sig
      module Url :
        sig
          val make_api_string :
            ?group:string ->
            ?identifier:string ->
            ?search:string -> string -> string list P.Dict.t -> string
          val url :
            ?group:string ->
            ?collection:string ->
            ?identifier:string -> ?search:string -> unit -> string
        end
      module Fetch :
        sig
          val fetch_body : string -> string
          val fetch_ocamlnet : Uri.t -> string
          val fetch :
            ?group:string ->
            ?collection:string ->
            ?identifier:string -> ?search:string -> unit -> string
        end
    end

module Spec :
  sig
    val mk_spec :
      'a list -> ('a * Restful.Param.mandopt * (string -> bool)) list
  end

module Debug : sig type t = Curl | Raw | DebugOff end

module Schema :
  sig
    val schemas_root : string
    val schema : 'a Data_encoding.Encoding.t -> string
  end
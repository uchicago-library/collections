module type PARAMS = sig
  val endpoint_name : string
  val make :
    ?collection:string ->
    ?identifier:string -> ?search:string -> unit -> string list Url.Querystring.t
  val to_list : string list Url.Querystring.t -> (string * string list) list
end

module type DEFAULTS = sig
  val collection : string
  val group : string
  val identifier : string
  val search : string
end

module type DEBUG = sig
  type t = Curl | Raw | DebugOff
end

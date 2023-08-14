module type INTERFACE = sig
  val endpoint_name : string

  val gimme :
    ?debug:Utils.Debug.t ->
    ?group:string ->
    ?collection:string -> ?identifier:'a -> ?search:'b -> unit -> string

  val subservice : Netcgi.cgi -> 'a -> 'b -> Nethttp.http_status

  module Schema :
  sig
    val input_schema : string
    val output_schema : string
    val input_path : string
    val output_path : string
    val input_write : unit -> unit
    val output_write : unit -> unit
  end
end


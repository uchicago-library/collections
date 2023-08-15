module Json : sig
  val to_string : Data_encoding.Json.json -> string
  val print : ?truncate:int -> Data_encoding.Json.json -> unit
end

module Json_string : sig
  val print : ?truncate:int -> string -> unit
end

module Encoding : sig
  val to_string : 'a Data_encoding.Encoding.t -> string
  val print : ?truncate:int -> 'a Data_encoding.Encoding.t -> unit
end

module Error : sig
  val to_string : exn -> string
end

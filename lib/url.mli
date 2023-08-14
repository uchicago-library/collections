module BaseURL :
  sig
    module MarkLogic :
      sig
        val ml_host : string
        val ml_port : int
        val ml_path : string
        val assemble_url_prefix_full :
          string -> string -> string -> int -> string -> string
        val assemble_url_prefix : ?group:string -> string -> string
      end
    module Ark :
      sig
        val ark_host : string
        val ark_path : string
        val assemble_url_prefix : string -> string -> string -> string
        val ark_base : string -> string
      end
  end

open Prelude

module BaseURL = struct
  module MarkLogic = struct
    let ml_host = "http://marklogic.lib.uchicago.edu"
    let ml_port = 8031
    let ml_path = "main.xqy?query="
    let ml_group = "dma"

    let assemble_url_prefix_full host group api_name port path =
      let parts = [
          host;
          ":";
          string_of_int port;
          "/";
          group;
          ";";
          path;
          api_name;
        ]
      in
      String.join ~sep:"" parts

    let assemble_url_prefix api_name =
      assemble_url_prefix_full
        ml_host
        ml_group
        api_name
        ml_port
        ml_path
  end

  module Ark = struct
    let ark_host = "https://ark.lib.uchicago.edu"
    let ark_path = "ark:61001"

    let assemble_url_prefix host path identifier =
      let parts = [
          host;
          "/";
          path;
          "/";
          identifier;
        ]
      in String.join ~sep:"" parts

    let ark_base = assemble_url_prefix ark_host ark_path
  end
end

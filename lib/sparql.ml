open Prelude

let collection_uri = "uri for MLC goes here, stick it in the FROM clause"

(* does Charles put the MLC triples in a named graph *)

module Queries = struct
  let item sparql_root identifier =
    Prelude.sprintf
      "SELECT ?coverage ?creator ?date ?description ?format ?identifier ?publisher ?rights ?subject ?title ?type ?spatial ?ClassificationLcc ?Local
       FROM <%s>
       WHERE {{
       <ark:61001/%s> <http://purl.org/dc/elements/1.1/identifier> ?identifier .
       OPTIONAL {{ <ark:61001/%s> <http://purl.org/dc/elements/1.1/creator> ?creator . }}
       OPTIONAL {{ <ark:61001/%s> <http://purl.org/dc/elements/1.1/date> ?date . }}
       OPTIONAL {{ <ark:61001/%s> <http://purl.org/dc/elements/1.1/description> ?description . }}
       OPTIONAL {{ <ark:61001/%s> <http://purl.org/dc/elements/1.1/format> ?format . }}
       OPTIONAL {{ <ark:61001/%s> <http://purl.org/dc/elements/1.1/publisher> ?publisher . }}
       OPTIONAL {{ <ark:61001/%s> <http://purl.org/dc/elements/1.1/rights> ?rights . }}
       OPTIONAL {{ <ark:61001/%s> <http://purl.org/dc/elements/1.1/subject> ?subject .  }}
       OPTIONAL {{ <ark:61001/%s> <http://purl.org/dc/elements/1.1/title> ?title . }}
       OPTIONAL {{ <ark:61001/%s> <http://purl.org/dc/elements/1.1/type> ?type . }}
       OPTIONAL {{ <ark:61001/%s> <http://purl.org/dc/terms/spatial> ?spatial . }}
       OPTIONAL {{ <ark:61001/%s> <http://id.loc.gov/ontologies/bibframe/ClassificationLcc> ?ClassificationLcc . }}
       OPTIONAL {{ <ark:61001/%s> <http://id.loc.gov/ontologies/bibframe/Local> ?Local . }}
       }}"
      sparql_root
      identifier
      identifier
      identifier
      identifier
      identifier
      identifier
      identifier
      identifier
      identifier
      identifier
      identifier
      identifier
      identifier
end

module Url = struct
  let ldr_password () =
    String.trim "\n" (readfile "./password.txt")

  let ldr_user () =
    String.trim "\n" (readfile "./user.txt")

  let port = 8008

  let ldr_base = "http://marklogic.lib.uchicago.edu"

  let ldr_route = "/v1/graphs"

  let ldr_url = Prelude.sprintf "%s:%d%s/sparql"
                  ldr_base
                  port
                  ldr_route

  let sparql_root =
    "https://repository.lib.uchicago.edu/digital_collections"

  let params query =
    "?query=" ^ query

  let mk_64 username password =
    Prelude.sprintf "%s:%s" username password
    |> Base64.encode_exn

  let mk_auth_header username password =
    "Authorization: Basic " ^ mk_64 username password

  let turtle_header = "Content-type: text-turtle"

  let headers () =
    let username = ldr_user () in
    let password = ldr_password () in
    [turtle_header;
     mk_auth_header username password]
end

let example_object = "b2nd42r7xp01"

let query () =
  Queries.item Url.sparql_root example_object

let hello_world = "SELECT *
                   WHERE {
                   ?s ?p ?o .
                   }
                   LIMIT 10"

module Fetch = struct
  let fetch = Utils.fetch

  let make_query query =
    let open Url in
    fetch
      ~headers:(headers ())
      (Uri.of_string @@ ldr_url ^ params (Uri.pct_encode query))
end

module type ENDPOINT = Endpoint_intf.ENDPOINT

let endpoint_to_spec (module E : ENDPOINT) =
  E.(endpoint_name, subservice)

let print_schemas (module E : ENDPOINT) =
  Prelude.print ("Printing input schema for " ^ E.endpoint_name) ;
  E.Schema.input_write () ;
  Prelude.print ("Printing output schema for " ^ E.endpoint_name) ;
  E.Schema.output_write ()

let endpoints = [ (module GetBrowseListLanguages : ENDPOINT) ;
                  (module GetItem : ENDPOINT) ;
                  (module GetResultsByKeyword : ENDPOINT) ;
                  (module GetBrowseListLocations : ENDPOINT) ;
                ]

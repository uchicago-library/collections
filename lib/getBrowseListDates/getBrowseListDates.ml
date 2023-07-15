module Params = struct
  include Url.Querystring

  let make_verbose collection =
    let qs = [ ("collection", collection) ]
    in Dict.of_list Dict.empty qs
  
  let make ?(collection=Defaults.collection) () =
    make_verbose collection
end

module Encoding = struct
  let vars_enc =
    let open Data_encoding in
    assoc (list string)

  let bindings_enc =
    let open Data_encoding in
    assoc (array (assoc (assoc string)))
end

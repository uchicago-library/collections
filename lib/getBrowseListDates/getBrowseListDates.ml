module Params = struct
  include Api.Querystring_intf

  let make_verbose collection =
    let qs = [ ("collection", collection) ]
    in Dict.of_list Dict.empty qs

  let make ?(collection=Defaults.collection) () =
    make_verbose collection
end


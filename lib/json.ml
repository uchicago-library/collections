open Prelude

module type PRINTING = sig
  val to_string : Json_repr.ezjsonm -> string
  val print : ?truncate:int -> Json_repr.ezjsonm -> unit
end

module Printing : PRINTING = struct
  let to_string json =
    let () = Json_repr.pp
               (module Json_repr.Ezjsonm)
               Format.str_formatter
               json
    in Format.flush_str_formatter ()

  let print_entire json =
    Format.printf
      "%a"
      (Json_repr.pp (module Json_repr.Ezjsonm))
      json

  let print ?truncate json =
    match truncate with
    | Some n -> Prelude.print
                @@ String.take n
                @@ to_string json
    | None -> print_entire json
end
include Printing

module Example = struct
  let json_string = {|{"name":"Watermelon","id":25,"family":"Cucurbitaceae","order":"Cucurbitales","genus":"Citrullus","nutritions":{"calories":30,"fat":0.2,"sugar":6.0,"carbohydrates":8.0,"protein":0.6}}|}

  let json = Ezjsonm.from_string json_string

  let marklogic_string = {|{"head":{"vars":["identifier","content","type","soundFile","contributors","creators","dates","description","languages","titles","accessRights","alternatives","locations","contentType","olacLinguisticDataType","dmaTitles","discourseType"]},"results":{"bindings":[{"identifier":{"type":"literal","value":"https://n2t.net/ark:61001/b2k40qk4wc8h"},"soundFile":{"type":"uri","value":"https://ark.lib.uchicago.edu/ark:61001/b2d75mb5kv6w"},"contributors":{"type":"literal","value":""},"creators":{"type":"literal","value":""},"dates":{"type":"literal","value":"2008-04-29|2008"},"languages":{"type":"literal","value":""},"titles":{"type":"literal","value":""},"alternatives":{"type":"literal","value":""},"locations":{"type":"literal","value":"(:unav)"},"dmaTitles":{"type":"literal","value":""}}]}}|}

  let marklogic = Ezjsonm.from_string marklogic_string

  let getBrowseListLanguages_string = {|{"head":{"vars":["prefLabel","code"]},"results":{"bindings":[{"prefLabel":{"xml:lang":"en","type":"literal","value":"Aguacateco"},"code":{"type":"literal","value":"agu"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Akateko"},"code":{"type":"literal","value":"knj"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Buglere"},"code":{"type":"literal","value":"sab"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Central Guerrero Nahuatl"},"code":{"type":"literal","value":"ngu"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Central Huasteca Nahuatl"},"code":{"type":"literal","value":"nch"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Chol"},"code":{"type":"literal","value":"ctu"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Chuj"},"code":{"type":"literal","value":"cac"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Classical Nahuatl"},"code":{"type":"literal","value":"nci"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"English"},"code":{"type":"literal","value":"eng"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Garifuna"},"code":{"type":"literal","value":"cab"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"German"},"code":{"type":"literal","value":"deu"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Highland Totonac"},"code":{"type":"literal","value":"tos"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Huastec"},"code":{"type":"literal","value":"hus"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Huautla Mazatec"},"code":{"type":"literal","value":"mau"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Huichol"},"code":{"type":"literal","value":"hch"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Ixcatec"},"code":{"type":"literal","value":"ixc"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Ixil"},"code":{"type":"literal","value":"ixl"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"K'iche'"},"code":{"type":"literal","value":"quc"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Kekchí"},"code":{"type":"literal","value":"kek"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Mam"},"code":{"type":"literal","value":"mam"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Mayo"},"code":{"type":"literal","value":"mfy"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Mezquital Otomi"},"code":{"type":"literal","value":"ote"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Mocho"},"code":{"type":"literal","value":"mhc"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Mopán Maya"},"code":{"type":"literal","value":"mop"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Ngäbere"},"code":{"type":"literal","value":"gym"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Pipil"},"code":{"type":"literal","value":"ppl"}},{"prefLabel":{"xml:lang":"es","type":"literal","value":"Pipil"},"code":{"type":"literal","value":"ppl"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Popti'"},"code":{"type":"literal","value":"jac"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Poqomam"},"code":{"type":"literal","value":"poc"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Poqomam, Eastern"},"code":{"type":"literal","value":"poa"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Poqomchi'"},"code":{"type":"literal","value":"poh"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Poqomchi', Western"},"code":{"type":"literal","value":"pob"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Purepecha"},"code":{"type":"literal","value":"tsz"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Q'anjob'al"},"code":{"type":"literal","value":"kjb"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Spanish"},"code":{"type":"literal","value":"spa"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Tlaxcala-Puebla-Central Nahuatl"},"code":{"type":"literal","value":"nhn"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Tojolabal"},"code":{"type":"literal","value":"toj"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Tzeltal"},"code":{"type":"literal","value":"tzh"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Tzotzil"},"code":{"type":"literal","value":"tzo"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Tzotzil, Zinacantán"},"code":{"type":"literal","value":"tzz"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Yucatec Maya"},"code":{"type":"literal","value":"yua"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Zapotec"},"code":{"type":"literal","value":"zap"}}]}}|}

  let getBrowseListLanguages = Ezjsonm.from_string getBrowseListLanguages_string
end

         

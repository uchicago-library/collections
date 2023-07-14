open Prelude

module Example = struct
  let string = {|{"head":{"vars":["prefLabel","code"]},"results":{"bindings":[{"prefLabel":{"xml:lang":"en","type":"literal","value":"Aguacateco"},"code":{"type":"literal","value":"agu"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Akateko"},"code":{"type":"literal","value":"knj"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Buglere"},"code":{"type":"literal","value":"sab"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Central Guerrero Nahuatl"},"code":{"type":"literal","value":"ngu"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Central Huasteca Nahuatl"},"code":{"type":"literal","value":"nch"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Chol"},"code":{"type":"literal","value":"ctu"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Chuj"},"code":{"type":"literal","value":"cac"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Classical Nahuatl"},"code":{"type":"literal","value":"nci"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"English"},"code":{"type":"literal","value":"eng"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Garifuna"},"code":{"type":"literal","value":"cab"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"German"},"code":{"type":"literal","value":"deu"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Highland Totonac"},"code":{"type":"literal","value":"tos"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Huastec"},"code":{"type":"literal","value":"hus"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Huautla Mazatec"},"code":{"type":"literal","value":"mau"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Huichol"},"code":{"type":"literal","value":"hch"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Ixcatec"},"code":{"type":"literal","value":"ixc"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Ixil"},"code":{"type":"literal","value":"ixl"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"K'iche'"},"code":{"type":"literal","value":"quc"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Kekchí"},"code":{"type":"literal","value":"kek"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Mam"},"code":{"type":"literal","value":"mam"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Mayo"},"code":{"type":"literal","value":"mfy"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Mezquital Otomi"},"code":{"type":"literal","value":"ote"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Mocho"},"code":{"type":"literal","value":"mhc"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Mopán Maya"},"code":{"type":"literal","value":"mop"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Ngäbere"},"code":{"type":"literal","value":"gym"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Pipil"},"code":{"type":"literal","value":"ppl"}},{"prefLabel":{"xml:lang":"es","type":"literal","value":"Pipil"},"code":{"type":"literal","value":"ppl"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Popti'"},"code":{"type":"literal","value":"jac"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Poqomam"},"code":{"type":"literal","value":"poc"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Poqomam, Eastern"},"code":{"type":"literal","value":"poa"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Poqomchi'"},"code":{"type":"literal","value":"poh"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Poqomchi', Western"},"code":{"type":"literal","value":"pob"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Purepecha"},"code":{"type":"literal","value":"tsz"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Q'anjob'al"},"code":{"type":"literal","value":"kjb"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Spanish"},"code":{"type":"literal","value":"spa"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Tlaxcala-Puebla-Central Nahuatl"},"code":{"type":"literal","value":"nhn"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Tojolabal"},"code":{"type":"literal","value":"toj"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Tzeltal"},"code":{"type":"literal","value":"tzh"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Tzotzil"},"code":{"type":"literal","value":"tzo"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Tzotzil, Zinacantán"},"code":{"type":"literal","value":"tzz"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Yucatec Maya"},"code":{"type":"literal","value":"yua"}},{"prefLabel":{"xml:lang":"en","type":"literal","value":"Zapotec"},"code":{"type":"literal","value":"zap"}}]}}|}
  let value = Ezjsonm.from_string string
end

module Parse = struct
  module Encoding = struct
    open Data_encoding

    let vars_enc = assoc @@ list string
    let bindings_enc =
      assoc @@ list @@ obj2
                         (req "prefLabel" @@ assoc string)
                         (req "code" @@ assoc string)

    let head_enc = req "head" vars_enc
    let results_enc = req "results" bindings_enc

    let enc = obj2 head_enc results_enc
    let schema = Printing.Encoding.to_string enc

    let example = Json.destruct enc Example.value
  end
end

module Transform = struct
  
end


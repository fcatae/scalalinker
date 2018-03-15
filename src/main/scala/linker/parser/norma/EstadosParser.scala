package linker.parser.norma

import linker.utils.Trie
import linker.parser.BasicLinkerParser
import linker.parser.TrieParser

trait EstadosParser {
  
  self : BasicLinkerParser with TrieParser =>
  
  val lookupSigla = Map(
      ("ac",List("acre")) 
    , ("al",List("alagoas"))
    , ("am",List("amazonas"))
    , ("ap",List("amapa"))
    , ("ba",List("bahia"))
    , ("ce",List("ceara"))
    , ("df",List("distrito","federal"))
    , ("es",List("espirito","santo")) 
    , ("go",List("goias"))
    , ("ma",List("maranhao"))
    , ("mg",List("minas","gerais"))
    , ("ms",List("mato","grosso","sul"))
    , ("mt",List("mato","grosso"))
    , ("pa",List("para"))
    , ("pb",List("paraiba"))
    , ("pe",List("pernambuco"))
    , ("pi",List("piaui"))
    , ("pr",List("parana"))
    , ("rj",List("rio","janeiro"))
    , ("rn",List("rio","grande","norte"))
    , ("ro",List("rondonia"))
    , ("rr",List("roraima"))
    , ("rs",List("rio","grande","sul"))
    , ("sc",List("santa","catarina"))
    , ("se",List("sergipe"))
    , ("sp",List("sao","paulo"))
    , ("to",List("tocantins"))    
  )
  
  val listaEstados = List(
      (List("acre"),List("acre")) 
    , (List("alagoas"),List("alagoas"))
    , (List("amazonas"),List("amazonas"))
    , (List("amapá"),List("amapa"))
    , (List("bahia"),List("bahia"))
    , (List("ceará"),List("ceara"))
    , (List("distrito","federal"),List("distrito","federal"))
    , (List("espírito","santo"),List("espirito","santo")) 
    , (List("goiás"),List("goias"))
    , (List("maranhão"),List("maranhao"))
    , (List("minas","gerais"),List("minas","gerais"))
    , (List("mato","grosso","do","sul"),List("mato","grosso","sul"))
    , (List("mato","grosso"),List("mato","grosso"))
    , (List("pará"),List("para"))
    , (List("paraíba"),List("paraiba"))
    , (List("pernambuco"),List("pernambuco"))
    , (List("piauí"),List("piaui"))
    , (List("paraná"),List("parana"))
    , (List("rio","de","janeiro"),List("rio","janeiro"))
    , (List("rio","grande","do","norte"),List("rio","grande","norte"))
    , (List("rondônia"),List("rondonia"))
    , (List("roraima"),List("roraima"))
    , (List("rio","grande","do","sul"),List("rio","grande","sul"))
    , (List("santa","catarina"),List("santa","catarina"))
    , (List("sergipe"),List("sergipe"))
    , (List("são","paulo"),List("sao","paulo"))
    , (List("tocantins"),List("tocantins"))    
  )
  
  val trieEstados = Trie.monta(listaEstados)
  
  lazy val parseEstados = parseTrie(trieEstados)

}
package linker.parser.norma

import linker.utils.Utils._
import linker.utils.Trie
import linker.parser.BasicLinkerParser
import linker.parser.TrieParser
import scala.language.postfixOps

trait MunicipiosParser {

  self : BasicLinkerParser with EstadosParser with TrieParser =>

  def lista1: List[(List[String], String)] = {
    val is = getClass().getResourceAsStream("municipios.txt")
    
    if(is == null) {
      print("Classe = ")
      println(getClass().getName())
      println("não foi possível encontrar arquivo de municipios")
    }

    (for (line <- io.Source.fromInputStream(is, "UTF-8").getLines) yield {      
      val l0 = line.split(";")
      val l1 = l0(0).split(",").toList
      l1 -> l0(1).trim
    }).toList
  }

  lazy val trieMunicipios = Trie.monta(lista2)

  lazy val lista2 = for ((municipio, estado) <- lista1) yield     
    municipio -> (limpaNome(municipio) -> lookupSigla(estado))
  

  val palavrasIgnoradas = Set("de", "do", "da", "dos", "das", "no", "na", "-", "e")

  def limpaNome(nome : List[String]) = nome.filterNot(palavrasIgnoradas contains).map(_.map(limpa))

  lazy val parseMunicipios = parseTrie(trieMunicipios)

}

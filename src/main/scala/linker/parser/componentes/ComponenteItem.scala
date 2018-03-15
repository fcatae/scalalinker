package linker.parser.componentes

import linker.parser.BasicLinkerParser
import linker.parser.Combinadores
import linker.parser.Terminais
import linker.parser.numeros.Numeros
import scala.language.postfixOps

trait ComponenteItem {
  
  self: BasicLinkerParser with
    Terminais with
  	Combinadores with
  	Numeros =>
  	  
  import linker.remissao.Remissoes.Item
  import linker.remissao.Remissoes.NumeroRemissao
  
  lazy val componenteDiretoItem = flatListaDe(componenteDiretoItemPlural | componenteDiretoItemSingular) ^? {
    case l if l nonEmpty => l
  }

  lazy val componenteDiretoItemSemNome = flatListaDe(componenteDiretoItemPlural | componenteDiretoItemSingularSemNome)

  lazy val componenteDiretoItemPlural: Parser[List[Item]] =
    (opt(os | nos) ~>
      opt(termos ~ (dos | os)) ~>
      itens ~>
      componenteDiretoItemSemNome)

  lazy val componenteDiretoItemSingular =
    opt(o | no | do_ | separador ~ opt(do_) | (opt(separador) ~ nos ~ termos ~ (do_ | o))) ~> item ~>
      numeroComComplemento(numeroArabico) ^^ {
        case num => List(Item(NumeroRemissao((num.map(_.num)): _*)))
      }

  lazy val componenteDiretoItemSingularSemNome =
    opt(o | no | do_ | separador ~ opt(do_) | (opt(separador) ~ nos ~ termos ~ (do_ | o))) ~>
      numeroComComplemento(numeroArabico) ^^ {
        case num => List(Item(NumeroRemissao((num.map(_.num)): _*)))
      }
      
  lazy val componenteItem = flatListaDe(componenteItemPlural | componenteItemSingular)

  lazy val componenteItemSingular =
    (opt(opt(todos | todas | ambos | ambas | nos ~ termos) ~ (da | separador ~ opt(da))) ~>
      item ~> numeroComComplemento(numeroAlfabeto)) ^^ {
        a => List(Item(NumeroRemissao(a.map(_.num): _*)))
      }

  lazy val componenteItemPlural = opt(os | nos) ~> itens ~>
    listaNumeros(numeroComComplemento(numeroAlfabeto)) ^^ {
      l =>
        l map { n =>
          Item(NumeroRemissao(n.map(_.num): _*))
        }
    }    

}
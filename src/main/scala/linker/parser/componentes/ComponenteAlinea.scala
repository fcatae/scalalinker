package linker.parser.componentes

import linker.parser.BasicLinkerParser
import linker.parser.Combinadores
import linker.parser.Terminais
import linker.parser.numeros.Numeros

import scala.language.postfixOps

trait ComponenteAlinea {
  
  self: BasicLinkerParser with
    Terminais with
  	Combinadores with
  	Numeros with
  	ComponenteItem =>
  	  
  import linker.remissao.Remissoes.Alinea
  import linker.remissao.Remissoes.NumeroRemissao
  
    lazy val componenteDiretoAlinea = flatListaDe(componenteDiretoAlineaPlural | componenteDiretoAlineaSingular) ^? {
    case l if l nonEmpty => l
  }

  lazy val componenteDiretoAlineaSemNome = flatListaDe(componenteDiretoAlineaPlural | componenteDiretoAlineaSingularSemNome)

  lazy val componenteDiretoAlineaPlural: Parser[List[Alinea]] =
    (opt(as | nas) ~>
      opt(nos ~ termos ~ (das | as)) ~>
      alineas ~>
      componenteDiretoAlineaSemNome)

  lazy val componenteDiretoAlineaSingular =
    opt(a | na | da | separador ~ opt(da) | (opt(separador) ~ nos ~ termos ~ (da | a))) ~> alinea ~>
      numeroComComplemento(numeroAlfabeto) ~ opt(separador ~> componenteDiretoItem) ^^ {
        case num ~ Some(itens) => itens map {
          i => Alinea(NumeroRemissao((num.map(_.num)): _*), Some(i))
        }
        case num ~ None =>
          List(Alinea(NumeroRemissao((num.map(_.num)): _*), None))
      }

  lazy val componenteDiretoAlineaSingularSemNome =
    opt(a | na | da | separador ~ opt(da) | (opt(separador) ~ nos ~ termos ~ (da | a))) ~>
      numeroComComplemento(numeroAlfabeto) ~ opt(separador ~> componenteDiretoItem) ^^ {
        case num ~ Some(itens) => itens map {
          i => Alinea(NumeroRemissao((num.map(_.num)): _*), Some(i))
        }
        case num ~ None =>
          List(Alinea(NumeroRemissao((num.map(_.num)): _*),None))
      }
      
  lazy val componenteAlinea = flatListaDe(componenteAlineaPlural | componenteAlineaSingular)

  lazy val componenteAlineaSingular = opt(componenteItem) ~
    (opt(opt(todos | todas | ambos | ambas | nos ~ termos) ~ (da | separador ~ opt(da))) ~>
      alinea ~> numeroComComplemento(numeroAlfabeto)) ^^ {
        case Some(item) ~ a => item map { 
          i => Alinea(NumeroRemissao(a.map(_.num): _*), Some(i)) 
        }
        case None ~ a => 
          List(Alinea(NumeroRemissao(a.map(_.num): _*), None))
      }

  lazy val componenteAlineaPlural = opt(os | nos) ~> alineas ~>
    listaNumeros(numeroComComplemento(numeroAlfabeto)) ^^ {
      l =>
        l map { n =>
          Alinea(NumeroRemissao(n.map(_.num): _*), None)
        }
    }

}
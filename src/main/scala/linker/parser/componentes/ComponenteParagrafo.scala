package linker.parser.componentes

import linker.parser.BasicLinkerParser
import linker.parser.Combinadores
import linker.parser.Terminais
import linker.parser.numeros.Numeros
import scala.language.postfixOps

trait ComponenteParagrafo {
  self : BasicLinkerParser with Terminais with Numeros with Combinadores with ComponenteInciso =>

  import lexical.Numero
  import linker.remissao.Remissoes.Paragrafo
  import linker.remissao.Remissoes.Caput
  import linker.remissao.Remissoes.NumeroRemissao

  lazy val componenteDiretoParagrafo = flatListaDe(componenteDiretoParagrafoPlural | componenteDiretoParagrafoSingular | componenteDiretoParagrafoCaputImplicito) ^? {
    case l if l nonEmpty => l
  }

  lazy val componenteDiretoParagrafoSemNome = flatListaDe(componenteDiretoParagrafoPlural | componenteDiretoParagrafoSingularSemNome | componenteDiretoParagrafoCaputImplicito)

  lazy val componenteDiretoParagrafoPlural : Parser[List[Paragrafo]] =
    (opt(os | nos) ~>
      opt(termos ~ (dos | os)) ~>
      paragrafos ~>
      componenteDiretoParagrafoSemNome)

  lazy val componenteDiretoParagrafoSingular =
    opt(o | no | do_ | separador ~ opt(do_) | (opt(separador) ~ nos ~ termos ~ (do_ | o))) ~>
      (paragrafo ~> (numeroComComplemento(numeroOrdinal) | numeroComComplemento(numeroArabicoMaiorQue10)) | numCaput) ~ opt(separador ~> componenteDiretoInciso) ^^ {
        case Nil ~ Some(incisos) => incisos map {
          i => Paragrafo(Caput, Some(i))
        }
        case Nil ~ None => List(Paragrafo(Caput, None))

        case num ~ Some(incisos) => incisos map {
          i => Paragrafo(NumeroRemissao((num.map(_.num)) : _*), Some(i))
        }
        case num ~ None => List(Paragrafo(NumeroRemissao((num.map(_.num)) : _*), None))
      }

  lazy val componenteDiretoParagrafoSingularSemNome =
    opt(o | no | do_ | separador ~ opt(do_) | (opt(separador) ~ nos ~ termos ~ (do_ | o))) ~>
      (numeroComComplemento(numeroOrdinal) | numeroComComplemento(numeroArabicoMaiorQue10) | numCaput) ~ opt(separador ~> componenteDiretoInciso) ^^ {
        case Nil ~ Some(incisos) => incisos map {
          i => Paragrafo(Caput, Some(i))
        }
        case Nil ~ None => List(Paragrafo(Caput, None))
        case num ~ Some(incisos) => incisos map {
          i => Paragrafo(NumeroRemissao((num.map(_.num)) : _*), Some(i))
        }
        case num ~ None => List(Paragrafo(NumeroRemissao((num.map(_.num)) : _*), None))

      }

  lazy val componenteDiretoParagrafoCaputImplicito =
    componenteDiretoInciso ^^ {
      case incisos if incisos nonEmpty => incisos map {
        i => Paragrafo(Caput, Some(i))
      }
      case _ => Nil
    }

  lazy val componenteParagrafo = flatListaDe(componenteParagrafoPlural | componenteParagrafoSingular)

  lazy val componenteParagrafoSingular = opt(componenteInciso) ~
    (opt(opt(todos | todas | ambos | ambas | nos ~ termos) ~ (do_ | separador ~ opt(do_))) ~>
      paragrafo ~> (numeroComComplemento(numeroOrdinal) | numeroComComplemento(numeroArabicoMaiorQue10) | numCaput)) ^? {
        case Some(incisos) ~ Nil => incisos map {
          i => Paragrafo(Caput, Some(i))
        }
        case Some(incisos) ~ p => incisos map {
          i => Paragrafo(NumeroRemissao(p.map(_.num) : _*), Some(i))
        }
        case None ~ p if p.length > 0 => List(Paragrafo(NumeroRemissao(p.map(_.num) : _*), None))
      } | componenteInciso ^? {
        case incisos if incisos.length > 0 => incisos map {
          i => Paragrafo(Caput, Some(i))
        }
      }

  lazy val componenteParagrafoPlural = opt(os | nos) ~> paragrafos ~>
    listaNumeros((numeroComComplemento(numeroOrdinal) | numeroComComplemento(numeroArabicoMaiorQue10) | numCaput)) ^^ {
      l =>
        l map {
          case Nil => Paragrafo(Caput, None)
          case n   => Paragrafo(NumeroRemissao(n.map(_.num) : _*), None)
        }
    }
}
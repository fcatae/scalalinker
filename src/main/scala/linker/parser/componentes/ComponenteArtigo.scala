package linker.parser.componentes

import linker.parser.BasicLinkerParser
import linker.parser.Combinadores
import linker.parser.Terminais
import linker.parser.numeros.Numeros
import scala.language.postfixOps

trait ComponenteArtigo {
  self : BasicLinkerParser with Terminais with Numeros with Combinadores with ComponenteParagrafo =>

  import linker.remissao.Remissoes.Artigo
  import linker.remissao.Remissoes.NumeroRemissao

  lazy val componenteDiretoArtigo =
    flatListaDe(componenteDiretoArtigoPlural | componenteDiretoArtigoSingular) ^? {
      case l if l nonEmpty => l
    }

  lazy val componenteDiretoArtigoSemNome = flatListaDe(componenteDiretoArtigoPlural | componenteDiretoArtigoSingularSemNome)

  lazy val componenteDiretoArtigoPlural : Parser[List[Artigo]] =
    (opt(os | nos) ~>
      opt(termos ~ (dos | os)) ~>
      artigos ~>
      componenteDiretoArtigoSemNome)

  lazy val componenteDiretoArtigoSingular =
    opt(o | no | do_ | separador ~ opt(do_) | (opt(separador) ~ nos ~ termos ~ (do_ | o))) ~>
      artigo ~> (numeroComComplemento(numeroOrdinal) | numeroComComplemento(numeroArabicoMaiorQue10)) ~ opt(separador ~> componenteDiretoParagrafo) ^^ {
        case num ~ Some(paragrafos) => paragrafos map {
          p => Artigo(NumeroRemissao((num.map(_.num)) : _*), Some(p))
        }
        case num ~ None =>
          List(Artigo(NumeroRemissao((num.map(_.num)) : _*), None))

      }

  lazy val componenteDiretoArtigoSingularSemNome =
    opt(o | no | do_ | separador ~ opt(do_) | (opt(separador) ~ nos ~ termos ~ (do_ | o))) ~>
      (numeroComComplemento(numeroOrdinal) | numeroComComplemento(numeroArabicoMaiorQue10)) ~ opt(separador ~> componenteDiretoParagrafo) ^^ {
        case num ~ Some(paragrafos) => paragrafos map {
          p => Artigo(NumeroRemissao((num.map(_.num)) : _*), Some(p))
        }
        case num ~ None =>
          List(Artigo(NumeroRemissao((num.map(_.num)) : _*), None))
      }

  lazy val componenteArtigo = flatListaDe(componenteArtigoPlural | componenteArtigoSingular) ^? {
    case l if l nonEmpty => l
  }

  lazy val componenteArtigoSingular = opt(componenteParagrafo) ~
    (opt(opt(todos | todas | ambos | ambas | nos ~ termos) ~ (do_ | separador ~ opt(do_))) ~>
      artigo ~> (numeroComComplemento(numeroOrdinal) | numeroComComplemento(numeroArabicoMaiorQue10))) ^^ {
        case Some(paragrafos) ~ a => paragrafos map {
          p => Artigo(NumeroRemissao(a.map(_.num) : _*), Some(p))
        }
        case None ~ a => List(Artigo(NumeroRemissao(a.map(_.num) : _*), None))
      }

  lazy val componenteArtigoPlural = opt(os | nos) ~> artigos ~>
    listaNumeros((numeroComComplemento(numeroOrdinal) | numeroComComplemento(numeroArabicoMaiorQue10))) ^^ {
      l =>
        l map { n =>
          Artigo(NumeroRemissao(n.map(_.num) : _*), None)
        }
    }

}
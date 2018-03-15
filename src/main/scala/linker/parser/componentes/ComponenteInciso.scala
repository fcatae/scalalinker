package linker.parser.componentes

import linker.parser.BasicLinkerParser
import linker.parser.Combinadores
import linker.parser.Terminais
import linker.parser.numeros.Numeros
import scala.language.postfixOps

trait ComponenteInciso {

  self : BasicLinkerParser with Terminais with Combinadores with Numeros with ComponenteAlinea =>

  import linker.remissao.Remissoes.Inciso
  import linker.remissao.Remissoes.NumeroRemissao

  lazy val componenteDiretoInciso = flatListaDe(componenteDiretoIncisoPlural | componenteDiretoIncisoSingular) ^? {
    case l if l nonEmpty => l
  }

  lazy val componenteDiretoIncisoSemNome = flatListaDe(componenteDiretoIncisoPlural | componenteDiretoIncisoSingularSemNome)

  lazy val componenteDiretoIncisoPlural : Parser[List[Inciso]] =
    (opt(os | nos) ~>
      opt(termos ~ (dos | os)) ~>
      incisos ~>
      componenteDiretoIncisoSemNome)

  lazy val componenteDiretoIncisoSingular =
    opt(o | no | do_ | separador ~ opt(do_) | (opt(separador) ~ nos ~ termos ~ (do_ | o))) ~> inciso ~>
      (numeroComComplemento(numeroArabico) | numeroComComplemento(numeroRomano)) ~ opt(separador ~> componenteDiretoAlinea) ^^ {
        case num ~ Some(alineas) => alineas map {
          a => Inciso(NumeroRemissao((num.map(_.num)) : _*), Some(a))
        }
        case num ~ None =>
          List(Inciso(NumeroRemissao((num.map(_.num)) : _*), None))
      }

  lazy val componenteDiretoIncisoSingularSemNome =
    opt(o | no | do_ | separador ~ opt(do_) | (opt(separador) ~ nos ~ termos ~ (do_ | o))) ~>
      (numeroComComplemento(numeroArabico) | numeroComComplemento(numeroRomano)) ~ opt(separador ~> componenteDiretoAlinea) ^^ {
        case num ~ Some(alineas) => alineas map {
          a => Inciso(NumeroRemissao((num.map(_.num)) : _*), Some(a))
        }
        case num ~ None => List(Inciso(NumeroRemissao((num.map(_.num)) : _*), None))
      }

  lazy val componenteInciso = flatListaDe(componenteIncisoPlural | componenteIncisoSingular)

  lazy val componenteIncisoSingular = opt(componenteAlinea) ~
    (opt(opt(todos | todas | ambos | ambas | nos ~ termos) ~ (do_ | separador ~ opt(do_))) ~>
      inciso ~> (numeroComComplemento(numeroArabico) | numeroComComplemento(numeroRomano))) ^^ {
        case Some(alineas) ~ i => alineas map {
          a => Inciso(NumeroRemissao(i.map(_.num) : _*), Some(a))
        }
        case None ~ i =>
          List(Inciso(NumeroRemissao(i.map(_.num) : _*), None))
      }

  lazy val componenteIncisoPlural = opt(os | nos) ~> incisos ~>
    listaNumeros((numeroComComplemento(numeroArabico) | numeroComComplemento(numeroRomano))) ^^ {
      l =>
        l map { n =>
          Inciso(NumeroRemissao(n.map(_.num) : _*), None)
        }
    }

}
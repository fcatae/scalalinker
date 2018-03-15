package linker.parser

import scala.util.parsing.combinator.PackratParsers

import componentes.ComponenteArtigo
import componentes.ComponenteParagrafo
import componentes.ComponenteInciso
import componentes.ComponenteAlinea
import componentes.ComponenteItem
import norma.Normas
import norma.MunicipiosParser
import norma.EstadosParser
import numeros.Numeros

object LinkerParser extends 
	BasicLinkerParser with
	PackratParsers with
	ComponenteArtigo with
	ComponenteParagrafo with
	ComponenteInciso with
	ComponenteAlinea with
	ComponenteItem with
	Normas with
	MunicipiosParser with
    EstadosParser with
    TrieParser with
	Terminais with
	Combinadores with
	Numeros {

  import lexical.Scanner

  lazy val texto = rep(naoPalavraInicial) ~> rep(remissao <~ rep(naoPalavraInicial)) ^^ {
    remissoes => remissoes.flatten
  }

  lazy val remissao = (componenteDiretoArtigo | componenteArtigo) ~ norma ^^ {
    case Nil ~ norma => List(norma)
    case artigos ~ norma => artigos map { 
      a => norma.copy(artigo = Some(a))
    }
  }

  def apply[T](p: Parser[T], s: String) = {

    phrase(p)(new PackratReader(new Scanner(s)))
    //phrase(p)(new Scanner(s))
  }

}

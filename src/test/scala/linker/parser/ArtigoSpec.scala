package linker.parser

import org.specs2._
import LinkerParser._
import linker.remissao.Remissoes.Artigo
import linker.remissao.Remissoes.NumeroRemissao

class ArtigoDiretoMenorQue10Spec extends Specification {
  def is =
    "Especificacao de chamada do parser" ^
      "chamando o parser com a produção componenteDiretoArtigo e o texto 'art. 1\u00ba'" ^
      "deve retornar um artigo com o número 1" ! exemplo ^ end

  def exemplo = LinkerParser(componenteDiretoArtigo, "art. 1\u00ba").getOrElse(Nil) mustEqual List(Artigo(NumeroRemissao(1), None))
}

class ArtigoDiretoIgualA10Spec extends Specification {
  def is =
    "Especificacao de chamada do parser" ^
      "chamando o parser com a produção componenteDiretoArtigo e o texto 'art. 10\u00ba'" ^
      "deve retornar um artigo com o número 10" ! exemplo ^ end

  def exemplo = LinkerParser(componenteDiretoArtigo, "art. 10\u00ba").getOrElse(Nil) mustEqual List(Artigo(NumeroRemissao(10), None))
}

class ArtigoDiretoMaiorQue10Spec extends Specification {
  def is =
    "Especificacao de chamada do parser" ^
      "chamando o parser com a produção componenteDiretoArtigo e o texto 'art. 11'" ^
      "deve retornar um artigo com o número 11" ! exemplo ^ end

  def exemplo = LinkerParser(componenteDiretoArtigo, "art. 11").getOrElse(Nil) mustEqual List(Artigo(NumeroRemissao(11), None))

}

class ArtigoMenorQue10Spec extends Specification {
  def is =
    "Especificacao de chamada do parser" ^
      "chamando o parser com a produção componenteArtigo e o texto 'art. 1\u00ba'" ^
      "deve retornar um artigo com o número 1" ! exemplo ^ end

  def exemplo = LinkerParser(componenteArtigo, "art. 1\u00ba").getOrElse(Nil) mustEqual List(Artigo(NumeroRemissao(1), None))
}

class ArtigoIgualA10Spec extends Specification {
  def is =
    "Especificacao de chamada do parser" ^
      "chamando o parser com a produção componenteArtigo e o texto 'art. 10\u00ba'" ^
      "deve retornar um artigo com o número 10" ! exemplo ^ end

  def exemplo = LinkerParser(componenteArtigo, "art. 10\u00ba").getOrElse(Nil) mustEqual List(Artigo(NumeroRemissao(10), None))
}

class ArtigoMaiorQue10Spec extends Specification {
  def is =
    "Especificacao de chamada do parser" ^
      "chamando o parser com a produção componenteArtigo e o texto 'art. 11'" ^
      "deve retornar um artigo com o número 11" ! exemplo ^ end

  def exemplo = LinkerParser(componenteArtigo, "art. 11").getOrElse(Nil) mustEqual List(Artigo(NumeroRemissao(11), None))

}

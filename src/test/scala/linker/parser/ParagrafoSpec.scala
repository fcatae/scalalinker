package linker.parser

import org.specs2._
import LinkerParser._
import linker.remissao.Remissoes.Paragrafo
import linker.remissao.Remissoes.NumeroRemissao

class ParagrafoImplicitoSpec extends Specification {
  def is =
    "Especificacao de chamada do parser" ^
      "chamando o parser com a produção componenteDiretoParagrafo e o texto '\u00a7 1\u00ba'" ^
      "deve retornar um parágrafo com o número 1" ! exemplo ^ end

  def exemplo = LinkerParser(componenteDiretoParagrafo, "\u00a7 1\u00ba").getOrElse(Nil) mustEqual List(Paragrafo(NumeroRemissao(1), None))

}
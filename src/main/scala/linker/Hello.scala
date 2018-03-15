package linker

import linker.parser.LinkerParser
import linker.remissao.urn.LexMLURN.NormaToLexMLURN

object Hello {

  import LinkerParser.texto

  def main(args : Array[String]) {
    //    val original = "nos termos dos arts. 22, par. 3\272, e 100, par. 15, inc. V da constituicao federal e do inc. IV, \247 3\272 do art. 7\272 da lei distrital 8.112/90 do distrito federal"
    val original = "nos termos dos arts. 22, par. 3o, e 100, par. 15, inc. V da constituicao federal e do inc. IV, par. 3o do art. 7o da lei distrital 8.112/90 do distrito federal"

    println(original)
    LinkerParser(texto, original) match {
      case LinkerParser.Success(r, _) => r.foreach(n => println(NormaToLexMLURN.toURN(n)))
      case e                          => println(e)
    }
  }
}

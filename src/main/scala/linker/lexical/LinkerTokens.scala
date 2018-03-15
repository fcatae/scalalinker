package linker.lexical

import scala.util.parsing.combinator.token.Tokens
import scala.util.parsing.input.Positional

trait LinkerTokens extends Tokens {

  abstract class LinkerToken extends Token with Positional

  case class Palavra(chars: String, inicial: Boolean) extends LinkerToken

  case class Numero(num: Int) extends LinkerToken {
    def chars = num.toString
  }

  case class IndicadorOrdinal(chars: String) extends LinkerToken

  case class Simbolo(chars: String) extends LinkerToken

  case object AspasDuplas extends LinkerToken {
    def chars = "\""
  }

  case object LinkerEOF extends LinkerToken {
    def chars = "\u001a"
  }

  case object Paragrafo extends LinkerToken {
    def chars = "\u00a7"
  }

  case object Paragrafos extends LinkerToken {
    def chars = "\u00a7\u00a7"
  }

  case object Ordinal extends LinkerToken {
    def chars = "1o"
  }

  case object Ponto extends LinkerToken {
    def chars = "."
  }

  case object Virgula extends LinkerToken {
    def chars = ","
  }

  case object PontoEVirgula extends LinkerToken {
    def chars = ";"
  }

  case object Hifen extends LinkerToken {
    def chars = "-"
  }

  case object Barra extends LinkerToken {
    def chars = "/"
  }

}
package linker.parser.numeros

import scala.util.parsing.combinator.PackratParsers
import linker.parser.CombinadoresGenericos
import scala.util.parsing.input.CharArrayReader

object NumeroRomanoParser extends PackratParsers with CombinadoresGenericos {

  type Elem = Char

  lazy val milhar = repNM(1, 3, 'm') ^^ { case l => l.length * 1000 }
  lazy val centena = padraoNumeroRomano('c', 'd', 'm', 100)
  lazy val dezena = padraoNumeroRomano('x', 'l', 'c', 10)
  lazy val unidade = padraoNumeroRomano('i', 'v', 'x', 1)

  def padraoNumeroRomano(unidade: Char, cinco: Char, dezena: Char, potencia: Int) =
    unidade ~ dezena ^^^ 9 * potencia |
      unidade ~ cinco ^^^ 4 * potencia |
      cinco ~> repNM(0, 3, unidade) ^^ { case l => (5 + l.length) * potencia } |
      repNM(1, 3, unidade) ^^ { case l => l.length * potencia }

  lazy val romano =
    opt(milhar) ~ opt(centena) ~ opt(dezena) ~ unidade ^^ {
      case om ~ oc ~ od ~ u =>
        om.getOrElse(0) + oc.getOrElse(0) + od.getOrElse(0) + u
    } |
      opt(milhar) ~ opt(centena) ~ dezena ~ opt(unidade) ^^ {
        case om ~ oc ~ d ~ ou =>
          om.getOrElse(0) + oc.getOrElse(0) + d + ou.getOrElse(0)
      } |
      opt(milhar) ~ centena ~ opt(dezena) ~ opt(unidade) ^^ {
        case om ~ c ~ od ~ ou =>
          om.getOrElse(0) + c + od.getOrElse(0) + ou.getOrElse(0)
      } |
      milhar ~ opt(centena) ~ opt(dezena) ~ opt(unidade) ^^ {
        case m ~ oc ~ od ~ ou =>
          m + oc.getOrElse(0) + od.getOrElse(0) + ou.getOrElse(0)
      }

  def apply(num: String) = {
    val romanoParser = phrase(romano)
    romanoParser(new PackratReader(new CharArrayReader(num.toCharArray)))
  }

}
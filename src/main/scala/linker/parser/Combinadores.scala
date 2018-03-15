package linker.parser

trait Combinadores extends BasicLinkerParser {

  self: BasicLinkerParser with Terminais =>

  import lexical.LinkerEOF
  import lexical.Palavra
  import lexical.PontoEVirgula
  import lexical.Virgula

  lazy val sempre = acceptIf({
    case LinkerEOF => false
    case _ => true
  })(_ => "")

  def flatListaDe[T](parser: Parser[List[T]]) =
    flatListaSeparadaPor(parser, PontoEVirgula) ||| flatListaSeparadaPor(parser, Virgula)
    //flatListaSeparadaPor(parser, separador)

  def flatListaSeparadaPor[T](parser: Parser[List[T]], separador: Parser[Any]) =
    rep1sep(parser, separador) ~ opt((opt(separador) ~ e) ~> parser) ^^ {
      case l ~ Some(elt) => l.flatten ++ elt
      case l ~ None => l.flatten
    }

  def listaDe[T](parser: Parser[T]) =
    listaSeparadaPor(parser, PontoEVirgula) ||| listaSeparadaPor(parser, Virgula)
    //listaSeparadaPor(parser, separador)

  def listaSeparadaPor[T](parser: Parser[T], separador: Parser[Elem]) =
    rep1sep(parser, separador) ~ opt((opt(separador) ~ e) ~> parser) ^^ {
      case l ~ Some(last) => l :+ last
      case l ~ None => l
    }

}
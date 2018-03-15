package linker.parser.numeros

import linker.parser.BasicLinkerParser
import linker.parser.Combinadores
import linker.parser.Terminais

trait Numeros {
  self : BasicLinkerParser with Terminais with Combinadores =>

  import lexical.AspasDuplas
  import lexical.Hifen
  import lexical.Numero
  import lexical.Ordinal
  import lexical.Palavra
  import lexical.Ponto

  def listaNumeros(n : Parser[List[Numero]]) =
    flatListaDe(intervalo(n))

  def intervalo(n : Parser[List[Numero]]) =
    n ~ opt(a ~> n) ^^ {
      case l ~ l1 => List(l) ++ l1.toList
    }

  lazy val numCaput : Parser[List[Numero]] = caput ^^ {
    a => Nil
  }

  lazy val numeroOrdinal : Parser[Numero] =
    Ordinal ^^^ Numero(1) |
      numero <~ (indicadorOrdinal | o | a ~ not(numero)) |
      ordinalExtenso

  lazy val ordinalExtenso =
    (unico | primeiro) ^^^ Numero(1) |
      segundo ^^^ Numero(2) |
      terceiro ^^^ Numero(3) |
      quarto ^^^ Numero(4) |
      quinto ^^^ Numero(5) |
      sexto ^^^ Numero(6) |
      setimo ^^^ Numero(7) |
      oitavo ^^^ Numero(8) |
      nono ^^^ Numero(9) |
      decimo ^^^ Numero(10)

  lazy val numeroArabico = numero ~ rep(Ponto ~> acceptMatch("numero", {
    case Numero(n) if n < 1000 => Numero(n)
  })) ^^ {
    case n ~ l => Numero(((n +: l).map(_.num.toString)).mkString("").toInt)
  }

  lazy val numeroArabicoMaiorQue10 : Parser[Numero] = numeroArabico ^? {
    case Numero(n) if n > 10 => Numero(n)
  }

  lazy val numeroAlfabeto = (AspasDuplas ~> letra) <~ AspasDuplas ^^ {
    case Palavra(p, _) => Numero(p(0).toInt - 'a'.toInt)
  }

  lazy val letra : Parser[Palavra] = palavra ^? {
    case Palavra(p, i) if p.length == 1 => Palavra(p, i)
  }

  lazy val numeroRomano = palavra ^? {
    case Palavra(p, _) if isNumeroRomano(p) => getNumeroRomano(p)
  } | failure("falhou ao interpretar número romano")

  def isNumeroRomano(s : String) = NumeroRomanoParser(s) match {
    case NumeroRomanoParser.Success(_, _) => true
    case _                                => false
  }

  def getNumeroRomano(s : String) = NumeroRomanoParser(s) match {
    case NumeroRomanoParser.Success(n, _) => Numero(n)
  }

  def numeroComComplemento(p : Parser[Numero]) : Parser[List[Numero]] = {
    p ~ opt(Hifen ~> palavra) ^^ {
      case n ~ Some(Palavra(c, _)) => List(n, complementoToNumero(c))
      case n ~ None                => List(n)
    }
  }

  def complementoToNumero(complemento : String) = Numero(
    fixMsd(complemento.toList).foldLeft(0) { (d0 : Int, d1 : Char) =>
      d0 * (1 + 'z' - 'a') + (d1 - 'a')
    })

  def fixMsd(l : List[Char]) = l match {
    case h0 :: h1 :: t => nextChar(h0) :: h1 :: t
    case l0            => l0
  }

  def nextChar(c : Char) = (c + 1).toChar

}
package linker.lexical

import scala.util.parsing.combinator.lexical.Lexical

object LinkerLexical extends Lexical with LinkerTokens {

  def whitespace = rep(accept(' ') | '\t' | '\n' | '\f' | '\r')

  def token : Parser[Token] = positioned(
    '1' ~ 'o' ^^^ Ordinal |
      '\u00a7' ~ '\u00a7' ^^^ Paragrafos |
      '\u00a7' ^^^ Paragrafo |
      '.' ^^^ Ponto |
      ',' ^^^ Virgula |
      ';' ^^^ PontoEVirgula |
      '-' ^^^ Hifen |
      '/' ^^^ Barra |
      '\"' ^^^ AspasDuplas |
      '\u001a' ^^^ LinkerEOF |
      indicadorOrdinal |
      palavra |
      numero |
      simbolo)

  def palavra = rep1(letter) ^^ { processaPalavra(_) }
  def numero = rep1(digit) ^^ { (x => Numero(x.mkString("").toInt)) }
  def indicadorOrdinal = (elem('\u00aa') | '\u00b0' | '\u00ba') ^^ { (x => IndicadorOrdinal(x.toString)) }
  def simbolo = chrExcept('\u001a') ^^ { (x => Simbolo(x.toString)) }

  def processaPalavra(l : List[Elem]) = {
    val palavra = l.mkString("").toLowerCase
    Palavra(palavra, palavrasIniciais contains palavra)
  }

  val palavrasIniciais = Set(
    "alínea",
    "alinea",
    "alíneas",
    "alineas",
    "art",
    "artigo",
    "artigos",
    "arts",
    "ato",
    "caput",
    "clt",
    "consolidação",
    "consolidacao",
    "constituição",
    "constituicao",
    "cpt",
    "decreto",
    "decretos",
    "emenda",
    "emendas",
    "inc",
    "inciso",
    "incisos",
    "incs",
    "item",
    "itens",
    "lei",
    "leis",
    "letra",
    "letras",
    "medida",
    "medidas",
    "par",
    "parágrafo",
    "paragrafo",
    "parágrafos",
    "paragrafos",
    "pars",
    "projeto",
    "projetos",
    "regimento",
    "regulamento",
    "resolução",
    "resolucao",
    "resoluções",
    "resolucoes",
    "súmula",
    "sumula")

}

package linker.parser.norma

import linker.parser.BasicLinkerParser
import linker.parser.Combinadores
import linker.parser.Terminais
import linker.parser.TrieParser
import linker.remissao.Remissoes._
import linker.parser.numeros.Numeros

sealed trait QualificadorNorma
case class QualificadorNumeroNorma(numero : List[Int]) extends QualificadorNorma
case class QualificadorDataNorma(data : DataNorma) extends QualificadorNorma
case class QualificadorAutoridadeNorma(autoridade : List[Sujeito]) extends QualificadorNorma
case class QualificadorEstadoNorma(estado : List[String]) extends QualificadorNorma
case class QualificadorMunicipioNorma(municipio : List[String], estado : List[String]) extends QualificadorNorma

trait Normas {

  self : BasicLinkerParser with Combinadores with Terminais with Numeros with MunicipiosParser with EstadosParser with TrieParser =>

  import lexical.Barra
  import lexical.Hifen
  import lexical.Numero
  import lexical.Palavra
  import lexical.Virgula

  val autoridadeSenado = AutoridadeNormal(List(Instituicao(List("senado", "federal"))))
  val acFederal = AutoridadeConvencionada(Some(Federal))
  val acEstadual = AutoridadeConvencionada(Some(Estadual))
  val acDistrital = AutoridadeConvencionada(Some(Distrital))
  val acMunicipal = AutoridadeConvencionada(Some(Municipal))

  lazy val norma = (opt(separador) ~ opt(todos | todas | ambos | ambas) ~ opt(de | do_ | da)) ~>
    (constituicao1988 | apelidos | normaExtenso)

  lazy val constituicao1988 = constituicao ~ federal ^^ {
    _ => Norma(tipo = List("constituicao"), autoridade = acFederal, data = Some(DataCompletaNorma(5, 10, 1988)), numero = Some(List(1988)))
  }

  lazy val apelidos =
    regimento ~ interno ~ do_ ~ senado ~ opt(federal) ^^ {
      _ => Norma(tipo = List("resolucao"), data = Some(DataCompletaNorma(27, 11, 1970)), numero = Some(List(93)), autoridade = autoridadeSenado)
    } |
      regulamento ~ administrativo ~ do_ ~ senado ~ opt(federal) ^^ {
        _ => Norma(tipo = List("resolucao"), data = Some(DataCompletaNorma(10, 11, 1970)), numero = Some(List(58)), autoridade = autoridadeSenado)
      } |
      ato ~ das ~ disposicoes ~ constitucionais ~ transitorias ^^ {
        _ => Norma(tipo = List("ato", "disposicoes", "constitucionais", "transitorias"), autoridade = acFederal, data = Some(DataCompletaNorma(5, 10, 1988)), numero = Some(List(1988)))
      } |
      (consolidacao ~ das ~ leis ~ do_ ~ trabalho | clt) ^^ {
        _ => Norma(tipo = List("decreto", "lei"), autoridade = acFederal, data = Some(DataCompletaNorma(1, 5, 1943)), numero = Some(List(5452)))
      } |
      (cf ~ Barra) ~> numero ^^ {
        case Numero(n) => Norma(tipo = List("constituicao"), autoridade = acFederal, data = Some(AnoNorma(n)), numero = Some(List(n)))
      }
  lazy val normaExtenso = tipoNorma ~ rep1(qualificador | ignoraQualificador) ^^ {
    case n ~ l =>
      val l0 = l.flatten
      l0.foldLeft(n)(consolida)
  }

  lazy val tipoNorma =
    tipoNormaLei |
      (resolucao | resolucoes) ^^ { _ => Norma(tipo = List("resolucao")) } |
      tipoNormaDecreto |
      emenda ~ constitucional ^^ { _ => Norma(tipo = List("emenda", "constitucional")) } |
      (medida ~ provisoria | medidas ~ provisorias) ^^ { _ => Norma(tipo = List("medida", "provisoria")) }

  lazy val tipoNormaLei =
    (lei ~> opt(delegada | complementar) ~ opt(esferaSingular) |
      leis ~> opt(delegadas | complementares) ~ opt(esferaPlural)) ^^ {
        case Some(Palavra(p, _)) ~ Some(n) => n.copy(tipo = List("lei", p))
        case Some(Palavra(p, _)) ~ None    => Norma(tipo = List("lei", p))
        case None ~ Some(n)                => n.copy(tipo = List("lei"))
        case None ~ None                   => Norma(tipo = List("lei"))
      }

  lazy val esferaSingular =
    distrital ^^ { _ => Norma(autoridade = acDistrital) } |
      estadual ^^ { _ => Norma(autoridade = acEstadual) } |
      federal ^^ { _ => Norma(autoridade = acFederal) } |
      municipal ^^ { _ => Norma(autoridade = acMunicipal) }

  lazy val esferaPlural =
    distritais ^^ { _ => Norma(autoridade = acDistrital) } |
      estaduais ^^ { _ => Norma(autoridade = acEstadual) } |
      federais ^^ { _ => Norma(autoridade = acFederal) } |
      municipais ^^ { _ => Norma(autoridade = acMunicipal) }

  lazy val tipoNormaDecreto =
    (decreto ~> opt(Hifen ~ lei) ~ opt(esferaSingular) |
      decretos ~> opt(Hifen ~ leis) ~ opt(esferaPlural)) ^^ {
        case Some(_) ~ Some(n) => n.copy(tipo = List("decreto", "lei"))
        case Some(_) ~ None    => Norma(tipo = List("decreto", "lei"))
        case None ~ Some(n)    => n.copy(tipo = List("decreto"))
        case None ~ None       => Norma(tipo = List("decreto"))

      }

  lazy val qualificador = opt(separador) ~> (qualificadorNumero | qualificadorData | qualificadorEstados | qualificadorMunicipios | qualificadorAutoridade)

  lazy val ignoraQualificador : Parser[List[QualificadorNorma]] = Hifen ~ rep(acceptIf {
    case Palavra(p, _) if p != "e" => true
    case _                         => false
  }(_ => "não é palavra")) ^^^ Nil

  lazy val qualificadorNumero : Parser[List[QualificadorNorma]] = opt(opt(e) ~ abrevNumero) ~> numeroComComplemento(numeroArabico) ^^ {
    n => List(QualificadorNumeroNorma(n map (_.num)))
  }

  lazy val qualificadorData : Parser[List[QualificadorNorma]] =
    (opt(ambos | ambas | todos | todas) ~ opt(de)) ~> parseData ^^ { d => List(QualificadorDataNorma(d)) } |
      Barra ~> numero ^? {
        case Numero(n) if Range(0, 99).contains(n) || Range(1800, 2100).contains(n) => List(QualificadorDataNorma(AnoNorma(if (n < 100) n + 1900 else n)))
      }

  lazy val parseData = dataExtenso | dataAbreviada

  lazy val dataExtenso = ((numero | numeroOrdinal) <~ opt(de)) ~ (parseMes <~ opt(Virgula ~ de)) ~ numero ^^ {
    case Numero(d) ~ m ~ Numero(a) => DataCompletaNorma(d, m, a)
  }

  lazy val dataAbreviada = (numero <~ Barra) ~ (numero <~ Barra) ~ numero ^^ {
    case Numero(d) ~ Numero(m) ~ Numero(a) => DataCompletaNorma(d, m, a)
  }

  lazy val parseMes =
    janeiro ^^^ 1 |
      fevereiro ^^^ 2 |
      marco ^^^ 3 |
      abril ^^^ 4 |
      maio ^^^ 5 |
      junho ^^^ 6 |
      julho ^^^ 7 |
      agosto ^^^ 8 |
      setembro ^^^ 9 |
      outubro ^^^ 10 |
      novembro ^^^ 11 |
      dezembro ^^^ 12

  lazy val qualificadorMunicipios = opt(opt(do_ ~ municipio) ~ (de | do_ | da)) ~> parseMunicipios ^^ {
    l =>
      l.map {
        i =>
          i match {
            case (m, e) => QualificadorMunicipioNorma(m, e)
          }
      }
  }

  lazy val qualificadorEstados = opt(opt(do_ ~ estado) ~ (de | do_ | da)) ~> parseEstados ^^ {
    l =>
      l.map {
        e => QualificadorEstadoNorma(e)
      }
  }

  lazy val qualificadorAutoridade =
    Hifen ~ cn ^^^ List(QualificadorAutoridadeNorma(List(Instituicao(List("congresso", "nacional"))))) |
      do_ ~ senado ~ federal ^^^ List(QualificadorAutoridadeNorma(List(Instituicao(List("senado", "federal"))))) |
      do_ ~ conselho ~ deliberativo ~ do_ ~ fundo ~ de ~ amparo ~ ao ~ trabalhador ^^^ List(QualificadorAutoridadeNorma(List(Instituicao(List("conselho", "deliberativo", "fundo", "amparo", "trabalhador")))))

  def consolida(norma : Norma, q : QualificadorNorma) = q match {
    case QualificadorNumeroNorma(num)     => norma.copy(numero = Some(num))
    case QualificadorDataNorma(d)         => norma.copy(data = Some(d))
    case QualificadorAutoridadeNorma(a)   => norma.copy(autoridade = AutoridadeNormal(a))
    case QualificadorEstadoNorma(e)       => norma.copy(local = Some(Estado(e)))
    case QualificadorMunicipioNorma(m, e) => norma.copy(local = Some(Municipio(m, e)))
  }

}

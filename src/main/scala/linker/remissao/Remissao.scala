package linker.remissao

import java.util.Date

object Remissoes {

  type Nome = List[String]

  sealed trait Remissao

  case class Norma(
      tipo : Nome = Nil,
      data : Option[DataNorma] = None,
      numero : Option[List[Int]] = None,
      autoridade : Autoridade = AutoridadeConvencionada(),
      local : Option[Local] = None,
      artigo : Option[Artigo] = None) extends Remissao {
    override def toString =
      "norma: {\n  tipo: " + tipo.mkString(" ") +
        local.map(",\n  local: " + _).getOrElse("") +
        numero.map(",\n  numero: " + _.mkString("-")).getOrElse("") +
        data.map(",\n  data: " + _).getOrElse("") +
        ",\n  autoridade: " + autoridade +
        artigo.map(",\n  artigo: " + _.toString.replaceAll("\n", "\n  ") + "\n}").getOrElse("\n")

  }

  case class Artigo(numero : IdRemissao, paragrafo : Option[Paragrafo]) extends Remissao {
    override def toString = "{\n  numero: " + numero + paragrafo.map(",\n  paragrafo: " + _.toString.replaceAll("\n", "\n  ") + "\n}").getOrElse("\n}")
  }
  case class Paragrafo(numero : IdRemissao, inciso : Option[Inciso]) extends Remissao {
    override def toString = "{\n  numero: " + numero + inciso.map(",\n  inciso: " + _.toString.replaceAll("\n", "\n  ") + "\n}").getOrElse("\n}")
  }
  case class Inciso(numero : IdRemissao, alinea : Option[Alinea]) extends Remissao {
    override def toString = "{\n  numero: " + numero + alinea.map(",\n  alinea: " + _.toString.replaceAll("\n", "\n  ") + "\n}").getOrElse("\n}")
  }
  case class Alinea(numero : IdRemissao, item : Option[Item]) extends Remissao {
    override def toString = "{\n  numero: " + numero + item.map(",\n  item: " + _.toString.replaceAll("\n", "\n  ") + "\n}").getOrElse("\n}")
  }
  case class Item(numero : IdRemissao) extends Remissao {
    override def toString = "{\n  numero: " + numero + "\n}"
  }

  trait IdRemissao
  case class NumeroRemissao(numeros : Int*) extends IdRemissao {
    override def toString = numeros.mkString("-")
  }

  case object Caput extends IdRemissao {
    override def toString = "caput"
  }

  sealed trait EsferaNorma
  case object Distrital extends EsferaNorma {
    override def toString = "distrital"
  }
  case object Estadual extends EsferaNorma {
    override def toString = "estadual"
  }
  case object Federal extends EsferaNorma {
    override def toString = "federal"
  }
  case object Municipal extends EsferaNorma {
    override def toString = "municipal"
  }

  sealed trait DataNorma
  case class AnoNorma(ano : Int) extends DataNorma {
    override def toString = ano.toString
  }
  case class DataCompletaNorma(dia : Int, mes : Int, ano : Int) extends DataNorma {
    override def toString = dia + "/" + mes + "/" + ano
  }

  sealed trait Autoridade
  case class AutoridadeConvencionada(esfera : Option[EsferaNorma] = None) extends Autoridade {
    override def toString = "convencionada" + esfera.map(" - " + _).getOrElse("")
  }
  case class AutoridadeNormal(autoridade : List[Sujeito]) extends Autoridade {
    override def toString = autoridade mkString ", "
  }

  sealed trait Sujeito
  case class Instituicao(instituicao : Nome, orgaos : List[Nome] = Nil, funcao : Option[Nome] = None) extends Sujeito
  case class Cargo(cargo : Nome) extends Sujeito

  sealed trait Local
  case class Estado(estado : Nome) extends Local
  case class Municipio(municipio : Nome, estado : Nome) extends Local

}
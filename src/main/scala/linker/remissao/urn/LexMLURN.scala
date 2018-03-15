package linker.remissao.urn

import linker.remissao.Remissoes._

object LexMLURN {

  object NormaToLexMLURN {

    val prefix = "urn:lex:br"

    def toURN(n : Norma) =
      prefix +
        localToURN(n.local) + ":" +
        autoridadeToURN(n.autoridade) + ":" +
        tipoNormaToURN(n.tipo) + ":" +
        dataNormaToURN(n.data) + ";" +
        numeroNormaToURN(n.numero) +
        artigoNormaToURN(n.artigo)

    def localToURN(ol : Option[Local]) = ol.map(l => l match {
      case Estado(e)       => ";" + e.mkString(".")
      case Municipio(m, e) => ";" + e.mkString(".") + ";" + m.mkString(".")
    }).getOrElse("")
    def autoridadeToURN(a : Autoridade) = a match {
      case AutoridadeConvencionada(Some(e)) => e.toString
      case AutoridadeNormal(ls) => ls.map(s => s match {
        case Instituicao(i, os, of) => i.mkString(".") + os.map(o => ";" + o.mkString(".")).mkString("") + of.map(f => ";" + f.mkString(".")).getOrElse("")
        case Cargo(c)               => c.mkString(".")
      }).mkString(",")
      case _ => ""
    }
    def tipoNormaToURN(t : Nome) = t.mkString(".")
    def dataNormaToURN(od : Option[DataNorma]) = od.map(d => d match {
      case AnoNorma(a)                => a
      case DataCompletaNorma(d, m, a) => a + "-" + m + "-" + d
    }).getOrElse("")

    def numeroNormaToURN(ons : Option[List[Int]]) = ons.map(ns => ns.mkString("-")).getOrElse("")

    def artigoNormaToURN(oa : Option[Artigo]) = oa.map(a => "!art" + a.numero + paragrafoNormaToURN(a.paragrafo)).getOrElse("")
    def paragrafoNormaToURN(op : Option[Paragrafo]) = op.map(p => "_" + paragrafoIdToURN(p.numero) + incisoNormaToURN(p.inciso)).getOrElse("")
    def paragrafoIdToURN(id : IdRemissao) = id match {
      case Caput              => "cpt"
      case n : NumeroRemissao => "par" + n.toString
    }
    def incisoNormaToURN(oi : Option[Inciso]) = oi.map(i => "_inc" + i.numero + alineaNormaToURN(i.alinea)).getOrElse("")
    def alineaNormaToURN(oa : Option[Alinea]) = oa.map(a => "_ali" + a.numero + itemNormaToURN(a.item)).getOrElse("")
    def itemNormaToURN(oi : Option[Item]) = oi.map(i => "_ite" + i.numero).getOrElse("")

  }

}
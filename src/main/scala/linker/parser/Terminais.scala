package linker.parser

trait Terminais {

  self : BasicLinkerParser =>

  import lexical.IndicadorOrdinal
  import lexical.LinkerEOF
  import lexical.Numero
  import lexical.Palavra
  import lexical.Paragrafo
  import lexical.Paragrafos
  import lexical.Ponto
  import lexical.PontoEVirgula
  import lexical.Simbolo
  import lexical.Virgula

  lazy val palavra = accept("palavra", { case p : Palavra => p })
  lazy val palavraInicial = accept("palavra inicial", { case Palavra(p, true) => Palavra(p, true) })
  lazy val numero = accept("numero", { case n : Numero => n })
  lazy val simbolo = accept("simbolo", { case s : Simbolo => s })
  lazy val indicadorOrdinal = accept("indicador ordinal", { case o : IndicadorOrdinal => o })
  lazy val naoPalavraInicial = acceptIf({
    case Palavra(_, true) => false
    case LinkerEOF        => false
    case Paragrafo        => false
    case Paragrafos       => false
    case _                => true
  })(_ => "")

  // conjunções, artigos, numerais e pronomes

  lazy val e = elem(Palavra("e", false))

  lazy val o = elem(Palavra("o", false))
  lazy val a = elem(Palavra("a", false))
  lazy val os = elem(Palavra("os", false))
  lazy val as = elem(Palavra("as", false))

  lazy val ao = elem(Palavra("ao", false))
  lazy val aos = elem(Palavra("aos", false))

  lazy val de = elem(Palavra("de", false))
  lazy val do_ = elem(Palavra("do", false))
  lazy val da = elem(Palavra("da", false))
  lazy val dos = elem(Palavra("dos", false))
  lazy val das = elem(Palavra("das", false))

  lazy val no = elem(Palavra("no", false))
  lazy val na = elem(Palavra("na", false))
  lazy val nos = elem(Palavra("nos", false))
  lazy val nas = elem(Palavra("nas", false))

  lazy val termos = elem(Palavra("termos", false))

  lazy val ambas = elem(Palavra("ambas", false))
  lazy val ambos = elem(Palavra("ambos", false))
  lazy val todas = elem(Palavra("todos", false))
  lazy val todos = elem(Palavra("todas", false))

  lazy val unico = elem(Palavra("unico", false)) | elem(Palavra("único", false))
  lazy val primeiro = elem(Palavra("primeiro", false))
  lazy val segundo = elem(Palavra("segundo", false))
  lazy val terceiro = elem(Palavra("terceiro", false))
  lazy val quarto = elem(Palavra("quarto", false))
  lazy val quinto = elem(Palavra("quinto", false))
  lazy val sexto = elem(Palavra("sexto", false))
  lazy val setimo = elem(Palavra("setimo", false)) | elem(Palavra("sétimo", false))
  lazy val oitavo = elem(Palavra("oitavo", false))
  lazy val nono = elem(Palavra("nono", false))
  lazy val decimo = elem(Palavra("decimo", false)) | elem(Palavra("décimo", false))

  lazy val janeiro = elem(Palavra("janeiro", false))
  lazy val fevereiro = elem(Palavra("fevereiro", false))
  lazy val marco = elem(Palavra("marco", false)) | elem(Palavra("março", false))
  lazy val abril = elem(Palavra("abril", false))
  lazy val maio = elem(Palavra("maio", false))
  lazy val junho = elem(Palavra("junho", false))
  lazy val julho = elem(Palavra("julho", false))
  lazy val agosto = elem(Palavra("agosto", false))
  lazy val setembro = elem(Palavra("setembro", false))
  lazy val outubro = elem(Palavra("outubro", false))
  lazy val novembro = elem(Palavra("novembro", false))
  lazy val dezembro = elem(Palavra("dezembro", false))

  lazy val separador = elem(Virgula) | elem(PontoEVirgula)

  lazy val abrevNumero =
    Palavra("n", false) ~ Ponto ~ Palavra("o", false) ~ Ponto |
      Palavra("n", false) ~ opt(Ponto) ~ opt(rep1(indicadorOrdinal) ~ opt(Palavra("s", false)) ~ opt(Ponto)) |
      no |
      Palavra("numero", false) |
      Palavra("número", false) |
      nos

  // Instituiuções
  lazy val amparo = elem(Palavra("amparo", false))
  lazy val cn = elem(Palavra("cn", false))
  lazy val conselho = elem(Palavra("conselho", false))
  lazy val deliberativo = elem(Palavra("deliberativo", false))
  lazy val estado = elem(Palavra("estado", false))
  lazy val fundo = elem(Palavra("fundo", false))
  lazy val municipio = elem(Palavra("municipio", false)) | elem(Palavra("município", false))
  lazy val trabalhador = elem(Palavra("trabalhador", false))

  // leis

  lazy val administrativo = elem(Palavra("administrativo", false))
  lazy val ato = elem(Palavra("ato", true))
  lazy val cf = elem(Palavra("cf", false))
  lazy val clt = elem(Palavra("clt", true))
  lazy val complementar = elem(Palavra("complementar", false))
  lazy val complementares = elem(Palavra("complementares", false))
  lazy val consolidacao = Palavra("consolidação", true) | Palavra("consolidacao", true)
  lazy val constitucional = elem(Palavra("constitucional", false))
  lazy val constitucionais = elem(Palavra("constitucionais", false))
  lazy val constituicao = Palavra("constituição", true) | Palavra("constituicao", true)
  lazy val decreto = elem(Palavra("decreto", true))
  lazy val decretos = elem(Palavra("decretos", true))
  lazy val delegada = elem(Palavra("delegada", false))
  lazy val delegadas = elem(Palavra("delegadas", false))
  lazy val disposicoes = Palavra("disposições", false) | Palavra("disposicoes", false)
  lazy val distrital = elem(Palavra("distrital", false))
  lazy val distritais = elem(Palavra("distritais", false))
  lazy val emenda = elem(Palavra("emenda", true))
  lazy val emendas = elem(Palavra("emendas", true))
  lazy val estadual = elem(Palavra("estadual", false))
  lazy val estaduais = elem(Palavra("estaduais", false))
  lazy val federal = elem(Palavra("federal", false))
  lazy val federais = elem(Palavra("federais", false))
  lazy val interno = elem(Palavra("interno", false))
  lazy val lei = elem(Palavra("lei", true))
  lazy val leis = elem(Palavra("leis", true))
  lazy val medida = elem(Palavra("medida", true))
  lazy val medidas = elem(Palavra("medidas", true))
  lazy val municipal = elem(Palavra("municipal", false))
  lazy val municipais = elem(Palavra("municipais", false))
  lazy val provisoria = Palavra("provisória", false) | Palavra("provisoria", false)
  lazy val provisorias = Palavra("provisórias", false) | Palavra("provisorias", false)
  lazy val regimento = elem(Palavra("regimento", true))
  lazy val regulamento = elem(Palavra("regulamento", true))
  lazy val resolucao = Palavra("resolução", true) | Palavra("resolucao", true)
  lazy val resolucoes = Palavra("resoluções", true) | Palavra("resolucoes", true)
  lazy val senado = elem(Palavra("senado", false))
  lazy val sumula = Palavra("súmula", true) | Palavra("sumula", true)
  lazy val trabalho = elem(Palavra("trabalho", false))
  lazy val transitoria = Palavra("transitória", false) | Palavra("transitoria", false)
  lazy val transitorias = Palavra("transitórias", false) | Palavra("transitorias", false)

  // componentes da norma

  lazy val artigo = Palavra("artigo", true) | Palavra("art", true) ~ opt(Ponto)
  lazy val artigos = Palavra("artigos", true) | Palavra("arts", true) ~ opt(Ponto)

  lazy val paragrafo =
    Palavra("parágrafo", true) |
      Palavra("paragrafo", true) |
      Palavra("par", true) ~ opt(Ponto) |
      Paragrafo

  lazy val caput = Palavra("caput", true) | Palavra("cpt", true) ~ opt(Ponto)

  lazy val paragrafos =
    Palavra("parágrafos", true) |
      Palavra("paragrafos", true) |
      Palavra("pars", true) ~ opt(Ponto) |
      Paragrafos

  lazy val inciso = Palavra("inciso", true) | Palavra("inc", true) ~ opt(Ponto)
  lazy val incisos = Palavra("incisos", true) | Palavra("incs", true) ~ opt(Ponto)

  lazy val alinea =
    Palavra("alinea", true) |
      Palavra("alínea", true) |
      Palavra("letra", true)

  lazy val alineas =
    Palavra("alineas", true) |
      Palavra("alíneas", true) |
      Palavra("letras", true)

  lazy val item = elem(Palavra("item", true))
  lazy val itens = elem(Palavra("itens", true))

}
package linker.parser

import linker.utils.Trie

trait TrieParser {

  self : BasicLinkerParser with Terminais with Combinadores =>

  import lexical.Hifen
  import lexical.Palavra

  def parseTrie[V](t : Trie[String, V]) : Parser[List[V]] = new Parser[List[V]] {
    def apply(in : Input) = p(t, in) match {
      case Success(v, in2)   => Success(v, in2)
      case Failure(msg, in3) => Failure(msg, in)
      case Error(msg, in4) => Error(msg, in)
    }

  }

  def p[V](t : Trie[String, V], in : Input) : ParseResult[List[V]] = {
    val palavraParser = palavra ^^ {
      case Palavra(p, _) => p
    } | Hifen ^^^ "-" | sempre ^^^ ""

    t match {
      case Trie(conteudo, filhos) =>
        val palavraEncontrada = palavraParser(in)        
        palavraEncontrada match {
          case Success(p1, in2) =>            
              filhos.get(p1).map(f => p(f, in2)).getOrElse(if(conteudo != Nil) Success(conteudo, in2) else Failure("não está na trie",in))            
          case Failure(msg, in3) => if(conteudo != Nil) Success(conteudo,in) else Failure("não está na trie", in)
          case Error(msg, in4) => Error(msg, in)
        }
    }

  }

}
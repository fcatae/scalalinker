package linker.utils

case class Trie[K,V](conteudo: List[V], filhos: Map[K,Trie[K,V]]) 

object Trie {
  def vazia[K,V] = Trie(Nil,Map.empty[K,Trie[K,V]])
  def monta[K,V](elements : List[(List[K],V)]) = monta1(vazia,elements)
  private def adiciona[K,V](trie : Trie[K,V],elemento: (List[K],V)) : Trie[K,V] = (trie,elemento) match {
    case (Trie(conteudo,filhos),(Nil,x)) => Trie(x :: conteudo, filhos)
    case (Trie(conteudo,filhos),(h :: t, x)) => 
      Trie(conteudo, filhos + (h -> adiciona(filhos.getOrElse(h,vazia),(t,x))))
  }
  private def monta1[K,V](inicial: Trie[K,V], elementos: List[(List[K],V)]) = 
    elementos.foldLeft(inicial)(adiciona)
  
}

object Utils {
  def limpa(c : Char) = c match {
    case 'á' => 'a'
    case 'à' => 'a'
    case 'ã' => 'a'
    case 'â' => 'a'
    case 'ç' => 'c'
    case 'é' => 'e'
    case 'ê' => 'e'
    case 'í' => 'i'
    case 'ó' => 'o'
    case 'ô' => 'o'
    case 'õ' => 'o'
    case 'ú' => 'u'
   
    case c => c
  }
    
}
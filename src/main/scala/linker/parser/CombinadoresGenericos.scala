package linker.parser

import scala.util.parsing.combinator.Parsers
import scala.annotation.tailrec

trait CombinadoresGenericos {

  self: Parsers =>

  def repNM[T](min: Int, max: Int, p: Parser[T]) =

    Parser {
      in =>

        @tailrec
        def iterador(numMatches: Int, listResults: List[T], input: Input): (List[T], Input) = {

          p(input) match {
            case Success(x, rest) => iterador(numMatches + 1, x +: listResults, rest)
            case _ => (listResults, input)
          }

        }

        val result = iterador(0, Nil, in)
        val intervalo = min to max

        if (intervalo contains result._1.length) {
          Success(result._1, result._2)
        } else {
          Failure("além do limite da repetição", result._2)
        }

    }
}
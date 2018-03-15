package linker.parser

import scala.util.parsing.combinator.syntactical.TokenParsers

import linker.lexical.LinkerLexical
import linker.lexical.LinkerTokens

trait BasicLinkerParser extends TokenParsers {

  type Tokens = LinkerTokens
  val lexical = LinkerLexical

}
package de.jendrik.martin

import scala.util.parsing.combinator.{RegexParsers, JavaTokenParsers}

import de.jendrik.martin.Token.Token
import net.arya.utf.Implicits.StringToUnicode

object Lexer extends RegexParsers {

	override protected val whiteSpace = " ".r

	def apply(code: String): Either[String, List[Token]] = {
		parse(tokens, code) match {
			case NoSuccess(msg, next) ⇒ Left(msg)
			case Success(res, _) ⇒
				//if(res.nonEmpty) res.head :: res.sliding(2).collect { case Seq(a, b) if a != b => b }.toList
				Right(res)
		}
	}

	def tokens: Parser[List[Token]] = phrase(rep(
		`module` | `class` | `extends` | `::` | `=` | `→` | `(` | `)` | `,` | comment | newline | tab | integerLiteral | floatLiteral | stringLiteral | identifier
	))

	def `module` = "module" ^^^ Token.`module`

	def `class` = "class" ^^^ Token.`class`

	def `extends` = "extends" ^^^ Token.`extends`

	def `::` = "::" ^^^ Token.`::`

	def `=` = "=" ^^^ Token.`=`

	def `→` = ("→" | "->") ^^^ Token.`→`

	def `(` = "(" ^^^ Token.`(`

	def `)` = ")" ^^^ Token.`)`

	def `,` = "," ^^^ Token.`,`

	def tab = "\t".r ^^^ Token.Tab

	def newline = """(\r?\n)""".r ^^^ Token.Newline

	def comment = ("#" ~ """[^(\r?\n)]+""".r) ^^^ Token.Comment

	def identifier = """\p{L}+""".r ^^ {case str ⇒ Token.Id(str)}

	def integerLiteral = """[+-]?[0-9]+""".r ^^ {case value ⇒ Token.IntLiteral(value.toLong)}

	def floatLiteral = """[+-]?[0-9]+[.,]([0-9]*|[0-9]+[eE][0-9]+)""".r ^^  {case value ⇒ Token.FloatLiteral(value.toDouble)}

	def stringLiteral: Parser[Token.StringLiteral] = (JavaTokens.stringLiteral ^^ { case str ⇒ Token.StringLiteral(str.toUtf32String)}).asInstanceOf[Lexer.Parser[Token.StringLiteral]]

	implicit def toDebugParser (name: String): {def !!![T] (p: Parser[T]): Parser[T]} = new {
		def !!![T] (p: Parser[T]) = log(p)(name) //for debugging
//		def !!![T](p:Parser[T]) = p              //for production
	}


	private object JavaTokens extends JavaTokenParsers
}

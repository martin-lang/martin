package de.jendrik.martin

import scala.annotation.tailrec
import scala.util.parsing.combinator.{JavaTokenParsers, RegexParsers}
import scala.util.parsing.input.CharSequenceReader

import de.jendrik.martin.Token.Token
import net.arya.utf.Implicits.StringToUnicode

object Lexer extends RegexParsers {

	override protected val whiteSpace = "[ ]+".r

	def apply(code: String): Either[String, List[Token]] = {
		parse(tokens, new CustomCharSeqReader(code)) match {
			case NoSuccess(msg, next) ⇒ Left(msg)
			case Success(res, _) ⇒
				val a: List[Token] = res filterNot (_ == Token.Comment)

				val b: List[Token] = a removeSubSeq(Token.Dedent, Token.Indent)
				println(a)
				println(b)
				Right(b)
		}
	}

	def tokens: Parser[List[Token]] = phrase(rep(
		`module` | `class` | `extends` | `var` | `:` | `.` | `=` | `→` | `(` | `)` | `[` | `]` | `{` | `}` | `,` | `§` | comment | integerLiteral | floatLiteral | stringLiteral | identifier | indentation | newline
	)) ^^ (raw ⇒ processIndentations(raw))

	private def processIndentations(tokens: List[Token], indents: List[Int] = List(0)): List[Token] = {
		tokens.headOption match {

			// if there is an increase in indentation level, we push this new level into the stack
			// and produce an INDENT
			case Some(Token.Tabs(n)) if n > indents.head =>
				Token.Indent :: processIndentations(tokens.tail, n :: indents)

			// if there is a decrease, we pop from the stack until we have matched the new level,
			// producing a DEDENT for each pop
			case Some(Token.Tabs(n)) if n < indents.head =>
				val (dropped, kept) = indents.partition(_ > n)
				(dropped map (_ => Token.Dedent)) ::: processIndentations(tokens.tail, kept)

			// if the indentation level stays unchanged, no tokens are produced
			case Some(Token.Tabs(n)) if n == indents.head =>
				processIndentations(tokens.tail, indents)

			// other tokens are ignored
			case Some(token) =>
				token :: processIndentations(tokens.tail, indents)

			// the final step is to produce a DEDENT for each indentation level still remaining, thus
			// "closing" the remaining open INDENTS
			case None =>
				indents.filter(_ > 0).map(_ => Token.Dedent)

		}
	}

	def `module` = "module" ^^^ Token.`module`

	def `class` = "class" ^^^ Token.`class`

	def `extends` = "extends" ^^^ Token.`extends`

	//def `io` = "io" ^^^ Token.`io`

	def `var` = "var" ^^^ Token.`var`

	def `:` = ":" ^^^ Token.`:`

	def `.` = "." ^^^ Token.`.`

	def `?.` = "?." ^^^ Token.`?.`

	def `=` = "=" ^^^ Token.`=`

	def `→` = ("→" | "->") ^^^ Token.`→`

	def `(` = "(" ^^^ Token.`(`

	def `)` = ")" ^^^ Token.`)`

	def `[` = "[" ^^^ Token.`[`

	def `]` = "]" ^^^ Token.`]`

	def `{` = "{" ^^^ Token.`{`

	def `}` = "}" ^^^ Token.`}`

	def `,` = "," ^^^ Token.`,`

	def `§` = "§" ^^^ Token.`§`

	//def tab = "\t".r ^^^ Token.Tab

	def indentation: Parser[Token.Tabs] = "indentation" !!! {
		"""\r?\n[\t]*""".r ^^ { whitespace =>
			val nTabs = whitespace.length - 1
			Token.Tabs(nTabs)
		}
	}

	def newline = """(\r?\n)+""".r ^^^ Token.Newline

	def comment = "comment" !!! (("#" ~ """([^\n])*""".r) ^^^ Token.Comment)

	def identifier = """\p{L}+(\p{L}|\p{Digit})*""".r ^^ {case str ⇒ Token.Id(str)}

	def integerLiteral = """[+-]?[0-9]+""".r ^^ {case value ⇒ Token.IntLiteral(value.toLong)}

	def floatLiteral = """[+-]?[0-9]+[.,]([0-9]*|[0-9]+[eE][0-9]+)""".r ^^  {case value ⇒ Token.FloatLiteral(value.toDouble)}

	def stringLiteral: Parser[Token.StringLiteral] = (JavaTokens.stringLiteral ^^ { case str ⇒ Token.StringLiteral(str.toUtf32String)}).asInstanceOf[Lexer.Parser[Token.StringLiteral]]

	def operator = """(\+|\-|\*|/|^|&|\||<|<=|>|>=)""".r ^^ {str ⇒ Token.Operator(str)}

	implicit def toDebugParser (name: String): {def !!![T] (p: Parser[T]): Parser[T]} = new {
//		def !!![T] (p: Parser[T]) = log(p)(name) //for debugging
		def !!![T](p:Parser[T]) = p              //for production
	}


	private object JavaTokens extends JavaTokenParsers

	private class CustomCharSeqReader(source: CharSequence, offset: Int = 0) extends CharSequenceReader(source, offset) {

		override def rest =
			if (offset < source.length) new CustomCharSeqReader(source, offset + 1)
			else this


		override def drop(n: Int) = new CustomCharSeqReader(source, offset + n)

		override def toString: String = {
			val next = if (atEnd) "" else "'first', ..."
			s"CharSequenceReader($next)"
		}
	}


	implicit class ListRemoveSubSequenceOps[A](val self: List[A]) {
		@tailrec final def removeSubSeq (sublist: A*): List[A] = {
			self.indexOfSlice(sublist) match {
				case -1 => self
				case index => self.patch(index, Nil, sublist.length).removeSubSeq(sublist: _*)
			}
		}

	}
}

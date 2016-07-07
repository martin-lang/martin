package de.jendrik.martin

import better.files._

object TestMain {

	def main (args: Array[String]) {
		1 === 1
		("": Any) === 1

		Lexer(args.headOption.getOrElse(file"./test/de/jendrik/martin/test.martin".lines.mkString("\n"))) match {
			case Left(msg) ⇒ println(msg)
			case Right(tokens) ⇒
				println(tokens mkString "   ")
				Parser(tokens) match {
					case Left(msg) ⇒ println(msg)
					case Right(parsed) ⇒
						println("PARSED SUCCESSFULLY")
						println(parsed.contents mkString "\n")
						println("JAVA CODE:\n")
						println(JavaCodeGenerator(parsed, file"./out/martin").mkString("\n\n"))
				}
		}
	}


	implicit class TypesafeEquals[A](val self: A) extends AnyVal {
		def ===(that: A) = self == that
	}
}

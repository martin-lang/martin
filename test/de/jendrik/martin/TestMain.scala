package de.jendrik.martin

import better.files._

object TestMain {

	def main (args: Array[String]) {
		Lexer(args.headOption.getOrElse(file"./test/de/jendrik/martin/test.martin".lines.mkString("\n"))) match {
			case Left(msg) ⇒ println(msg)
			case Right(tokens) ⇒ println(tokens mkString " ")
		}
	}

}

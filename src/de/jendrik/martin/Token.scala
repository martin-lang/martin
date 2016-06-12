package de.jendrik.martin

import net.arya.utf.Utf32String

object Token {

	trait Token

	case object `module`                          extends Token
	case object `class`                           extends Token
	case object `extends`                         extends Token
	case object `::`                              extends Token
	case object `=`                               extends Token
	case object `→`                               extends Token
	case object `(`                               extends Token
	case object `)`                               extends Token
	case object `,`                               extends Token
	case object Comment                           extends Token
	case object Newline                           extends Token
	case object Tab                               extends Token
	case class  Id(name: String)                  extends Token
	case class  IntLiteral(value: Long)           extends Token
	case class  FloatLiteral(value: Double)       extends Token
	case class  StringLiteral(value: Utf32String) extends Token

}

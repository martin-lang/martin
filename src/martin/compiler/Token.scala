package martin.compiler

/*
	MARTIN PROGRAMMING LANGUAGE
	martin-lang.pro

	Copyright (C) 2016 Jendrik Wenke

	This program is free software: you can redistribute it and/or modify
	it under the terms of the GNU Affero General Public License as published
	by the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU Affero General Public License for more details.

	You should have received a copy of the GNU Affero General Public License
	along with this program.  If not, see <http://www.gnu.org/licenses/>. */

object Token {

	sealed trait Token

	case object `module`                          extends Token
	case object `class`                           extends Token
	case object `extends`                         extends Token
	case object `var`                             extends Token
	case object `val`                             extends Token
	case object `:`                               extends Token
	case object `.`                               extends Token
	case object `=`                               extends Token
	case object `→`                               extends Token
	case object `(`                               extends Token
	case object `)`                               extends Token
	case object `[`                               extends Token
	case object `]`                               extends Token
	case object `{`                               extends Token
	case object `}`                               extends Token
	case object `,`                               extends Token
	case object `§`                               extends Token
	case object `?.`                              extends Token
	case object Comment                           extends Token
	case object Newline                           extends Token
	case object Indent                            extends Token
	case object Dedent                            extends Token
	case class  Tabs(nTabs: Int)                  extends Token
	case class  Operator(op: String)              extends Token
	case class  Id(name: String)                  extends Token
	case class  IntLiteral(value: Long)           extends Token
	case class  FloatLiteral(value: Double)       extends Token

	case class  StringLiteral(value: String) extends Token

}

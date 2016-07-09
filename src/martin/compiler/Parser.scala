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

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

import martin.compiler.AST.{AST, ClassDef, Expr, FloatLiteral, Id, IntLiteral, MartinFile, MemberCall, MethodCall, MethodDef, ModuleDef, Newline, StringLiteral, Type, TypedValue, VarDef}
import martin.compiler.Token.{Dedent, Indent, Operator, Token, `(`, `)`, `,`, `:`, `=`, `[`, `]`, `class`, `extends`, `module`, `val`, `var`, `§`}


object Parser extends Parsers {

	type Elem = Token

	override type Input = Reader[Token]

	def apply (tokens: List[Token]): Either[String, MartinFile] = {
		file(new TokenReader(tokens)) match {
			case NoSuccess(msg, _) ⇒ Left(msg)
			case Success(res, _)   ⇒ Right(res)
		}
	}

	def file = phrase(
		opt(newline) ~
		opt(moduleDef) ~
		rep(
			newline | classDef | methodDef
		)
	) ^^ { case _ ~ moduleDef ~ others ⇒ MartinFile(moduleDef.toSeq ++: others: _*) }

	def moduleDef = "moduleDef" !!! (`module` ~ id ^^ { case _ ~ name ⇒ ModuleDef(name) })

	def classDef = "classDef" !!! {
		(tags ~
		 (`class` ~> id) ~
		 opt(args) ~
		 opt(`extends` ~> id ~ opt(open ~> repsep(id, `,`) <~ closing))) ~
		 opt(Indent ~> classContents <~ Dedent) ^^ {
			case tags ~ name ~ Some((x: List[_])) ~ Some(parent ~ Some(List(args))) ~ contents ⇒ ClassDef(tags, name, x, Some(parent), List(args), contents.getOrElse(Nil): _*)
			case tags ~ name ~ Some((x: List[_])) ~ Some(parent ~ None) ~ contents             ⇒ ClassDef(tags, name, x, Some(parent), List(), contents.getOrElse(Nil): _*)
			case tags ~ name ~ Some((x: List[_])) ~ None ~ contents                            ⇒ ClassDef(tags, name, x, None, List(), contents.getOrElse(Nil): _*)
			case tags ~ name ~ None ~ Some(parent ~ Some(List(args))) ~ contents               ⇒ ClassDef(tags, name, List(), Some(parent), List(args), contents.getOrElse(Nil): _*)
			case tags ~ name ~ None ~ Some(parent ~ None) ~ contents                           ⇒ ClassDef(tags, name, List(), Some(parent), List(), contents.getOrElse(Nil): _*)
			case tags ~ name ~ None ~ None ~ contents                                          ⇒ ClassDef(tags, name, List(), None, List(), contents.getOrElse(Nil): _*)

		}
	}

	def classContents: Parser[List[AST]] = "classContents" !!! rep(classDef | methodDef | varDef)

	def methodDef = "methodDef" !!! (rep(accept(`§`) ~> id) ~ id ~ opt(args) ~ Token.`:` ~ tpe ~ Token.`=` ~ (Indent ~> methodContents <~ Dedent | methodContent) ^^ {
		case (tags: List[_]) ~ name ~ Some(args: List[_]) ~ _ ~ retType  ~ _ ~ (contents: List[AST]) ⇒ MethodDef(tags, name, args, retType, contents)
		case (tags: List[_]) ~ name ~ Some(args: List[_]) ~ _ ~ retType ~ _ ~ (content: AST)        ⇒ MethodDef(tags, name, args, retType, List(content))
		case (tags: List[_]) ~ name ~ None ~ _ ~ retType ~ _ ~ (contents: List[AST])                ⇒ MethodDef(tags, name, List(), retType, contents)
		case (tags: List[_]) ~ name ~ None ~ _ ~ retType ~ _ ~ (content: AST)                       ⇒ MethodDef(tags, name, List(), retType, List(content))
	})

	def args: Parser[List[TypedValue]] = "args" !!! ((open ~ repsep(typedValue, `,`) ~ closing) ^^ { case _ ~ xs ~ _ ⇒ xs })

	def methodContents = rep1(methodContent)

	def methodContent: Parser[AST] = "methodContent" !!! (varDef | methodCall | memberCall | numericExpr)

	def varDef = "varDef" !!! (((`var` ~> id) ~ (`:` ~> tpe) ~ (`=` ~> expr)) ^^ { case name ~ tpe ~ expr ⇒ VarDef(false, name, tpe, expr) })

	def valDef = "valDef" !!! (((`val` ~> id) ~ (`:` ~> tpe) ~ (`=` ~> expr)) ^^ { case name ~ tpe ~ expr ⇒ VarDef(true, name, tpe, expr) })

	def methodCall: Parser[MethodCall] = "methodCall" !!! (id ~ (open ~> repsep(expr, accept(`,`)) <~ closing) ^^ {
		case name ~ (args: List[_]) ⇒ MethodCall(name, args: _*)
	})

	def numericExpr: Parser[Expr] = "numericExpr" !!! chainl1(multDivModExpr,
	                                                          Operator("+") ^^^ ((x: Expr, y: Expr) ⇒ MemberCall(true, x, MethodCall(Id("+"), y)))
	                                                          | Operator( "-") ^^^ ((x: Expr, y: Expr) ⇒ MemberCall(true, x, MethodCall(Id("-"), y))))

	def multDivModExpr: Parser[Expr] = "multDivModExpr" !!! chainl1(powExpr,
	                                                                Operator("*") ^^^ ((x: Expr, y: Expr) ⇒ MemberCall(true, x, MethodCall(Id("*"), y)))
	                                                                | Operator("/") ^^^ ((x: Expr, y: Expr) ⇒ MemberCall(true, x, MethodCall(Id("/"), y)))
	                                                                | Operator("%") ^^^ ((x: Expr, y: Expr) ⇒ MemberCall(true, x, MethodCall(Id("%"), y))))

	def powExpr: Parser[Expr] = "powExpr" !!! chainl1[Expr](factor, Operator("^") ^^^ ((x: Expr, y: Expr) ⇒ MemberCall(true, x, MethodCall(Id("^"), y))))

	def factor: Parser[Expr] = "factor" !!! (opt(Operator("-") | Operator("−") | Operator("+")) ~ expr ^^ {
		case Some(Operator("-") | Operator("−")) ~ num ⇒ MemberCall(true, IntLiteral(0), MethodCall(Id("-"), num))
		case _ ~ num                                   ⇒ num
	} | (open ~> numericExpr <~ closing))

	def expr: Parser[Expr] = "expr" !!! (methodCall | memberCall | id | intLiteral | floatLiteral | stringLiteral)

	//noinspection ScalaUnnecessaryParentheses
	def tags: Parser[List[Id]] = "tags" !!! (rep(accept(`§`) ~> id))

	def intLiteral: Parser[IntLiteral] = accept("intLiteral", { case Token.IntLiteral(value) ⇒ IntLiteral(value) })

	def floatLiteral: Parser[FloatLiteral] = accept("floatLiteral", { case Token.FloatLiteral(value) ⇒ FloatLiteral(value) })

	def stringLiteral: Parser[StringLiteral] = accept("stringLiteral", { case Token.StringLiteral(value) ⇒ StringLiteral(value) })

	def memberCall: Parser[MemberCall] = "memberCall" !!! (id ~ (accept(Token.`.`) | accept(Token.`?.`)) ~ (memberCall | methodCall) ^^ {
		case obj ~ Token.`.`  ~ (method: MethodCall) ⇒ MemberCall(isSafe = false, obj, method)
		case obj ~ Token.`?.`  ~ (method: MethodCall) ⇒ MemberCall(isSafe = true, obj, method)
		case obj ~ Token.`.` ~ (member: MemberCall) ⇒ MemberCall(isSafe = false,  obj, member)
		case obj ~ Token.`?.` ~ (member: MemberCall) ⇒ MemberCall(isSafe = true,  obj, member)
	})


	def newline = accept(Token.Newline) ^^^ Newline

	def open = accept(`(`)

	def closing = accept(`)`)

	def id: Parser[Id] = accept("identifier", { case id: Token.Id ⇒ AST.Id(id.name) })

	def optionallyTypedValue: Parser[Either[Id, TypedValue]] = "optionallyTypedValue" !!! (typedValue ^^ { case tv ⇒ Right(tv) } | id ^^ { case id ⇒ Left(id) })

	def typedValue = "typedValue" !!! ((id ~ accept(Token.`:`) ~ tpe) ^^ { case id ~ _ ~ tpe ⇒ TypedValue(id, tpe) })

	def tpe: Parser[Type] = (id ~ opt(`[` ~> tpe <~ `]`)) ^^ {
		case name ~ Some(args) ⇒ Type(name, args)
		case name ~ None       ⇒ Type(name)
	}

	implicit def toDebugParser (name: String): {def !!![T] (p: Parser[T]): Parser[T]} = new {
		def !!![T] (p: Parser[T]) = log(p)(name) //for debugging
//		def !!![T](p:Parser[T]) = p              //for production
	}


	class TokenReader (tokens: Seq[Token]) extends Reader[Token] {

		override def pos: Position = NoPosition

		override def rest: Reader[Token] = new TokenReader(tokens.tail)

		override def toString: String = {
			val next: String = if (atEnd) "" else s"$first, ..."
			s"TokenReader($next)"
		}

		override def first: Token = tokens.head

		override def atEnd: Boolean = tokens.isEmpty

	}

}

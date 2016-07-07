package de.jendrik.martin

import net.arya.utf.Utf32String

object AST {

	sealed trait AST
	sealed trait Expr extends AST
	sealed trait Taggable {
		this: AST ⇒
		def tags: List[Id]
	}
	sealed trait CanBeMember extends AST {
		this: AST ⇒
		//def name: String
	}



	case object This extends Expr
	case class IntLiteral(value: Long) extends Expr
	case class FloatLiteral(value: Double) extends Expr
	case class StringLiteral(value: Utf32String) extends Expr
	case class Id (name: String) extends Expr with CanBeMember
	case class MethodCall(methodName: Id, args: Expr*) extends Expr with CanBeMember {
		//override def name: String = methodName.name
	}
	case class MemberCall(isSafe: Boolean, obj: Expr, member: CanBeMember) extends Expr with CanBeMember {
		//override def name: String = obj.toString +  "." + member.name
	}

	case object Newline extends AST
	case class Type(name: Id, typeargs: Type*) extends AST
	case class MartinFile (contents: AST*) extends AST
	case class VarDef (name: Id, tpe: Type, value: Expr) extends AST
	case class ModuleDef(name: Id) extends AST
	case class TypedValue(id: Id, tpe: Type) extends AST
	case class ClassDef(tags: List[Id], name: Id, params: Seq[TypedValue], parent: Option[Id], parentArgs: Seq[Expr], contents: AST*) extends AST with Taggable
	case class MethodDef(tags: List[Id], name: Id, args: Seq[TypedValue], retType: Type, contents: Seq[AST]) extends AST with Taggable //no vararg because copy is needed

}

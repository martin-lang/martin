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



import java.net.URI
import javax.tools.JavaFileObject.Kind
import javax.tools.{SimpleJavaFileObject, ToolProvider}

import scala.collection.JavaConverters._
import scala.collection.mutable

import better.files.File
import AST.{AST, ClassDef, FloatLiteral, Id, IntLiteral, MartinFile, MemberCall, MethodCall, MethodDef, ModuleDef, StringLiteral, Taggable, This, Type, VarDef}
import martin.compiler.AST.{AST, ClassDef, MartinFile, MemberCall, MethodDef, Taggable, This, Type, VarDef}

object JavaCodeGenerator {

	val javaModifierTags = Vector("private", "protected", "public", "static", "final", "abstract").map(Id(_))

	def apply (code: MartinFile, outputDir: File): Seq[(String, String)] = {
		var module: Option[String] = None

		def generateClass (c: ClassDef): String = {
			val s = new StringBuilder
			c.tags.diff(javaModifierTags).foreach(t ⇒ s.insert(0, "@" + t.name + " "))
			s ++= javaModifiers(c)
			s ++= " class "
			s ++= c.name.name + " "
			c.parent match {
				case Some(Id(parent)) ⇒ s ++= "extends " + parent
				case None             ⇒ ()
			}
			s ++= "{\n"
			if (c.params.nonEmpty || c.parentArgs.nonEmpty) {
				s ++= "public " + c.name.name
				s ++= c.params.map(p ⇒ generateType(p.tpe) + " " + p.id.name).mkString("(", ", ", ")")
				s ++= "{\n"
				s ++= "super" + c.parentArgs.mkString("(", ",", ")") + ";"
				s ++= "}\n"
			}
			c.contents.map(generate).foreach(s ++= _)
			s ++= "}\n"
			s.toString
		}

		def generateMethod (m: MethodDef): String = {
			val s = new mutable.StringBuilder()
			m.tags.diff(javaModifierTags).foreach(t ⇒ s ++= "@" + t.name + " ")
			s ++= javaModifiers(m)
			s ++= " "
			s ++= generateType(m.retType)
			s ++= " "
			s ++= m.name.name
			s ++= m.args.map(tv ⇒ generateType(tv.tpe) + " " + tv.id.name).mkString("(", ",", ")")
			s ++= "{\n"
			m.contents.dropRight(1).map(generate).foreach(s ++= _ + ";\n")
			if(m.retType != Type(Id("Void"))) s ++= "return "
			s ++= generate(m.contents.last)
			s ++= ";}\n"
			s.toString
		}

		def generateType (t: Type, inGenerics: Boolean = false): String = {
			val s = new mutable.StringBuilder()
			val Id(name) = t.name
			if (name == "Array") {
				s ++= t.typeargs.map(t ⇒ generateType(t, inGenerics = false)).head + "[]"
			} else {
				s ++= ((name, inGenerics) match {
					case ("Int", true)    ⇒ "Long"
					case ("Int", false)   ⇒ "long"
					case ("Float", true)  ⇒ "Double"
					case ("Float", false) ⇒ "double"
					case ("Void", true)   ⇒ "Void"
					case ("Void", false)  ⇒ "void"
					case (x, _)           ⇒ x
				})
				if (t.typeargs.nonEmpty)
					s ++= t.typeargs.map(t ⇒ generateType(t, inGenerics = true)).mkString("<", ",", ">")
			}
			s.toString
		}

		def generateVarDef (v: VarDef): String = {
			val s = new mutable.StringBuilder()
			s ++= generateType(v.tpe)
			s ++= " "
			s ++= v.name.name
			s ++= " = "
			s ++= generate(v.value)
			s ++= ";"
			s.toString()
		}

		def generateMemberCall (mc: MemberCall): String = {
			mc.member match {
				case method @ MethodCall(Id("new"), args @ _*) ⇒ "new " + generate(mc.obj) + method.args.map(generate).mkString("(", ", ", ")")
				case _ ⇒
					if (mc.isSafe) {
						val obj = generate(mc.obj)
						s"($obj == null) ? null : $obj." + generate(mc.member)
					} else generate(mc.obj) + "." + generate(mc.member)
			}
		}

		def generate (code: AST): String = code match {
			case ModuleDef(Id(name)) ⇒ module = Some("package " + name + ";\n"); ""
			case c: ClassDef         ⇒ generateClass(c)
			case m: MethodDef        ⇒ generateMethod(m)
			case v: VarDef           ⇒ generateVarDef(v)

			case This                        ⇒ "this"
			case IntLiteral(v)               ⇒ v.toString
			case FloatLiteral(v)             ⇒ v.toString
			case StringLiteral(s)            ⇒ StringContext.treatEscapes(s)
			case Id(name)                    ⇒ name
			case MethodCall(name, args @ _*) ⇒ name.name.toString + args.map(generate).mkString("(", ",", ")")
			case mc: MemberCall              ⇒ generateMemberCall(mc)
		}

		val topLevelMethods: Seq[MethodDef] = code.contents.collect { case m: MethodDef ⇒ m }
		val classes: Seq[String] = (code.contents diff topLevelMethods map generate) :+
		                           generate(ClassDef(Nil, Id("Main"), Nil, None, Nil, topLevelMethods map { (x: MethodDef) ⇒ x.copy(tags = Id("static") +: x.tags) }: _*))

		val packagedClasses: Seq[(String, String)] = (classes
		                       filterNot (_ == "")
		                       map (s ⇒ (s, s.indexOf("public class") |> { i ⇒ s.substring(i + 13, s.indexOf(" ", i + 13)) }))
		                       map {case (c, n) ⇒ ((module getOrElse "") + c, n)}
		                       map {case (c, className) ⇒ (c, if (module.isDefined) c.substring(c.indexOf("package") + 8, c.indexOf(";")) + "/" + className else className)})

		packagedClasses foreach { case (c, className) ⇒ (outputDir / "tmp").createChild(className + ".java").write(c) }

		(outputDir / "classes").createIfNotExists(asDirectory = true)
		println((outputDir / "classes").toJava.exists())
		ToolProvider.getSystemJavaCompiler.
			getTask(null, null, null, List("-d", (outputDir / "classes").pathAsString).asJava, null, packagedClasses.map({case (c, className) ⇒ new JavaSourceFromFile((outputDir / (className + ".java")).path.toUri, c) }).asJava).call()
		packagedClasses
	}

	def javaModifiers (x: Taggable): String = {
		val tags = Id("public") :: x.tags
		javaModifierTags.filter(tags.contains(_)).map(_.name).mkString(" ")
	}


	implicit class SeqRemoveOps[A] (val self: Seq[A]) {

		def remove (xs: A*) = self diff xs
	}


	implicit class PipedObject[A] (value: A) {

		def |>[B] (f: A => B): B = f(value)
	}

	class JavaSourceFromFile (name: URI, code: String) extends SimpleJavaFileObject(name, Kind.SOURCE) {
		override def getCharContent (ignoreEncodingErrors: Boolean) = code
	}

}
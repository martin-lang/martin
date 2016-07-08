#The Martin Programming Language

The Martin Programming Language, named after Martin Odersky, the creator of scala,
is a general purpose object-oriented programming language I initially created just for fun.

##Features

###Consistent Types

All predefined types are capitalized.
Martin only supports this set of Java's primitives:

| Java | Martin
| ---  | ---
| `long` | `Int`
| `double` | `Float`
| `boolean`  | `Bool`
| `void` | `Void`

###Ranges

In Future releases of Martin, variables and parameters may restrict the range of an `Int` or `Float`. This will be proven at compile time if possible, and will generate checks to throw an exception at runtime

###Union and Intersection Types
In the future, Martin  will support union and intersecetion types.

###Tags

In Java, `@Override` is an annotation, while it's a keyword in Scala and C++; `@inline` is an annotation in Scala and a keyword in C++.
Martin features a unification of annotations and modifiers called tags because there's no real difference between them.
Tags are prepended with the `§` symbol (`U+00A7 SECTION SIGN`). The tags `private protected public final abstract` are compiled to Java modifiers.

###Top-level methods

You can declare methods without an enclosing class. These methods are compiled as static methods of the class `Main`.

###Simple variable definitions

Variables are declared like this: `var i: Int = 42`. To make a variable constant (`final` in Java) use `val` instead of `var`.

###Simple constructors

Writing `class Person(val name: String, val age: Int)` will generate the following Java code:
```
public class Person {
	final String name;
	final int age;
	public Person(String name, int age) {
		this.name = name;
		this.age = age;
	}
}
```

###Multiple Inheritance
Martin will support multiple inheritance in the future, avoiding the diamond problem by forcing the subclass to override methods inherited from more than one class. The overridden method may delegate to a parent method

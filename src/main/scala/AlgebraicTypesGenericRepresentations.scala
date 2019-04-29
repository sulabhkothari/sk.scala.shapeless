object AlgebraicTypesGenericRepresentations {

  sealed trait Shape

  final case class Rectangle(width: Double, height: Double) extends
    Shape

  final case class Circle(radius: Double) extends Shape

  def area(shape: Shape): Double =
    shape match {
      case Rectangle(w, h) => w * h
      case Circle(r) => math.Pi * r * r
    }

  type Rectangle2 = (Double, Double)
  type Circle2 = Double
  type Shape2 = Either[Rectangle2, Circle2]

  def area2(shape: Shape2): Double =
    shape match {
      case Left((w, h)) => w * h
      case Right(r) => math.Pi * r * r
    }

  def main(args: Array[String]): Unit = {
    val rect: Shape = Rectangle(3.0, 4.0)
    val circ: Shape = Circle(1.0)
    println(area(rect))
    println(area(circ))
    val rect2: Shape2 = Left((3.0, 4.0))
    val circ2: Shape2 = Right(1.0)
    println(area2(rect2))
    println(area2(circ2))
  }
}

object HListsUsage {
  def main(args: Array[String]): Unit = {
    import shapeless.{HList, ::, HNil}
    val product: String :: Int :: Boolean :: HNil =
      "Sunday" :: 1 :: false :: HNil
    val first = product.head
    println(first)
    val second = product.tail.head
    println(second)
    val rest = product.tail.tail
    println(rest)

    //compilation error
    //product.tail.tail.tail.head

    val newProduct: Long :: String :: Int :: Boolean :: HNil = 42L :: product
    println(newProduct)

    // The behaviour we get from HLists isn’t magic. We could have achieved all of this functionality using (A, B) and
    // Unit as alternatives to :: and HNil. However, there is an advantage in keeping our representation types separate
    // from the semantic types used in our applications. HList provides this separation.


    // Shapeless provides a type class called Generic that allows us to switch back and forth between a concrete ADT
    // and its generic representation. Some behind-the-scenes macro magic allows us to summon instances of Generic
    //without boilerplate:
    import shapeless.Generic
    case class IceCream(name: String, numCherries: Int, inCone: Boolean)
    val iceCreamGen = Generic[IceCream]
    println(iceCreamGen)

    val iceCream = IceCream("Sundae", 1, false)
    println(iceCream)
    val repr = iceCreamGen.to(iceCream)
    println(repr)
    val iceCream2 = iceCreamGen.from(repr)
    println(iceCream2)

    case class Employee(name: String, number: Int, manager: Boolean)
    // Create an employee from an ice cream:
    val employee = Generic[Employee].from(Generic[IceCream].to(iceCream))
    println(employee)

    // Other product types
    //It’s worth no􏰀ng that Scala tuples are actually case classes, so Generic works with them just fine:
    val tupleGen = Generic[(String, Int, Boolean)]
    println(tupleGen.to(("Hello", 123, true)))
  }
}

object GenericCoproducts {

  import shapeless.{Coproduct, :+:, CNil, Inl, Inr}

  case class Red()

  case class Amber()

  case class Green()

  type Light = Red :+: Amber :+: Green :+: CNil

  // In general coproducts take the form A :+: B :+: C :+: CNil meaning “A or B or C”, where :+: can be loosely
  // interpreted as Either. The overall type of a coproduct encodes all the possible types in the disjunction, but each
  // concrete instance contains a value for just one of the possibilities. :+: has two subtypes, Inl and Inr, that
  // correspond loosely to Left and Right. We create instances of a coproduct by nesting Inl and Inr constructors:
  def main(args: Array[String]): Unit = {
    val red: Light = Inl(Red())
    val green: Light = Inr(Inr(Inl(Green())))
    println(red)
    println(green)

    // Every coproduct type is terminated with CNil, which is an empty type with no values, similar to Nothing.
    // We can’t instantiate CNil or build a Coproduct purely from instances of Inr. We always have exactly one Inl in a value.

    switchingEncodingsUsingGeneric
  }

  def switchingEncodingsUsingGeneric = {
    // Coproduct types are difficult to parse on first glance. However, we can see how they fit into the larger picture
    // of generic encodings. In addition to understanding case classes and case objects, shapeless’ Generic type class
    // also understands sealed traits and abstract classes:
    import shapeless.Generic
    sealed trait Shape
    final case class Rectangle(width: Double, height: Double) extends
      Shape
    final case class Circle(radius: Double) extends Shape
    val gen = Generic[Shape]
    println(gen.to(Rectangle(3.0, 4.0)))
    println(gen.to(Circle(1.0)))
  }
}
import shapeless.syntax.SingletonOps
import shapeless.tag

object AccessingNamesDuringImplicitDerivation {

  def typeTaggingAndPhantomTypes = {

    // Phantom types are types with no run-time seman􏰀cs, like this:
    trait Cherries

    val number = 42
    val numCherries = number.asInstanceOf[Int with Cherries]

    println(numCherries)

    // Shapeless uses this trick to tag fields and subtypes in an ADT with the singleton types of their names. If you
    // find using asInstanceOf uncomfortable then don’t worry: shapeless provides two tagging syntaxes to avoid such
    // unsavoriness.
    //The first syntax, ->>, tags the expression on the right of the arrow with the singleton type of the literal
    // expression on the left􏰁:
    import shapeless.labelled.{KeyTag, FieldType}
    import shapeless.syntax.singleton._
    val someNumber = 123
    val numCherries2 = "numCherries" ->> someNumber
    // numCherries: Int with shapeless.labelled.KeyTag[String("numCherries"),Int] = 123
    println(numCherries2)

    // The tag encodes both the name and type of the field, the combination of which is useful when searching for entries
    // in a Repr using implicit resolution.
    // The second syntax takes the tag as a type rather than a literal value. This is useful when we know what tag to
    // use but don’t have the ability to write specific literals in our code:
    import shapeless.labelled.field
    println(field[Cherries](123))
    // res11: shapeless.labelled.FieldType[Cherries,Int] = 123

    // FieldType is a type alias that simplifies extracting the tag and base types from a tagged type:
    // type FieldType[K, V] = V with KeyTag[K, V]

    // Tags exist purely at compile time and have no run􏰀me representa􏰀on. How do we convert them to values we can use
    // at runtime? Shapeless provides a type class called Witness for this purpose. If we combine Witness and FieldType,
    // we get something very compelling—the ability to extract the field name from a tagged field:
    import shapeless.Witness
    val numCherries3 = "numCherriesType" ->> 123

    // numCherries: Int with shapeless.labelled.KeyTag[String("numCherries"),Int] = 123
    // Get the tag from a tagged value:
    def getFieldName[K, V](v: FieldType[K, V])
                          (implicit witness: Witness.Aux[K]): K =
      witness.value

    println("Get the tag from a tagged value --> " + getFieldName(numCherries3))

    // If we build an HList of tagged elements, we get a data structure that has some of the properties of a Map. We can
    // reference fields by tag, manipulate and replace them, and maintain all of the type and naming information along
    // the way. Shapeless calls these structures “records”.

    def recordsAndLabelledGeneric = {
      import shapeless.labelled.{KeyTag, FieldType}
      import shapeless.syntax.singleton._
      import shapeless.{HList, ::, HNil}
      val garfield = ("cat" ->> "Garfield") :: ("orange" ->> true) :: HNil
      println("HList of tagged elements --> " + garfield)

      // LabelledGeneric tags each item in a product or coproduct with the corresponding field or type name from the
      // concrete ADT (although the names are represented as Symbols, not Strings). Shapeless provides a suite of Map-like
      // operations on records
    }

    recordsAndLabelledGeneric
  }

  def derivingProductInstancesWithLabelledGeneric = {
    // We’ll define a JsonEncoder type class that converts values to a JSON AST. This is the approach taken by Argonaut,
    // Circe, Play JSON, Spray JSON, and many other Scala JSON libraries.
    import JsonEncoderTypes._
    val iceCream = IceCream("Sundae", 1, false)

    // Ideally we'd like to produce something like this:
    val iceCreamJson: JsonValue =
      JsonObject(List(
        "name" -> JsonString("Sundae"),
        "numCherries" -> JsonNumber(1),
        "inCone" -> JsonBoolean(false)
      ))

    import shapeless.LabelledGeneric
    val gen = LabelledGeneric[IceCream].to(iceCream)
    // String with KeyTag[Symbol with Tagged["name"], String] ::
    // Int with KeyTag[Symbol with Tagged["numCherries"], Int] ::
    // Boolean with KeyTag[Symbol with Tagged["inCone"], Boolean] ::
    // HNil

    // Instead of representing the field names with literal string types, shapeless is representing them with symbols
    // tagged with literal string types. The details of the implementation aren’t particularly important: we can still
    // use Witness and FieldType to extract the tags, but they come out as Symbols instead of Strings

    println("LabelledGeneric --> " + gen)
  }

  def instancesForHLists = {
    import JsonEncoderTypes._
    trait JsonObjectEncoder[A] extends JsonEncoder[A] {
      def encode(value: A): JsonObject
    }

    def createObjectEncoder[A](fn: A => JsonObject): JsonObjectEncoder[A] =
      new JsonObjectEncoder[A] {
        def encode(value: A): JsonObject =
          fn(value)
      }

    // Let’s define JsonEncoder instances for HNil and ::. Our encoders are going to generate and manipulate JsonObjects,
    // so we’ll introduce a new type of encoder to make that easier:
    import shapeless.{HList, ::, HNil, Lazy}

    implicit val hnilEncoder: JsonObjectEncoder[HNil] = createObjectEncoder(hnil => JsonObject(Nil))

    // LabelledGeneric will give us an HList of tagged types (FieldType alias of V with KeyTag), so let’s start by
    // introducing a new type variable for the key type. In the body of our method we’re going to need the value
    // associated with K. We’ll add an implicit Witness to do this for us. We can access the value of K using witness.value,
    // but the compiler has no way of knowing what type of tag we’re going to get. LabelledGeneric uses Symbols for tags,
    // so we’ll put a type bound on K and use symbol.name to convert it to a String:
    import shapeless.Witness
    import shapeless.labelled.FieldType

    implicit def hlistObjectEncoder[K <: Symbol, H, T <: HList](
                                                                 implicit
                                                                 witness: Witness.Aux[K],
                                                                 hEncoder: Lazy[JsonEncoder[H]],
                                                                 tEncoder: JsonObjectEncoder[T]
                                                               ): JsonObjectEncoder[FieldType[K, H] :: T] = {
      val fieldName: String = witness.value.name

      // hlistObjectEncoder returns HList so below call will derive createObjectEncoder's type A = HList
      // or FieldType[K, H] :: T. So, input function also takes HList as parameter hlist:HList
      createObjectEncoder { hlist =>
        val head = hEncoder.value.encode(hlist.head)
        val tail = tEncoder.encode(hlist.tail)
        JsonObject((fieldName, head) :: tail.fields)
      }
    }

    import shapeless.LabelledGeneric
    implicit def genericObjectEncoder[A, H](
                                             implicit
                                             generic: LabelledGeneric.Aux[A, H],
                                             hEncoder: Lazy[JsonObjectEncoder[H]]
                                           ): JsonEncoder[A] =
      createObjectEncoder { value =>
        hEncoder.value.encode(generic.to(value))
      }

    val iceCream = IceCream("Sundae", 1, false)

    println("Json with retained field names --> " + JsonEncoder[IceCream].encode(iceCream))


  }

  def derivingCoproductsInstancesWithLabelledGeneric = {
    import shapeless.{LabelledGeneric, HNil, HList, ::}
    sealed trait Shape
    final case class Rectangle(width: Double, height: Double) extends
      Shape
    final case class Circle(radius: Double) extends Shape
    println("Coproduct --> " + LabelledGeneric[Shape].to(Circle(1.0)))
    // Rectangle with KeyTag[Symbol with Tagged["Rectangle"], Rectangle] :+:
    // Circle with KeyTag[Symbol with Tagged["Circle"], Circle] :+:
    // CNil

    import JsonEncoderTypes._
    trait JsonObjectEncoder[A] extends JsonEncoder[A] {
      def encode(value: A): JsonObject
    }

    def createObjectEncoder[A](fn: A => JsonObject): JsonObjectEncoder[A] =
      new JsonObjectEncoder[A] {
        def encode(value: A): JsonObject =
          fn(value)
      }

    implicit val hnilEncoder: JsonObjectEncoder[HNil] = createObjectEncoder(hnil => JsonObject(Nil))

    import shapeless.{Coproduct, :+:, CNil, Inl, Inr, Witness, Lazy}
    import shapeless.labelled.FieldType
    implicit val cnilObjectEncoder: JsonObjectEncoder[CNil] = createObjectEncoder(cnil => throw new Exception("Inconceivable!"))

    // This is required encoder for case classes - Circle, Rectangle; this encoder is used by the coproduct encoder
    implicit def hlistObjectEncoder[K <: Symbol, H, T <: HList](
                                                                 implicit
                                                                 witness: Witness.Aux[K],
                                                                 hEncoder: Lazy[JsonEncoder[H]],
                                                                 tEncoder: JsonObjectEncoder[T]
                                                               ): JsonObjectEncoder[FieldType[K, H] :: T] = {
      val fieldName: String = witness.value.name

      // hlistObjectEncoder returns HList so below call will derive createObjectEncoder's type A = HList
      // or FieldType[K, H] :: T. So, input function also takes HList as parameter hlist:HList
      createObjectEncoder { hlist =>
        val head = hEncoder.value.encode(hlist.head)
        val tail = tEncoder.encode(hlist.tail)
        JsonObject((fieldName, head) :: tail.fields)
      }
    }

    implicit def coproductObjectEncoder[K <: Symbol, H, T <: Coproduct](implicit
                                                                        witness: Witness.Aux[K],
                                                                        hEncoder: Lazy[JsonEncoder[H]],
                                                                        tEncoder: JsonObjectEncoder[T]
                                                                       ): JsonObjectEncoder[FieldType[K, H] :+: T] = {
      val typeName = witness.value.name
      createObjectEncoder {
        case Inl(h) =>
          JsonObject(List(typeName -> hEncoder.value.encode(h)))
        case Inr(t) =>
          tEncoder.encode(t)
      }
    }

    import shapeless.LabelledGeneric
    implicit def genericObjectEncoder[A, H](
                                             implicit
                                             generic: LabelledGeneric.Aux[A, H],
                                             hEncoder: Lazy[JsonObjectEncoder[H]]
                                           ): JsonEncoder[A] =
      createObjectEncoder { value =>
        hEncoder.value.encode(generic.to(value))
      }

    val shape: Shape = Circle(1.0)
    println("Circle as Shape to Json --> " + JsonEncoder[Shape].encode(shape))
  }

  def main(args: Array[String]): Unit = {
    import shapeless.syntax.singleton._
    println(42.narrow)
    // x: Int(42) = 42
    //math.sqrt(4).narrow
    // <console>:17: error: Expression scala.math.`package`.sqrt(4.0) does not evaluate to a constant or a stable reference value
    //        math.sqrt(4.0).narrow

    // Add "-Yliteral-types" compiler option with Typelevel Scala or Lightbend Scala 2.13 is required
    //val theAnswer: 42 = 42
    //println(theAnswer)

    typeTaggingAndPhantomTypes
    derivingProductInstancesWithLabelledGeneric
    instancesForHLists
    derivingCoproductsInstancesWithLabelledGeneric
  }
}

object JsonEncoderTypes {

  sealed trait JsonValue

  case class JsonObject(fields: List[(String, JsonValue)]) extends
    JsonValue

  case class JsonArray(items: List[JsonValue]) extends JsonValue

  case class JsonString(value: String) extends JsonValue

  case class JsonNumber(value: Double) extends JsonValue

  case class JsonBoolean(value: Boolean) extends JsonValue

  case object JsonNull extends JsonValue

  trait JsonEncoder[A] {
    def encode(value: A): JsonValue
  }

  object JsonEncoder {
    def apply[A](implicit enc: JsonEncoder[A]): JsonEncoder[A] = enc
  }

  def createEncoder[A](func: A => JsonValue): JsonEncoder[A] = new JsonEncoder[A] {
    def encode(value: A): JsonValue = func(value)
  }

  // a few primitive instances
  implicit val stringEncoder: JsonEncoder[String] =
    createEncoder(str => JsonString(str))
  implicit val doubleEncoder: JsonEncoder[Double] =
    createEncoder(num => JsonNumber(num))
  implicit val intEncoder: JsonEncoder[Int] =
    createEncoder(num => JsonNumber(num))
  implicit val booleanEncoder: JsonEncoder[Boolean] = createEncoder(bool => JsonBoolean(bool))

  // a few instance combinators
  implicit def listEncoder[A]
  (implicit enc: JsonEncoder[A]): JsonEncoder[List[A]] =
    createEncoder(list => JsonArray(list.map(enc.encode)))

  implicit def optionEncoder[A]
  (implicit enc: JsonEncoder[A]): JsonEncoder[Option[A]] =
    createEncoder(opt => opt.map(enc.encode).getOrElse(JsonNull))
}

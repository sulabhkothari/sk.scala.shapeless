
object AutoDeriveTypeClassInstances {
  // Type classes are a programming pa􏰂ern borrowed from Haskell (the word “class” has nothing to do with classes in
  // object oriented programming). We encode them in Scala using traits and implicits. A type class is a parameterised
  //trait representing some sort of general functionality that we would like to apply to a wide range of types

  def resolvingInstances = {
    // Even with implicit resolution power, the compiler can’t pull apart our case classes and sealed traits. We are
    // required to define instances for ADTs by hand. Shapeless’ generic representations change all of this, allowing us
    // to derive instances for any ADT for free.


    // The apply method, known as a “summoner” or “materializer”, allows us to summon a type class instance given a target type
    CsvEncoder[IceCream]

    // when working with shapeless we encounter situations where implicitly doesn’t infer types correctly. We can always
    // define the summoner method to do the right thing, so it’s worth writing one for every type class we create.
    // We can also use a special method from shapeless called “the” (more on this later):
    import shapeless._
    the[CsvEncoder[IceCream]]
    // The instance method, sometimes named pure, provides a terse syntax for creating new type class instances, reducing
    // the boilerplate of anonymous class syntax:

    //implicit val booleanEncoder: CsvEncoder[Boolean] = instance(b => if(b) List("yes") else List("no"))
    val reprEncoder: CsvEncoder[String :: Int :: Boolean :: HNil] = implicitly
    println(reprEncoder.encode("abc" :: 123 :: true :: HNil))

    //println()
    val iceCreams: List[IceCream] = List(
      IceCream("Sundae", 1, false),
      IceCream("Cornetto", 0, true),
      IceCream("Banana Split", 0, false)
    )

    println(CsvEncoder.writeCsv(iceCreams))
  }

  def derivingInstancesForCoproducts = {
    import AlgebraicTypesGenericRepresentations.{Shape, Rectangle, Circle}
    import CsvEncoder._

    val shapes: List[Shape] = List(
      Rectangle(3.0, 4.0),
      Circle(1.0)
    )
    println(writeCsv(shapes))

    // SI-7046 and you
    //There is a Scala compiler bug called SI-7046 that can cause coproduct generic resolution to fail. The bug causes
    // certain parts of the macro API, on which shapeless depends, to be sensitive to the order of the definitions in our
    // source code. Problems can o􏰁en be worked around by reordering code and renaming files, but such workarounds tend
    // to be volatile and unreliable.
    //If you are using Lightbend Scala 2.11.8 or earlier and coproduct resolution fails for you, consider upgrading to
    // Lightbend Scala 2.11.9 or Typelevel Scala 2.11.8. SI-7046 is fixed in each of these releases.
  }

  def derivingInstancesForRecursiveTypes = {
    sealed trait Tree[A]
    case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
    case class Leaf[A](value: A) extends Tree[A]

    // Recursive implicit resolution fails
    // val csnc = CsvEncoder[Tree[Int]]

    // If the compiler sees the same type constructor twice and the complexity of the type parameters is increasing, it
    // assumes that branch of search is “diverging”. This is a problem for shapeless because types like ::[H, T] and
    // :+:[H, T] can appear several times as the compiler expands different generic representations. This causes the
    // compiler to give up prematurely even though it would eventually find a solution if it persisted with the same
    // expansion. Consider the following types:
    case class Bar(baz: Int, qux: String)
    case class Foo(bar: Bar)
    // The expansion for CsvEncoder[Foo] looks like this:
    // CsvEncoder[Foo]
    // CsvEncoder[Bar :: HNil]
    // CsvEncoder[Bar]
    // CsvEncoder[Int :: String :: HNil] // 4 uh oh
    // The compiler attempts to resolve a CsvEncoder[::[H, T]] twice in this branch of search, on lines 2 and 4. The type
    // parameter for T is more complex on line 4 than on line 2, so the compiler assumes (incorrectly in this case) that
    // the branch of search is diverging. It moves onto another branch and, again, the result is failure to generate a
    // suitable instance.

    println(CsvEncoderForLazy.writeCsv(List(Branch(Branch(Branch(Leaf(89),Leaf(0)),Leaf(100)),Branch(Leaf(12), Leaf(99))))))
    println(CsvEncoderForLazy.writeCsv(List(Foo(Bar(12,"abc")))))

  }

  def debuggingImplicits = {
    // The reify method from scala.reflect takes a Scala expression as a parameter and returns an AST object representing
    // the expression tree, complete with type annotations:
    import scala.reflect.runtime.universe.reify
    println(reify(CsvEncoder[Int]))
  }

  def main(args: Array[String]): Unit = {
    resolvingInstances
    derivingInstancesForCoproducts
    derivingInstancesForRecursiveTypes
    debuggingImplicits
  }

}

case class IceCream(name: String, numCherries: Int, inCone: Boolean)

trait CsvEncoder[A] {
  def encode(value: A): List[String]
}

object CsvEncoder {
  //  implicit def pairEncoder[A, B](
  //                                  implicit
  //                                  aEncoder: CsvEncoder[A],
  //                                  bEncoder: CsvEncoder[B]
  //                                ): CsvEncoder[(A, B)] =
  //    new CsvEncoder[(A, B)] {
  //      def encode(pair: (A, B)): List[String] = {
  //        val (a, b) = pair
  //        aEncoder.encode(a) ++ bEncoder.encode(b)
  //      }
  //    }

  // "Summoner" method
  def apply[A](implicit enc: CsvEncoder[A]): CsvEncoder[A] =
    enc

  // "Constructor" method
  def instance[A](func: A => List[String]): CsvEncoder[A] = new CsvEncoder[A] {
    def encode(value: A): List[String] =
      func(value)
  }

  def createEncoder[A](func: A => List[String]): CsvEncoder[A] = new CsvEncoder[A] {
    def encode(value: A): List[String] = func(value)
  }

  implicit val stringEncoder: CsvEncoder[String] =
    createEncoder(str => List(str))
  implicit val intEncoder: CsvEncoder[Int] =
    createEncoder(num => List(num.toString))
  implicit val booleanEncoder: CsvEncoder[Boolean] = createEncoder(bool => List(if (bool) "yes" else "no"))

  import shapeless.{HList, ::, HNil}

  implicit val hnilEncoder: CsvEncoder[HNil] =
    createEncoder(hnil => Nil)

  implicit def hlistEncoder[H, T <: HList](
                                            implicit
                                            hEncoder: CsvEncoder[H],
                                            tEncoder: CsvEncoder[T]
                                          ): CsvEncoder[H :: T] =
    createEncoder {
      case h :: t =>
        hEncoder.encode(h) ++ tEncoder.encode(t)
    }

  import shapeless.Generic
  // Not Required after addition of genericEncoder
  //  implicit val iceCreamEncoder: CsvEncoder[IceCream] = {
  //    val gen = Generic[IceCream]
  //    val enc = CsvEncoder[gen.Repr]
  //    createEncoder(iceCream => enc.encode(gen.to(iceCream)))
  //  }

  def writeCsv[A](values: List[A])(implicit enc: CsvEncoder[A]): String =
    values.map(value => enc.encode(value).mkString(",")).mkString("\n")

  // Given a type A and an HList type R, an implicit Generic to map A to R, and a CsvEncoder for R, create a CsvEncoder for A.
  //  implicit def genericEncoder[A, R](
  //                                     implicit
  //                                     gen: Generic[A] {type Repr = R},
  //                                     enc: CsvEncoder[R]
  //                                   ): CsvEncoder[A] = createEncoder(a => enc.encode(gen.to(a)))
  // This is commented in  favour of Aux types

  // The compiler expands a call like:
  //  writeCsv(iceCreams)
  // to use our family of deriva􏰀on rules:
  // writeCsv(iceCreams)(
  //  genericEncoder(
  //    Generic[IceCream],
  //    hlistEncoder(stringEncoder,
  //hlistEncoder(intEncoder, hlistEncoder(booleanEncoder, hnilEncoder)))))
  // and can infer the correct expansions for many different product types.

  // Aux type aliases
  // Type refinements like Generic[A] { type Repr = L } are verbose and difficult to read, so shapeless provides a type
  // alias Generic.Aux to rephrase the type member as a type parameter:
  // package shapeless
  // object Generic {
  //  type Aux[A, R] = Generic[A] { type Repr = R }
  //}

  implicit def genericEncoder[A, R](
                                     implicit
                                     gen: Generic.Aux[A, R],
                                     enc: CsvEncoder[R]
                                   ): CsvEncoder[A] = createEncoder(a => enc.encode(gen.to(a)))

  // If shapeless can’t calculate a Generic it means that the type in question isn’t an ADT— somewhere in the algebra
  // there is a type that isn’t a case class or a sealed abstract type.


  import shapeless.{Coproduct, :+:, CNil, Inl, Inr}

  implicit def coproductEncoder[H, T <: Coproduct](
                                                    implicit hEncoder: CsvEncoder[H],
                                                    tEncoder: CsvEncoder[T]
                                                  ): CsvEncoder[H :+: T] = createEncoder {
    case Inl(h) => hEncoder.encode(h)
    case Inr(t) => tEncoder.encode(t)
  }

  // Alarmingly, the encoder for CNil throws an exception! Don’t panic, though. Remember that we can’t create values of
  // type CNil, so the throw expression is dead code. It’s ok to fail abruptly here because we will never reach this point.
  implicit val cnilEncoder: CsvEncoder[CNil] =
  createEncoder(cnil => throw new Exception("Inconceivable!"))

  implicit val doubleEncoder: CsvEncoder[Double] =
    createEncoder(d => List(d.toString))
}

trait CsvEncoderForLazy[A] {
  def encode(value: A): List[String]
}

object CsvEncoderForLazy {
  def apply[A](implicit enc: CsvEncoderForLazy[A]): CsvEncoderForLazy[A] =
    enc

  def createEncoder[A](func: A => List[String]): CsvEncoderForLazy[A] = new CsvEncoderForLazy[A] {
    def encode(value: A): List[String] = func(value)
  }

  implicit val stringEncoder: CsvEncoderForLazy[String] =
    createEncoder(str => List(str))
  implicit val intEncoder: CsvEncoderForLazy[Int] =
    createEncoder(num => List(num.toString))
  implicit val booleanEncoder: CsvEncoderForLazy[Boolean] = createEncoder(bool => List(if (bool) "yes" else "no"))

  import shapeless.{HList, ::, HNil}

  implicit val hnilEncoder: CsvEncoderForLazy[HNil] =
    createEncoder(hnil => Nil)

  import shapeless.Generic

  def writeCsv[A](values: List[A])(implicit enc: CsvEncoderForLazy[A]): String =
    values.map(value => enc.encode(value).mkString(",")).mkString("\n")

  import shapeless.{Coproduct, :+:, CNil, Inl, Inr}

  implicit val cnilEncoder: CsvEncoderForLazy[CNil] =
    createEncoder(cnil => throw new Exception("Inconceivable!"))

  implicit val doubleEncoder: CsvEncoderForLazy[Double] =
    createEncoder(d => List(d.toString))

  // We use Lazy by wrapping it around specific implicit parameters. As a rule of thumb, it is always a good idea to wrap
  // the “head” parameter of any HList or Coproduct rule and the Repr parameter of any Generic rule in Lazy:
  import shapeless.Lazy

  implicit def hlistEncoder[H, T <: HList](
                                            implicit
                                            hEncoder: Lazy[CsvEncoderForLazy[H]], // wrap in Lazy
                                            tEncoder: CsvEncoderForLazy[T]
                                          ): CsvEncoderForLazy[H :: T] = createEncoder {
    case h :: t =>
      hEncoder.value.encode(h) ++ tEncoder.encode(t)
  }

  implicit def coproductEncoder[H, T <: Coproduct](
                                                    implicit
                                                    hEncoder: Lazy[CsvEncoderForLazy[H]], // wrap in Lazy
                                                    tEncoder: CsvEncoderForLazy[T]
                                                  ): CsvEncoderForLazy[H :+: T] = createEncoder {
    case Inl(h) => hEncoder.value.encode(h)
    case Inr(t) => tEncoder.encode(t)
  }

  implicit def genericEncoder[A, R](
                                     implicit
                                     gen: Generic.Aux[A, R],
                                     rEncoder: Lazy[CsvEncoderForLazy[R]] // wrap in Lazy
                                   ): CsvEncoderForLazy[A] = createEncoder { value =>
    rEncoder.value.encode(gen.to(value))
  }
}
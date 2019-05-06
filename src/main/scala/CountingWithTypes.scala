// Terms that are usually considered primitive in other notations (such as integers, booleans, pairs, lists, and tagged
// unions) are mapped to higher-order functions under Church encoding. The Church-Turing thesis asserts that any
// computable operator (and its operands) can be represented under Church encoding. In the untyped lambda calculus the
// only primitive data type is the function.
object CountingWithTypes {
  def representingNumbersAsTypes = {
    // Shapeless uses “Church encoding” to represent natural numbers at the type level. It provides a type Nat with two
    // subtypes: _0 represen􏰀ng zero, and Succ[N] represen􏰀ng N+1:
    import shapeless.{Nat, Succ}
    type Zero = Nat._0
    type One = Succ[Zero]
    type Two = Succ[One]

    // Shapeless provides aliases for the first 22 Nats as Nat._N:
    // Nat._1
    // Nat._2
    // Nat._3
    // println(Nat.toInt(Nat._1))

    //Nat has no run􏰀me semanti􏰀cs. We have to use the ToInt type class to convert a Nat to a run􏰀me Int:
    import shapeless.ops.nat.ToInt
    val toInt = ToInt[Two]
    println("Nat to runtime Int --> " + toInt.apply())

    // The Nat.toInt method provides a convenient shorthand for calling toInt.apply(). It accepts the instance of ToInt
    // as an implicit parameter:
    println("Convenient Shorthand --> " + Nat.toInt[Nat._3])

  }

  def lengthOfGenericRepresentations = {
    // One use case for Nat is determining the lengths of HLists and Coproducts.
    // Shapeless provides the shapeless.ops.hlist.Length and shapeless.ops.coproduct. type classes for this:
    import shapeless._
    import shapeless.ops.{hlist, coproduct, nat}
    val hlistLength = hlist.Length[String :: Int :: Boolean :: HNil]
    val coproductLength = coproduct.Length[Double :+: Char :+: CNil]
    println("HList length --> " + Nat.toInt[hlistLength.Out])
    println("Coproduct length --> " + Nat.toInt[coproductLength.Out])

    trait SizeOf[A] {
      def value: Int
    }
    def sizeOf[A](implicit size: SizeOf[A]): Int = size.value

    implicit def genericSizeOf[A, L <: HList, N <: Nat](
                                                         implicit
                                                         generic: Generic.Aux[A, L],
                                                         size: hlist.Length.Aux[L, N],
                                                         sizeToInt: nat.ToInt[N]
                                                       ): SizeOf[A] =
      new SizeOf[A] {
        val value = sizeToInt.apply()
      }

    case class IceCream(name: String, numCherries: Int, inCone: Boolean)
    println("sizeOf type class --> " + sizeOf[IceCream])
  }

  def randomValueGenerator = {
    trait Random[A] {
      def get: A
    }
    def random[A](implicit r: Random[A]): A = r.get

    // Instance constructor:
    def createRandom[A](func: () => A): Random[A] =
      new Random[A] {
        def get = func()
      }

    // Random numbers from 0 to 9:
    implicit val intRandom: Random[Int] = createRandom(() => scala.util.Random.nextInt(10))
    // Random characters from A to Z:
    implicit val charRandom: Random[Char] =
      createRandom(() => ('A'.toInt + scala.util.Random.nextInt(26)).
      toChar)
    // Random booleans:
    implicit val booleanRandom: Random[Boolean] = createRandom(() => scala.util.Random.nextBoolean)

    for (i <- 1 to 3) println(random[Int])
    for (i <- 1 to 3) println(random[Char])

    import shapeless._
    implicit def genericRandom[A, R](
                                      implicit
                                      gen: Generic.Aux[A, R],
                                      random: Lazy[Random[R]]
                                    ): Random[A] =
      createRandom(() => gen.from(random.value.get))

    implicit val hnilRandom: Random[HNil] =
      createRandom(() => HNil)

    implicit def hlistRandom[H, T <: HList](
                                             implicit
                                             hRandom: Lazy[Random[H]],
                                             tRandom: Random[T]
                                           ): Random[H :: T] =
      createRandom(() => hRandom.value.get :: tRandom.get)

    case class Cell(col: Char, row: Int)
    for (i <- 1 to 5) println(random[Cell])

    implicit val cnilRandom: Random[CNil] =
      createRandom(() => throw new Exception("Inconceivable!"))


    // The correct behaviour should be to choose H 1/n of the 􏰀me, where n is the length of the coproduct. This ensures
    // an even probability distribu􏰀on across the sub-types of the coproduct. It also ensures we choose the head of a
    // single-subtype Coproduct 100% of the 􏰀me, which means we never call cnilProduct.get.
    import shapeless.ops.coproduct
    import shapeless.ops.nat.ToInt
    implicit def coproductRandom[H, T <: Coproduct, L <: Nat](
                                                               implicit
                                                               hRandom: Lazy[Random[H]],
                                                               tRandom: Random[T],
                                                               tLength: coproduct.Length.Aux[T, L],
                                                               tLengthAsInt: ToInt[L]
                                                             ): Random[H :+: T] = {
      createRandom { () =>
        val length = 1 + tLengthAsInt()
        val chooseH = scala.util.Random.nextDouble < (1.0 / length)
        if (chooseH) Inl(hRandom.value.get) else Inr(tRandom.get)
      }
    }

    sealed trait Light
    case object Red extends Light
    case object Amber extends Light
    case object Green extends Light

    for (i <- 1 to 100) println(random[Light])
  }

  def otherNatOperations = {
    import shapeless._
    val hlist = 123 :: "foo" :: true :: 'x' :: HNil
    println("HList starts with Nat._0")
    println("Get zeroth value --> " + hlist.apply[Nat._0])
    println("Get first value --> " + hlist.apply[Nat._1])
    println("Get third value --> " + hlist.apply[Nat._3])
    println("Take & Drop --> " + hlist.take(Nat._3).drop(Nat._1))
    println("UpdatedAt --> " + hlist.updatedAt(Nat._1, "bar").updatedAt(Nat._2, "baz"))
  }

  def main(args: Array[String]): Unit = {
    representingNumbersAsTypes
    lengthOfGenericRepresentations
    randomValueGenerator
    otherNatOperations
  }
}

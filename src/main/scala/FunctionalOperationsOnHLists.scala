// This is not real shapeless code.
// It's just for demonstration.
//trait Case[P, A] {
//  type Result
//  def apply(a: A): Result
//}
//trait Poly {
//def apply[A](arg: A)(implicit cse: Case[this.type, A]): cse.Result =
//    cse.apply(arg)
//}

// object myPoly extends Poly {
//  implicit def intCase =
//    new Case[this.type, Int] {
//      type Result = Double
//      def apply(num: Int): Double = num / 2.0
//}
//  implicit def stringCase =
//    new Case[this.type, String] {
//      type Result = Int
//      def apply(str: String): Int = str.length
//    }
//}
//  myPoly.apply(123)
// res8: Double = 61.5

object FunctionalOperationsOnHLists {
  def polymorphicFunctions = {

    //1. We’re extending a trait called Poly1 instead of Poly. Shapeless has a Poly type and a set of subtypes, Poly1
    //   through Poly22, supporti􏰀ng different ari􏰀es of polymorphic func􏰀tion.
    //2. The Case.Aux types doesn’t seem to reference the singleton type of the Poly. Case.Aux is actually a type alias
    //   defined within the body of Poly1. The singleton type is there, we just don’t see it.
    //3. We’re using a helper method, at, to define cases. This acts as an instance constructor method as discussed in
    //   Secti􏰀on 3.1.2), which eliminates a lot of boilerplate.
    import shapeless._
    object myPoly extends Poly1 {
      implicit val intCase: Case.Aux[Int, Double] =
        at(num => num / 2.0)
      implicit val stringCase: Case.Aux[String, Int] =
        at(str => str.length)
    }

    println("Poly1 --> " + myPoly.apply(123))
    println("Poly1 --> " + myPoly.apply("hello"))

    object multiply extends Poly2 {
      implicit val intIntCase: Case.Aux[Int, Int, Int] =
        at((a, b) => a * b)
      implicit val intStrCase: Case.Aux[Int, String, String] = at((a, b) => b * a)
    }

    println("Poly2 --> " + multiply(3, 4))
    println("Poly2 --> " + multiply(3, "4"))

    // Because Cases are just implicit values, we can define cases based on type classes and do all of the advanced implicit
    // resoluti􏰀on covered in previous chapters.
    import scala.math.Numeric
    object total extends Poly1 {
      implicit def base[A](implicit num: Numeric[A]):
      Case.Aux[A, Double] =
        at(num.toDouble)

      implicit def option[A](implicit num: Numeric[A]):
      Case.Aux[Option[A], Double] = at(opt => opt.map(num.toDouble).getOrElse(0.0))

      implicit def list[A](implicit num: Numeric[A]):
      Case.Aux[List[A], Double] =
        at(list => num.toDouble(list.sum))
    }
    println("Poly with implicit resolution --> " + total(10))
    println("Poly with implicit resolution --> " + total(Option(20.0)))
    println("Poly with implicit resolution --> " + total(List(1L, 2L, 3L)))

    // Idiosyncrasies of type inference
    // Poly pushes Scala’s type inference out of its comfort zone. We can easily confuse the compiler by asking it to do
    // too much inference at once. For example, the following code compiles ok:
    val a = myPoly.apply(123)
    val b: Double = a
    // However, combining the two lines causes a compila􏰀tion error:
    // val a1: Double = myPoly.apply(123)
    // <console>:17: error: type mismatch;
    //  found   : Int(123)
    //required: myPoly.ProductCase.Aux[shapeless.HNil,?] (which expands to) shapeless.poly.Case[myPoly.type,
    // shapeless.HNil]{type Result = ?}
    //      val a: Double = myPoly.apply(123)
    //                                   ^
    //If we add a type annota􏰀on, the code compiles again:
    val a2: Double = myPoly.apply[Int](123)

    // This behaviour is confusing and annoying. Unfortunately there are no concrete rules to follow to avoid problems.
    // The only general guideline is to try not to over-constrain the compiler, solve one constraint at a 􏰀me, and give
    // it a hint when it gets stuck.
  }

  def mapAndFlatMapUsingPoly = {
    import shapeless._
    object sizeOf extends Poly1 {
      implicit val intCase: Case.Aux[Int, Int] =
        at(identity)
      implicit val stringCase: Case.Aux[String, Int] =
        at(_.length)
      implicit val booleanCase: Case.Aux[Boolean, Int] =
        at(bool => if (bool) 1 else 0)
    }
    println("HList map using Poly --> " + (10 :: "hello" :: true :: HNil).map(sizeOf))

    //error
    //(1.5 :: HNil).map(sizeOf)

    // We can also flatMap over an HList, as long as every corresponding case in our Poly returns another HList:
    object valueAndSizeOf extends Poly1 {
      implicit val intCase: Case.Aux[Int, Int :: Int :: HNil] =
        at(num => num :: num :: HNil)
      implicit val stringCase: Case.Aux[String, String :: Int :: HNil] = at(str => str :: str.length :: HNil)
      implicit val booleanCase: Case.Aux[Boolean, Boolean :: Int :: HNil] =
        at(bool => bool :: (if (bool) 1 else 0) :: HNil)
    }
    println("HList flatMap using Poly --> " + (10 :: "hello" :: true :: HNil).flatMap(valueAndSizeOf))
  }

  def foldingUsingPoly = {
    import shapeless._
    object sum extends Poly2 {
      implicit val intIntCase: Case.Aux[Int, Int, Int] =
        at((a, b) => a + b)
      implicit val intStringCase: Case.Aux[Int, String, Int] = at((a, b) => a + b.length)
    }
    println("HList foldLeft using Poly --> " + (10 :: "hello" :: 100 :: HNil).foldLeft(0)(sum))

    // We can also reduceLeft, reduceRight, foldMap, and so on. Each opera􏰀tion has its own associated type class.
  }

  def definingTypeClassesUsingPoly = {
    trait ProductMapper[A, B, P] {
      def apply(a: A): B
    }

    import shapeless._
    import shapeless.ops.hlist
    implicit def genericProductMapper[
    A, B,
    P <: Poly,
    ARepr <: HList,
    BRepr <: HList
    ](
       implicit
       aGen: Generic.Aux[A, ARepr],
       bGen: Generic.Aux[B, BRepr],
       mapper: hlist.Mapper.Aux[P, ARepr, BRepr]
     ): ProductMapper[A, B, P] =
      new ProductMapper[A, B, P] {
        def apply(a: A): B =
          bGen.from(mapper.apply(aGen.to(a)))
      }

    // Interes􏰀tingly, although we define a type P for our Poly, we don’t reference any values of type P anywhere in our
    // code. The Mapper type class uses implicit resoluti􏰀on to find Cases, so the compiler only needs to know the
    // singleton type of P to locate the relevant instances.

    implicit class ProductMapperOps[A](a: A) {

      class Builder[B] {
        def apply[P <: Poly](poly: P)
                            (implicit pm: ProductMapper[A, B, P]): B =
          pm.apply(a)
      }

      def mapTo[B]: Builder[B] = new Builder[B]
    }

    object conversions extends Poly1 {
      implicit val intCase: Case.Aux[Int, Boolean] = at(_ > 0)
      implicit val boolCase: Case.Aux[Boolean, Int] = at(if (_) 1 else 0)
      implicit val strCase: Case.Aux[String, String] = at(identity)
    }
    case class IceCream1(name: String, numCherries: Int, inCone: Boolean)
    case class IceCream2(name: String, hasCherries: Boolean, numCones: Int)
    println("Type Mapping using Poly & HLists --> "+IceCream1("Sundae", 1, false).mapTo[IceCream2](conversions))
  }


  def main(args: Array[String]): Unit = {
    polymorphicFunctions
    mapAndFlatMapUsingPoly
    definingTypeClassesUsingPoly
  }
}

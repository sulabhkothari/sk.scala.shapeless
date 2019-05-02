object TypesAndImplicits {
  def dependentTypes = {
    import shapeless.Generic
    def getRepr[A](value: A)(implicit gen: Generic[A]) = gen.to(value)

    trait Generic2[A, Repr]
    def getRepr2[A, R](value: A)(implicit generic: Generic2[A, R]): R = ???

    case class Vec(x: Int, y: Int)
    case class Rect(origin: Vec, size: Vec)
    println(getRepr(Vec(1, 2)))
    println(getRepr(Rect(Vec(0, 0), Vec(5, 5))))

    // What we’re seeing here is called dependent typing: the result type of getRepr depends on its value parameters via
    // their type members.
  }

  def dependentlyTypedFunctions = {
    import shapeless.{HList, Generic, ::, HNil}
    import shapeless.ops.hlist.Last

    case class Employee(id: Int, age: Int, name: String)
    println(Generic[Employee].to(Employee(12, 22, "Abc")).last)
    val last1 = Last[String :: Int :: HNil]
    val last2 = Last[Int :: String :: HNil]
    println(last1("foo" :: 123 :: HNil))
    println(last2(321 :: "bar" :: HNil))

    // We get two forms of protection against errors. The implicits defined for Last ensure we can only summon instances
    // if the input HList has at least one element:
    //Last[HNil]

    // In addition, the type parameters on the instances of Last check whether we pass in the expected type of HList:
    // last1(321 :: "bar" :: HNil)

    trait Second[L <: HList] {
      type Out

      def apply(value: L): Out
    }
    object Second {
      type Aux[L <: HList, O] = Second[L] {type Out = O}

      def apply[L <: HList](implicit inst: Second[L]): Aux[L, inst.Out] = inst
    }
    // This code uses the idiomatic layout described in Section 3.1.2. We define the Aux type in the companion object
    // beside the standard apply method for summoning instances.

    // Summoner methods versus “implicitly” versus “the”
    // Note that the return type on apply is Aux[L, O], not Second[L]. This is important. Using Aux ensures the apply
    // method does not erase the type members on summoned instances. If we define the return type as Second[L], the Out
    // type member will be erased from the return type and the type class will not work correctly.

    // The implicitly method from scala.Predef has this behaviour. Compare the type of an instance of Last summoned with
    // implicitly:
    implicitly[Last[String :: Int :: HNil]]
    // res6: shapeless.ops.hlist.Last[String :: Int :: shapeless. HNil] = shapeless.ops.hlist$Last$$anon$34@651110a2

    // to the type of an instance summoned with Last.apply:
    println(Last[String :: Int :: HNil])
    // res7: shapeless.ops.hlist.Last[String :: Int :: shapeless. HNil]{type Out = Int} = shapeless.ops. hlist$Last$$anon$34@571bb8f6

    // The type summoned by implicitly has no Out type member. For this reason, we should avoid implicitly when working
    // with dependently typed func􏰀ons. We can either use custom summoner methods, or we can use shapeless’ replacement
    // method, the:
    import shapeless.the
    println(the[Last[String :: Int :: HNil]])

    import Second._
    implicit def hlistSecond[A, B, Rest <: HList]: Aux[A :: B :: Rest, B] =
      new Second[A :: B :: Rest] {
        type Out = B

        def apply(value: A :: B :: Rest): B =
          value.tail.head
      }

    val second1 = Second[String :: Boolean :: Int :: HNil]
    val second2 = Second[String :: Int :: Boolean :: HNil]
    println(second1("efg" :: true :: 12 :: HNil))
    println(second2("hij" :: 12 :: false :: HNil))
    val second3 = Second[Int :: Int :: String :: HNil]
    println(second3(1 :: 2 :: "abs" :: HNil))

    // Summoning is subject to similar constraints as Last. If we try to summon an instance for an incompatible HList,
    // resolution fails and we get a compile error:
    //Second[String :: HNil]
  }

  def chainingDependentFunctions = {
    // Dependently typed functions provide a means of calcula􏰀ng one type from another. We can chain dependently typed
    // func􏰀ons to perform calculations involving multiple steps. For example, we should be able to use a Generic to
    // calculate a Repr for a case class, and use a Last to calculate the type of the last element. Let’s try coding this:
    import shapeless.ops.hlist.Last
    import shapeless.{Generic, HList, HNil, ::}
    def lastField[A, Repr <: HList](input: A)(
      implicit
      gen: Generic.Aux[A, Repr],
      last: Last[Repr]
    ): last.Out = last.apply(gen.to(input))

    case class Vec(x: Int, y: Int)

    case class Rect(origin: Vec, size: Vec)

    println(lastField(Rect(Vec(1, 2), Vec(3, 4))))

    // As a general rule, we always write code in this style. By encoding all the free variables as type parameters, we
    // enable the compiler to unify them with appropriate types. This goes for more subtle constraints as well.
    // For example, suppose we wanted to summon a Generic for a case class of exactly one field. We might be tempted to
    // write this:
    //    def getWrappedValue[A, H](input: A)(
    //      implicit
    //      gen: Generic.Aux[A, H :: HNil]
    //    ): H = gen.to(input).head
    case class Wrapper(value: Int)
    getWrappedValue(Wrapper(42))
    // The method definition compiles but the compiler can never find implicits at the call site.
    // The problem is that the gen parameter is over-constrained: the compiler can’t find a Repr and ensure its length
    // at the same time. The type Nothing also o􏰁en provides a clue, appearing when the compiler fails to unify covariant
    // type parameters.
    //The solution to our problem above is to separate implicit resolution into steps:
    // 1. find a Generic with a suitable Repr for A;
    // 2. provide the Repr that has a head type H.

    // IsHCons is a shapeless type class that splits an HList into a Head and Tail. We can use IsHCons instead of =:=:

    import shapeless.ops.hlist.IsHCons
    def getWrappedValue[A, Repr <: HList, Head](in: A)(
      implicit
      gen: Generic.Aux[A, Repr],
      isHCons: IsHCons.Aux[Repr, Head, HNil]
    ): Head = gen.to(in).head

    println(getWrappedValue(Wrapper(42)))
  }

  def main(args: Array[String]): Unit = {
    dependentTypes
    dependentlyTypedFunctions
    chainingDependentFunctions
  }
}

// Dependent typing:
// def getRepr[A](value: A)(implicit gen: Generic[A]) = gen.to(value)
// the result type of getRepr depends on its value parameters via their type members.

// When coding with shapeless, we are oft􏰁en trying to find a target type that depends on values in our code. This
// relationship is called dependent typing.

// Problems involving dependent types can be conveniently expressed using implicit search, allowing the compiler to
// resolve intermediate and target types given a starting point at the call site.

//1. We should extract every intermediate type out to a type parameter. Many type parameters won’t be used in the result,
//   but the compiler needs them to know which types it has to unify.
//2. The compiler resolves implicits from left 􏰁to right, back tracking if it can’t find a working combination. We should write implicits
//   in the order we need them, using one or more type variables to connect them to previous implicits.
//3. The compiler can only solve for one constraint at a time, so we mustn’t over-constrain any single implicit.
//4. We should state the return type explicitly, specifying any type parameters and type members that may be needed
//   elsewhere. Type members are oft􏰁en important, so we should use Aux types to preserve them where appropriate. If we
//   don’t state them in the return type, they won’t be available to the compiler for further implicit resolution.
//5. The Aux type alias pattern is useful for keeping code readable. We should look out for Aux aliases when using tools
//   from the shapeless toolbox, and implement Aux aliases on our own dependently typed functions.

// When we find a useful chain of dependently typed operations we can capture them as a single type class. This is
// sometimes called the “lemma” pattern (a term borrowed from mathematical proofs).
// There are three general sets of ops, available from three packages:
//• shapeless.ops.hlist defines type classes for HLists. These can be used directly via extension methods on HList,
//  defined in shapeless.syntax.hlist.
//• shapeless.ops.coproduct defines type classes for Coproducts. These can be used directly via extension methods on
//  Coproduct, defined in shapeless.syntax.coproduct.
//• shapeless.ops.record defines type classes for shapeless records (HLists containing tagged elements—Sec􏰀on 5.2).
//  These can be used via extension methods on HList, imported from shapeless.record,
//  and defined in shapeless.syntax.record.

object WorkingWithHListsAndCoproducts {

  def simpleOpsExamples = {
    // HList has init and last extension methods based on two type classes: shapeless.ops.hlist.Init and
    // shapeless.ops.hlist.Last. While init drops the last element of an HList, last drops all except the last one.
    import shapeless._
    println("Last --> " + ("Hello" :: 123 :: true :: HNil).last)
    println("Init --> " + ("Hello" :: 123 :: true :: HNil).init)

    // Second, the type class is only defined for HLists with at least one element. This gives us a degree of sta􏰀c checking.
    //HNil.last
  }

  // The "lemma" pattern
  def creatingCustomOp = {
    import shapeless._
    trait Penultimate[L] {
      type Out

      def apply(l: L): Out
    }
    object Penultimate {
      type Aux[L, O] = Penultimate[L] {type Out = O}

      def apply[L](implicit p: Penultimate[L]): Aux[L, p.Out] = p
    }

    import shapeless.ops.hlist
    implicit def hlistPenultimate[L <: HList, M <: HList, O](
                                                              implicit
                                                              init: hlist.Init.Aux[L, M],
                                                              last: hlist.Last.Aux[M, O]
                                                            ): Penultimate.Aux[L, O] =
      new Penultimate[L] {
        type Out = O

        def apply(l: L): O =
          last.apply(init.apply(l))
      }

    type BigList = String :: Int :: Boolean :: Double :: HNil
    val bigList: BigList = "foo" :: 123 :: true :: 456.0 :: HNil
    println("Penultimate type --> " + Penultimate[BigList].apply(bigList))

    //type TinyList = String :: HNil
    //val tinyList = "bar" :: HNil
    //Penultimate[TinyList].apply(tinyList)
    // <console>:21: error: could not find implicit value for parameter p: Penultimate[TinyList]
    //        Penultimate[TinyList].apply(tinyList)
    //                   ^

    implicit class PenultimateOps[A](a: A) {
      def penultimate(implicit inst: Penultimate[A]): inst.Out =
        inst.apply(a)
    }
    println("Penultimate extension --> " + bigList.penultimate)

    implicit def genericPenultimate[A, R, O](
                                              implicit
                                              generic: Generic.Aux[A, R],
                                              penultimate: Penultimate.Aux[R, O]
                                            ): Penultimate.Aux[A, O] = new Penultimate[A] {
      type Out = O

      def apply(a: A): O =
        penultimate.apply(generic.to(a))
    }

    case class IceCream(name: String, numCherries: Int, inCone: Boolean)
    println("For Product Types --> " + IceCream("Sundae", 1, false).penultimate)
  }

  def caseClassMigrations = {

    case class IceCreamV1(name: String, numCherries: Int, inCone: Boolean)

    // Remove fields:
    case class IceCreamV2a(name: String, inCone: Boolean)

    // Reorder fields:
    case class IceCreamV2b(name: String, inCone: Boolean, numCherries: Int)

    // Insert fields (provided we can determine a default value):
    case class IceCreamV2c(name: String, inCone: Boolean, numCherries: Int, numWaffles: Int)

    trait Migration[A, B] {
      def apply(a: A): B
    }

    implicit class MigrationOps[A](a: A) {
      def migrateTo[B](implicit migration: Migration[A, B]): B =
        migration.apply(a)
    }

    import shapeless._
    import shapeless.ops.hlist
    //    implicit def genericMigration[A, B, ARepr <: HList, BRepr <: HList](implicit
    //                                                                        aGen: LabelledGeneric.Aux[A, ARepr],
    //                                                                        bGen: LabelledGeneric.Aux[B, BRepr],
    //                                                                        inter: hlist.Intersection.Aux[ARepr, BRepr, BRepr]): Migration[A, B] = new Migration[A, B] {
    //      def apply(a: A): B =
    //        bGen.from(inter.apply(aGen.to(a)))
    //    }

    // Take a moment to locate Intersection in the shapeless codebase. Its Aux type alias takes three parameters: two
    // input HLists and one output for the intersec􏰀tion type. In the example above we are specifying ARepr and BRepr as
    // the input types and BRepr as the output type. This means implicit resolu􏰀tion will only succeed if B has an exact
    // subset of the fields of A, specified with the exact same names in the same order:
    //println("Intersection migration --> " + IceCreamV1("Sundae", 1, true).migrateTo[IceCreamV2a])

    //error
    //IceCreamV1("Sundae", 1, true).migrateTo[IceCreamV2b]


    //    implicit def genericMigration[
    //    A, B,
    //    ARepr <: HList, BRepr <: HList,
    //    Unaligned <: HList
    //    ](
    //       implicit
    //       aGen: LabelledGeneric.Aux[A, ARepr],
    //       bGen: LabelledGeneric.Aux[B, BRepr],
    //       inter: hlist.Intersection.Aux[ARepr, BRepr, Unaligned],
    //       align: hlist.Align[Unaligned, BRepr]
    //     ): Migration[A, B] = new Migration[A, B] {
    //      def apply(a: A): B =
    //        bGen.from(align.apply(inter.apply(aGen.to(a))))
    //    }
    // We introduce a new type parameter called Unaligned to represent the intersecti􏰀on of ARepr and BRepr before
    // alignment, and use Align to convert Unaligned to BRepr. With this modified definiti􏰀on of Migration we can both
    // remove and reorder fields:
    //println("Reordering 1 --> " + IceCreamV1("Sundae", 1, true).migrateTo[IceCreamV2a])
    //println("Reordering 2 --> " + IceCreamV1("Sundae", 1, true).migrateTo[IceCreamV2b])

    //error
    //IceCreamV1("Sundae", 1, true).migrateTo[IceCreamV2c]

    // We need a mechanism for calculati􏰀ng default values to support the additi􏰀on of new fields. Shapeless doesn’t provide
    // a type class for this, but Cats does in the form of a Monoid. Monoid defines two opera􏰀tions: empty for creati􏰀ng a
    // “zero” value and combine for “adding” two values. We only need empty in our code, but it will be trivial to define
    // combine as well. Cats provides instances of Monoid for all the primiti􏰀ve types we care about (Int, Double, Boolean,
    // and String). We can define instances for HNil and ::.
    import cats.Monoid
    import cats.instances.all._
    import shapeless.labelled.{field, FieldType}
    def createMonoid[A](zero: A)(add: (A, A) => A): Monoid[A] =
      new Monoid[A] {
        def empty = zero

        def combine(x: A, y: A): A = add(x, y)
      }

    implicit val hnilMonoid: Monoid[HNil] =
      createMonoid[HNil](HNil)((x, y) => HNil)

    implicit def emptyHList[K <: Symbol, H, T <: HList](
                                                         implicit
                                                         hMonoid: Lazy[Monoid[H]],
                                                         tMonoid: Monoid[T]
                                                       ): Monoid[FieldType[K, H] :: T] =
      createMonoid(field[K](hMonoid.value.empty) :: tMonoid.empty) {
        (x, y) =>
          field[K](hMonoid.value.combine(x.head, y.head)) :: tMonoid.combine(x.tail, y.tail)
      }

    implicit def genericMigration[
    A, B, ARepr <: HList, BRepr <: HList,
    Common <: HList, Added <: HList, Unaligned <: HList
    ](
       implicit
       aGen: LabelledGeneric.Aux[A, ARepr],
       bGen: LabelledGeneric.Aux[B, BRepr],
       inter: hlist.Intersection.Aux[ARepr, BRepr, Common],
       diff: hlist.Diff.Aux[BRepr, Common, Added],
       monoid: Monoid[Added],
       prepend: hlist.Prepend.Aux[Added, Common, Unaligned], align: hlist.Align[Unaligned, BRepr]
     ): Migration[A, B] =
      new Migration[A, B] {
        def apply(a: A): B =
          bGen.from(align(prepend(monoid.empty, inter(aGen.to(a)))))
      }

    // Note that this code doesn’t use every type class at the value level. We use Diff to calculate the Added data
    // type, but we don’t actually need diff.apply at run 􏰀me. Instead we use our Monoid to summon an instance of Added.
    println("Final Remove fields--> " + IceCreamV1("Sundae", 1, true).migrateTo[IceCreamV2a])
    println("Final Reorder fields --> " + IceCreamV1("Sundae", 1, true).migrateTo[IceCreamV2b])
    println("Final Insert fields --> " + IceCreamV1("Sundae", 1, true).migrateTo[IceCreamV2c])
  }

  def main(args: Array[String]): Unit = {
    simpleOpsExamples
    creatingCustomOp
    caseClassMigrations
  }
}

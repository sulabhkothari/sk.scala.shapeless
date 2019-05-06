object RecordOpsExamples {
  import shapeless._
  def main(args: Array[String]): Unit = {
    import record._
    import ops.record._
    import syntax.singleton._

    case class Book(author: String, title: String, id: Int, price: Double)
    case class ExtendedBook(author: String, title: String, id: Int, price: Double, inPrint: Boolean)

    val bookGen = LabelledGeneric[Book]
    val bookExtGen = LabelledGeneric[ExtendedBook]

    val tapl = Book("Benjamin Pierce", "Types and Programming Languages", 262162091, 44.11)

    val rec = bookGen.to(tapl)

    // Read price field
    val currentPrice = rec(Symbol("price"))  // Static type is Double
    println("Current price is "+currentPrice)
    println

    // Update price field, relying on static type of currentPrice
    val updated = bookGen.from(rec.updateWith(Symbol("price"))(_+2.0))
    println(updated)

    import shapeless._
    case class IceCream(name: String, numCherries: Int, inCone: Boolean)
    val sundae = LabelledGeneric[IceCream].
      to(IceCream("Sundae", 1, false))

    println("Record Ops get field value --> "+sundae.get('name))

    //error
    //sundae.get('nomCherries)

    println("Record Ops update field value --> "+sundae.updated('numCherries, 3))

    val removed = sundae.remove('inCone)
    println("Removed value --> " + removed._1)
    println("Instance after removal --> " + removed._2)

    // The updateWith method and Modifier type class allow us to modify a field with an update func􏰀on:
    println("updateWith method and Modifier type class --> " + sundae.updateWith('name)("MASSIVE " + _))

    println("Conversion to Map --> "+sundae.toMap)

    // There are other record ops that we don’t have room to cover here. We can rename fields, merge records, map over
    // their values, and much more. See the source code of shapeless.ops.record and shapeless.syntax.record
    // for more informati􏰀on.
  }
}

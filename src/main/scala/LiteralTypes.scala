package typelevel

object LiteralTypesInt:

  import scala.compiletime.ops.int.{+, -}

  val one: 1 = 1
  val two: 2 = 2
  // val three: 2 = 3

  val test: two.type - one.type = one

object LiteralTypesString:

  import scala.compiletime.ops.string.{+, CharAt, Length, Substring}

  val foo: "foo" = "foo"
  // val bar: "bar" = "baz"
  val baz : "baz" = "baz"

  val foobaz: foo.type + baz.type = "foobaz"
  val char: CharAt[foobaz.type, 1] = 'o'
  val substr: Substring[foobaz.type, 3, Length[foobaz.type]] = "baz"

object LiteralTypesLogic:

  import scala.compiletime.ops.boolean.&&

  val localTrue: true = true
  val localFalse: false = false
  val and: localTrue.type && localFalse.type = false
  summon[(false && true) =:= false]
  summon[(true && true) =:= true]

object MatchTypes:

  type FirstElem[X] = X match
    case String => Char
    case Array[t] => t
    case Iterable[t] => t
    case AnyVal => X

  // Dependant typing
  def firstElem[X](x: X): FirstElem[X] = x match
    case x: String      => x.charAt(0)
    case x: Array[t]    => x(0)
    case x: Iterable[t] => x.head
    case x: AnyVal      => x

  firstElem("foo"): Char // 'f'
  firstElem(List("one", "two")): String // "one"

object RecursiveMatchTypes:

  type LeafElem[X] = X match
    case String => Char
    case Array[t] => LeafElem[t]
    case Iterable[t] => LeafElem[t]
    case AnyVal => X

  def leafElem[X](x: X): LeafElem[X] = x match
    case x: String      => x.charAt(0)
    case x: Array[t]    => leafElem(x(0))
    case x: Iterable[t] => leafElem(x.head)
    case x: AnyVal      => x

  leafElem("foo"): Char // 'f'
  leafElem(Vector(List("one", "two"))): Char // 'o'
  leafElem(Array(3, 2)): Int // 3

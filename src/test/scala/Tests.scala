import scala.reflect.runtime.{universe => ru}
import quasiquotes._

class BasicTests extends QQSuite {

  implicit val universe: ru.type = ru
  import ru._

  val xplusy = { val x = 1; val y = 2; reify(x + y) }.tree
  val classxy = reify { class x extends AnyRef { def y = null } }.tree

  test("without arguments") {
    assert(xplusy ≈ {
      q"x + y"
    })
    assert(classxy ≈ {
      q"{ class x extends AnyRef { def y = null } }"
    })
  }

  test("insert tree by itself") {
    assert(xplusy ≈ {
      val $u = ru
      val sum = q"x + y"
      q"$sum"
    })
  }

  test("insert trees into if expression") {
    assert({
      val x = 1
      val y = 2
      reify { if(x > y) x - y else x + y }.tree
    } ≈ {
      val xplusy = q"x + y"
      val xminusy = q"x - y"
      val xgty = q"x > y"
      q"if($xgty) $xminusy else $xplusy"
    })
  }

  test("insert method") {
    assert(classxy ≈ {
      val y = q"def y = null"
      q"{ class x extends AnyRef { $y } }"
    })
  }

  test("insert term name into val") {
    assert({
      reify { val x = 1 }.tree
    } ≈ {
      val xtermname = newTermName("x")
      q"{ val $xtermname = 1 }"
    })
  }

  test("insert term name into assign") {
    assert(Assign(Ident(newTermName("x")), Literal(Constant(1))) ≈ {
      val xtermname = newTermName("x")
      q"{ $xtermname = 1 }"
    })
  }

  test("insert type name into class") {
    assert({
      reify { class x extends AnyRef }.tree
    } ≈ {
      val xname = newTypeName("x")
      q"{ class $xname extends AnyRef }"
    })
  }

  test("insert type name into typedef") {
    assert({
      reify { type T = Int }.tree
    } ≈ {
      val tname = newTypeName("T")
      q"{ type $tname = Int }"
    })
  }
}
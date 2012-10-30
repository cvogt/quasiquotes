import org.scalatest.FunSuite

import scala.reflect.runtime.{universe => u}
import u._
import quasiquotes._

class Tests extends FunSuite with Similar {

  test("tree insertion") {
    val xplusy = { val x = 1; val y = 2; reify(x + y).tree }
    assert(xplusy ≈ {
      q"x + y"
    })
    assert(xplusy ≈ {
      val sum = q"x + y"
      q"$sum"
    })

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

    val classxy = reify { class x { def y = null } }.tree
    assert(classxy ≈ {
      q"{ class x { def y = null } }"
    })
    assert(classxy ≈ {
      val y = q"def y = null"
      q"{ class x { $y } }"

    })
  }

  test("term name insertion") {
    assert({
      reify { val x = 1 }.tree
    } ≈ {
      val xtermname = newTermName("x")
      q"{ val $xtermname = 1 }"
    })

    assert(Assign(Ident(newTermName("x")), Literal(Constant(1))) ≈ {
      val xtermname = newTermName("x")
      q"{ $xtermname = 1 }"
    })
  }

  test("type name insertion") {
    assert({
      reify { class x }.tree
    } ≈ {
      val xname = newTypeName("x")
      q"{ class $xname }"
    })
  }
}

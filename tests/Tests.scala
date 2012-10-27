import scala.reflect.runtime.{universe => u}
import u._
import quasiquotes._

object Tests extends App with Similar {
  val xplusy = { val x = 1; val y = 2; reify(x + y).tree }
  xplusy ≈ {
    q"x + y"
  }
  xplusy ≈ {
    val sum = q"x + y"
    q"$sum"
  }

  {
    val x = 1
    val y = 2
    reify { if(x > y) x - y else x + y }.tree
  } ≈ {
    val xplusy = q"x + y"
    val xminusy = q"x - y"
    val xgty = q"x > y"
    q"if($xgty) $xminusy else $xplusy"
  }

  val classxy = reify { class x { def y = null } }.tree
  classxy ≈ {
    q"{ class x { def y = null } }"
  }
  classxy ≈ {
    val y = q"def y = null"
    q"{ class x { $y } }"
  }

  {
    reify { val x = 1 }.tree
  } ≈ {
    val xtermname = newTermName("x")
    q"{ val $xtermname = 1 }"
  }

  Assign(Ident(newTermName("x")), Literal(Constant(1))) ≈ {
    val xtermname = newTermName("x")
    q"{ $xtermname = 1 }"
  }

  {
    reify { class x }.tree
  } ≈ {
    val xname = newTypeName("x")
    q"{ class $xname }"
  }
}

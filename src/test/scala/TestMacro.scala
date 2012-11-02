import scala.language.experimental.{macros => macrosFeature}
import scala.reflect.macros
import quasiquotes._

object TestSimpleMacro {
  def apply(arg: Any) = macro impl
  def impl(ctx: macros.Context)(arg: ctx.Expr[Any]): ctx.Expr[Any] = {
    implicit val universe: ctx.universe.type = ctx.universe
    ctx.Expr(q"10 + 20")
  }
}

import scala.language.experimental.{macros => macrosFeature}
import scala.reflect.{api => reflect, macros, runtime}

object quasiquotes {

  implicit class QuasiQuote(ctx: StringContext) {
    object q {
      def apply(args: Any*) = macro Impl.apply
    }
  }

  private object Impl {
    def apply(c: macros.Context)(args: c.Expr[Any]*): c.Expr[Any] = {
      import c.universe._

      val parts = c.prefix.tree match {
	case Select(Apply(_, List(Apply(_, args))), _) =>
	  args.map(_ match { case Literal(Constant(s: String)) => s })
      }
      
      require(parts.length == 1)
      println(c.parse(parts(0)))
      //val universeTree = treeBuild.mkRuntimeUniverseRef
      //val mirrorTree = treeBuild.mkAttributedSelect(universeTree, universeTree.tpe.member("rootMirror": TermName))
      //c.Expr(c.reifyTree(universeTree, mirrorTree, tree))
      reify(null)
    }
  }
}

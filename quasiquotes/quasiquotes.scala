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
	case Select(qq, _) => qq match {
	  case Apply(_, qqargs) => qqargs match {
	    case List(stringctx) => stringctx match {
	      case Apply(_, args) =>
		args.map(_ match {
		  case Literal(l) => l match {
		    case Constant(s: String) => s
		  }
		})	    
	    }
	  }
	}
      }
      require(parts.length == 1)
      val tree = c.parse(parts(0))
      val universeTree = reify(scala.reflect.runtime.universe).tree
      val mirrorTree = reify(scala.reflect.runtime.universe.rootMirror).tree
      //c.Expr(c.reifyTree(universeTree, mirrorTree, tree))
      reify(null)
    }
  }
}

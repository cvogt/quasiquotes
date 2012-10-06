import scala.language.experimental.{macros => macrosFeature}
import scala.reflect.macros

object quasiquotes {
  implicit class QuasiQuote(ctx: StringContext) {
    object q {
      def apply(args: Any*) = macro Impl.apply
    }
  }
}

private object Impl {
  def apply(c: macros.Context)(args: c.Expr[Any]*): c.Expr[Any] = {
    val ext = new { val ctx: c.type = c } with Ext
    import ext.{ctx => _, _}
    import c.universe._

    val parts = extractStringContextParts
    require(parts.length == 1)
    val tree = c.parse(parts(0))
    val reifiedtree = reifyTree(tree)

    println(s"tree = $tree")
    println(s"raw tree = ${showRaw(tree)}")
    println(s"reified tree = $reifiedtree")
    println(s"reified raw tree = ${showRaw(reifiedtree)}")

    c.Expr(reifiedtree)
  }
}

private abstract class Ext {
  val ctx: macros.Context
  import ctx.universe._

  def extractStringContextParts =
    ctx.prefix.tree match {
      case Select(Apply(_, List(Apply(_, args))), _) =>
        args.map(_ match { case Literal(Constant(s: String)) => s })
    }

  object nme {
    val Literal = "Literal"
    val Import = "Import"
    val UNIVERSE_PREFIX = "scala.reflect.runtime.universe"
  }

  def reifyAny(reifee: Any): Tree = reifee match {
    //case sym: Symbol              => reifySymRef(sym)
    //case tpe: Type                => reifyType(tpe)
    case name: Name               => reifyName(name)
    case tree: Tree               => reifyTree(tree)
    //case pos: Position            => reifyPosition(pos)
    //case mods: global.Modifiers   => reifyModifiers(mods)
    case xs: List[_]              => reifyList(xs)
    case s: String                => Literal(Constant(s))
    case v if isAnyVal(v)         => Literal(Constant(v))
    case null                     => Literal(Constant(null))
    case _                        =>
      throw new Error(s"reifee $reifee of type ${reifee.getClass} is not supported")
  }

  def reifyName(name: Name) = {
    val factory =
      if (name.isTypeName)
        "newTypeName"
      else
        "newTermName"
    mirrorCall(factory, Literal(Constant(name.toString)))
  }

  def reifyList(xs: List[Any]): Tree =
    mkList(xs.map(reifyAny))

  def reifyTree(tree: Tree): Tree =
    tree match {
      case EmptyTree =>
        reifyMirrorObject(EmptyTree)
      case Literal(const @ Constant(_)) =>
        mirrorCall(nme.Literal, reifyProduct(const.asInstanceOf[Product]))
      case Import(expr, selectors) =>
        mirrorCall(null, null)
      case _ =>
        reifyProduct(tree.asInstanceOf[Product])
    }

  def reifyMirrorObject(name: String): Tree =
    mirrorSelect(name)

  def reifyMirrorObject(x: Product): Tree =
    reifyMirrorObject(x.productPrefix)

  def termPath(fullname: String): Tree = {
    val parts = fullname split "\\."
    val prefixParts = parts.init
    val lastName = newTermName(parts.last)
    if (prefixParts.isEmpty) Ident(lastName)
    else {
      val prefixTree = ((Ident(prefixParts.head): Tree) /: prefixParts.tail)(Select(_, _))
      Select(prefixTree, lastName)
    }
  }

  def reifyProduct(x: Product): Tree = {
    val prefix = x.productPrefix
    val elements = x.productIterator.toList
    println(elements)
    if (prefix.startsWith("Tuple"))
      scalaFactoryCall(prefix, elements.map(reifyAny).toList: _*)
    else
      mirrorCall(prefix, elements.map(reifyAny): _*)
  }

  def call(fname: String, args: Tree*): Tree =
    Apply(termPath(fname), args.toList)

  def scalaFactoryCall(name: String, args: Tree*): Tree =
    call(s"scala.$name.apply", args: _*)

  def mirrorCall(name: TermName, args: Tree*): Tree =
    call(s"${nme.UNIVERSE_PREFIX}.${name.toString}", args: _*)

  def mirrorCall(name: String, args: Tree*): Tree =
    call(s"${nme.UNIVERSE_PREFIX}.$name", args: _*)

  def mirrorSelect(name: String): Tree =
    termPath(s"${nme.UNIVERSE_PREFIX}.$name")

  def mkList(args: List[Tree]): Tree =
    scalaFactoryCall("collection.immutable.List", args: _*)

  def isAnyVal(x: Any) = x match {
    case _: Byte | _: Short | _: Char | _: Int | _: Long | _: Float | _: Double | _: Boolean | _: Unit => true
    case _                                                                                             => false
  }
}

import scala.language.experimental.{macros => macrosFeature}
import scala.reflect.{api => reflect}
import scala.reflect.macros
import scala.collection.mutable

// TODO: check incoming data
// TODO: which trees are special-cased?


object quasiquotes {
  implicit class QuasiQuote(ctx: StringContext) {
    object q {
      def apply(args0: Any*)(implicit universe0: reflect.Universe) =
        macro StaticImpl.apply
    }
  }
}

private object StaticImpl {
  def apply(c: macros.Context)(args0: c.Expr[Any]*)(universe0: c.Expr[reflect.Universe]): c.Expr[Any] = {
    val impl = new {
      val ctx: c.type = c
      val args = args0
      val universe = universe0
    } with Impl
    c.Expr(impl.result)
  }
}

private abstract class Impl {

  val ctx: macros.Context
  val args: Seq[ctx.Expr[Any]]
  val universe: ctx.Expr[reflect.Universe]
  import ctx.universe._

  def memberType(thistype: Type, name: String): Type = {
    val sym = thistype.typeSymbol.typeSignature.member(newTypeName(name))
    sym.asType.toType.typeConstructor.asSeenFrom(thistype, sym.owner)
  }

  val universeType = universe.actualType

  val termNameType = memberType(universeType, "TermName")
  val typeNameType = memberType(universeType, "TypeName")
  val treeType = memberType(universeType, "Tree")
  /*println("--- Universe Types")
  println(showRaw(termNameType, printIds=true))
  println(showRaw(typeNameType, printIds=true))
  println(showRaw(treeType, printIds=true))*/

  val qqprefix = "$quasiquote$"
  val qquniverse = "$u"
  val qqmirror = "$m"

  val parts =
    ctx.prefix.tree match {
      case Select(Apply(_, List(Apply(_, args))), _) =>
        args.map(_ match { case Literal(Constant(s: String)) => s })
    }

  //println("\n--- Parts Types")
  val (code, subsmap) = {
    val sb = new StringBuilder(parts.head)
    val subsmap = mutable.Map[String, Expr[Any]]()
    for((arg, part) <- args.zip(parts.tail)) {
      val placeholder = ctx.fresh(qqprefix)
      sb.append(placeholder)
      sb.append(part)
      subsmap(placeholder) = arg
      //println(s"$placeholder -> ${showRaw(arg)} :: ${showRaw(arg.actualType, printIds=true)}")
    }
    (sb.toString, subsmap)
  }

  val tree = ctx.parse(code)
  val reified = reifyTree(tree)
  val result = wrap(reified)

  /*println()
  println("parsed code")
  println(s"= ${tree}")
  println(s"= ${showRaw(tree)}")
  println()
  println("reifiedtree")
  println(s"= ${reified}")
  println(s"= ${showRaw(reified)}")
  println()
  println("result")
  println(s"= ${result}")
  println(s"= ${showRaw(result)}")
  println()*/

  def wrap(t: Tree) = {
    Block(
      List(ValDef(Modifiers(), newTermName(qquniverse), TypeTree(), universe.tree)),
      t)
  }

  def reifyTree(tree: Tree): Tree = tree match {
    case Ident(name) if subsmap.contains(name.encoded) && subsmap(name.encoded).actualType <:< treeType =>
      subsmap(name.encoded).tree
    //case emptyValDef =>
    //  mirrorBuildSelect("emptyValDef")
    case EmptyTree =>
      reifyMirrorObject(EmptyTree)
    case Literal(const @ Constant(_)) =>
      mirrorCall("Literal", reifyProduct(const.asInstanceOf[Product]))
    case Import(expr, selectors) =>
      val args = mkList(selectors.map(s => reifyProduct(s.asInstanceOf[Product])))
      mirrorCall("Import", reifyAny(expr), args)
    case _ =>
      reifyProduct(tree.asInstanceOf[Product])
  }

  def reifyAny(reifee: Any): Tree = reifee match {
    case name: Name               => reifyName(name)
    case tree: Tree               => reifyTree(tree)
    case mods: Modifiers          => reifyModifiers(mods)
    case xs: List[_]              => reifyList(xs)
    case s: String                => Literal(Constant(s))
    case v if isAnyVal(v)         => Literal(Constant(v))
    case null                     => Literal(Constant(null))
    case _                        =>
      throw new Error(s"reifee $reifee of type ${reifee.getClass} is not supported")
  }

  def reifyModifiers(m: Modifiers) =
    mirrorFactoryCall("Modifiers", mirrorBuildCall("flagsFromBits", reifyAny(m.flags)), reifyAny(m.privateWithin), reifyAny(m.annotations))

  def reifyName(name: Name) = {
    if(!subsmap.contains(name.decoded)) {
      val factory =
        if (name.isTypeName)
          "newTypeName"
        else
          "newTermName"
      mirrorCall(factory, Literal(Constant(name.toString)))
    } else {
      subsmap(name.decoded).tree
    }
  }

  def reifyList(xs: List[Any]): Tree =
    mkList(xs.map(reifyAny))

  def reifyMirrorObject(name: String): Tree =
    mirrorSelect(name)

  def reifyMirrorObject(x: Product): Tree =
    reifyMirrorObject(x.productPrefix)

  def reifyProduct(x: Product): Tree = {
    val prefix = x.productPrefix
    val elements = x.productIterator.toList
    if (prefix.startsWith("Tuple"))
      scalaFactoryCall(prefix, elements.map(reifyAny).toList: _*)
    else
      mirrorCall(prefix, elements.map(reifyAny): _*)
  }

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

  def call(fname: String, args: Tree*): Tree =
    Apply(termPath(fname), args.toList)

  def scalaFactoryCall(name: String, args: Tree*): Tree =
    call(s"scala.$name.apply", args: _*)

  def mirrorCall(name: String, args: Tree*): Tree =
    call(s"$qquniverse.$name", args: _*)

  def mirrorSelect(name: String): Tree =
    termPath(s"$qquniverse.$name")

  def mirrorBuildSelect(name: String): Tree =
    termPath(s"$qquniverse.build.$name")

  def mirrorFactoryCall(value: Product, args: Tree*): Tree =
    mirrorFactoryCall(value.productPrefix, args: _*)

  def mirrorFactoryCall(prefix: String, args: Tree*): Tree =
    mirrorCall(prefix, args: _*)

  def mirrorBuildCall(name: String, args: Tree*): Tree =
    call(s"$qquniverse.build.$name", args: _*)

  def mkList(args: List[Tree]): Tree =
    scalaFactoryCall("collection.immutable.List", args: _*)

  def isAnyVal(x: Any) = x match {
    case _: Byte | _: Short | _: Char | _: Int | _: Long | _: Float | _: Double | _: Boolean | _: Unit => true
    case _                                                                                             => false
  }
}

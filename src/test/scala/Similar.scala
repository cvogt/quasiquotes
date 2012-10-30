import scala.reflect.runtime.{universe => u}
import u._

trait Similar {

  implicit class TestSimilar(tree1: Tree) {
    def ≈(tree2: Tree) =
      if(!similar(tree1, tree2))
        Some(showRaw(tree1) + "\n=/=\n" + showRaw(tree2))
      else
        None
  }

  def similar(const1: Constant, const2: Constant) = const1.value == const2.value

  def similar(i1: Int, i2: Int) = i1 == i2

  def similar(name1: Name, name2: Name): Boolean = name1 == name2

  def similar(l1: List[Tree], l2: List[Tree]): Boolean =
    (l1.length == l2.length) && l1.zip(l2).forall(pair => similar(pair._1, pair._2))

  def similar1(ll1: List[List[Tree]], ll2: List[List[Tree]]): Boolean =
    (ll1.length == ll2.length) && ll1.zip(ll2).forall(pair => similar(pair._1, pair._2))

  def similar2(l1: List[ImportSelector], l2: List[ImportSelector]) = false //FIXME

  def similar(mod1: Modifiers, mod2: Modifiers) = mod1 == mod2

  def similar(t1: Tree, t2: Tree): Boolean = {
    val res = (t1, t2) match {
      case (Alternative(trees1), Alternative(trees2)) =>
        similar(trees1, trees2)
      case (Annotated(annot1, arg1), Annotated(annot2, arg2)) =>
        similar(annot1, annot2) && similar(arg1, arg2)
      case (AppliedTypeTree(tpt1, args1), AppliedTypeTree(tpt2, args2)) =>
        similar(tpt1, tpt2) && similar(args1, args2)
      case (Apply(fun1, args1), Apply(fun2, args2)) =>
        similar(fun1, fun2) && similar(args1, args2)
      case (Assign(lhs1, rhs1), Assign(lhs2, rhs2)) =>
        similar(lhs1, lhs2) && similar(rhs1, rhs2)
      case (AssignOrNamedArg(lhs1, rhs1), AssignOrNamedArg(lhs2, rhs2)) =>
        similar(lhs1, lhs2) && similar(rhs1, rhs2)
      case (Bind(name1, body1), Bind(name2, body2)) =>
        similar(name1, name2) && similar(body1, body2)
      case (Block(stats1, expr1), Block(stats2, expr2)) =>
        similar(stats1, stats2) && similar(expr1, expr2)
      case (CaseDef(pat1, guard1, body1), CaseDef(pat2, guard2, body2)) =>
        similar(pat1, pat2) && similar(guard1, guard2) && similar(body1, body2)
      case (ClassDef(mods1, name1, tparams1, impl1),
            ClassDef(mods2, name2, tparams2, impl2)) =>
        similar(mods1, mods2) && similar(name1, name2) &&
        similar(tparams1, tparams2) && similar(impl1, impl2)
      case (CompoundTypeTree(templ1), CompoundTypeTree(templ2)) =>
        similar(templ1, templ2)
      case (DefDef(mods1, name1, tparams1, vparamss1, tpt1, rhs1),
            DefDef(mods2, name2, tparams2, vparamss2, tpt2, rhs2)) =>
        similar(mods1, mods2) && similar(name1, name2) &&
        similar(tparams1, tparams2) && similar1(vparamss1, vparamss2) &&
        similar(tpt1, tpt2) && similar(rhs1, rhs2)
      case (ExistentialTypeTree(tpt1, where1), ExistentialTypeTree(tpt2, where2)) =>
        similar(tpt1, tpt2) && similar(where1, where2)
      case (ValDef(mods1, name1, tpt1, rhs1), ValDef(mods2, name2, tpt2, rhs2)) =>
        similar(mods1, mods2) && similar(name1, name2) && similar(tpt1, tpt2) && similar(rhs1, rhs2)
      case (UnApply(fun1, args1), UnApply(fun2, args2)) =>
        similar(fun1, fun2) && similar(args1, args2)
      case (Typed(expr1, _), Typed(expr2, _)) =>
        similar(expr1, expr2)
      case (Function(vparams1, body1), Function(vparams2, body2)) =>
        similar(vparams1, vparams2) && similar(body1, body2)
      case (Ident(name1), Ident(name2)) =>
        similar(name1, name2)
      case (If(cond1, then1, else1), If(cond2, then2, else2)) =>
        similar(cond1, cond2) && similar(then1, then2) && similar(else1, else2)
      case (Import(expr1, sel1), Import(expr2, sel2)) =>
        similar(expr1, expr2) && similar2(sel1, sel2)
      case (ImportSelector(name1, namepos1, rename1, renamepos1),
            ImportSelector(name2, namepos2, rename2, renamepos2)) =>
        similar(name1, name2) && similar(namepos1, namepos2) &&
        similar(rename1, rename2) && similar(renamepos1, renamepos2)
      case (LabelDef(name1, params1, rhs1), LabelDef(name2, params2, rhs2)) =>
        similar(name1, name2) && similar(params1, params2) && similar(rhs1, rhs2)
      case (Literal(const1), Literal(const2)) =>
        similar(const1, const2)
      case (Match(sel1, cases1), Match(sel2, cases2)) =>
        similar(sel1, sel2) && similar(cases1, cases2)
      case (ModuleDef(mods1, name1, impl1), ModuleDef(mods2, name2, impl2)) =>
        similar(mods1, mods2) && similar(name1, name2) && similar(impl1, impl2)
      case (New(tpt1), New(tpt2)) =>
        similar(tpt1, tpt2)
      case (PackageDef(pid1, stats1), PackageDef(pid2, stats2)) =>
        similar(pid1, pid2) && similar(stats1, stats2)
      case (ReferenceToBoxed(ident1), ReferenceToBoxed(ident2)) =>
        similar(ident1, ident2)
      case (Return(expr1), Return(expr2)) =>
        similar(expr1, expr2)
      case (Select(qual1, name1), Select(qual2, name2)) =>
        similar(qual1, qual2) && similar(name1, name2)
      case (SelectFromTypeTree(qual1, name1), SelectFromTypeTree(qual2, name2)) =>
        similar(qual1, qual2) && similar(name1, name2)
      case (SingletonTypeTree(ref1), SingletonTypeTree(ref2)) =>
        similar(ref1, ref2)
      case (Star(el1), Star(el2)) =>
        similar(el1, el2)
      case (Super(qual1, mix1), Super(qual2, mix2)) =>
        similar(qual1, qual2) && similar(mix1, mix2)
      case (Template(_, _, body1), Template(_, _, body2)) =>
        similar(body1, body2)
      case (This(qual1), This(qual2)) =>
        similar(qual1, qual2)
      case (Throw(expr1), Throw(expr2)) =>
        similar(expr1, expr2)
      case (Try(block1, catches1, finalizer1), Try(block2, catches2, finalizer2)) =>
        similar(block1, block2) && similar(catches1, catches2) && similar(finalizer1, finalizer2)
      case (TypeApply(fun1, args1), TypeApply(fun2, args2)) =>
        similar(fun1, fun2) && similar(args1, args2)
      case (TypeBoundsTree(lo1, hi1), TypeBoundsTree(lo2, hi2)) =>
        similar(lo1, lo2) && similar(hi1, hi2)
      case (TypeTree(), TypeTree()) => true
      case (EmptyTree, EmptyTree) => true
      case _ => false
    }
    //if(!res) println(showRaw(t1) + "=/=" + showRaw(t2))
    res
  }
}
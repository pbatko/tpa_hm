package hm


object HmInference {

  sealed trait TypeTerm

  case class TypeVar(name: String) extends TypeTerm {
    override def toString: String = s"$name"
  }

  case class TypeApp(name: String, typeTerms: Seq[TypeTerm] = Seq.empty) extends TypeTerm {
    override def toString: String = {
      if (typeTerms.isEmpty) {
        name
      } else {
        s"$name(${typeTerms.mkString(", ")})"
      }.capitalize
    }
  }

  /**
   * @return true if t1 occurs in t2
   */
  def occurs(t1: TypeTerm, t2: TypeTerm): Boolean = {
    t2 match {
      case TypeApp(_, typeTerms) =>
        typeTerms.contains(t1) || typeTerms.exists {
          typeTerm =>
            occurs(t1, typeTerm)
        }
      case _ => false
    }
  }

  case class Substitution(typeVar: TypeVar, typeTerm: TypeTerm) {
    override def toString: String = s"[$typeTerm / $typeVar]"
  }


  def substitute(substitutions: Seq[Substitution], t: TypeTerm): TypeTerm = {
    t match {
      case tv: TypeVar =>
        substitutions match {
          case Substitution(v1, t1) :: xs =>
            if (v1 == tv) {
              t1
            } else {
              substitute(xs, tv)
            }
          case ss if ss.isEmpty => t
        }
      case TypeApp(name, args) =>
        val nArgs = args.map(substitute(substitutions, _))
        TypeApp(name, nArgs)
    }
  }


  def compose(ls: Seq[Substitution], rs: Seq[Substitution]): Seq[Substitution] = {
    rs match {
      case rs_h :: rs_t =>
        val newLs = rs_h +: ls.map {
          case Substitution(v, t) =>
            val newT = substitute(Seq(rs_h), t)
            Substitution(v, newT)
        }
        compose(newLs, rs_t)
      case _ if rs.isEmpty =>
        ls
    }
  }


  def unify(t1: TypeTerm, t2: TypeTerm): Seq[Substitution] = {
    println(s"$t1 || $t2")
    (t1, t2) match {
      case (x@TypeVar(n1), y@TypeVar(n2)) =>
        if (n1 == n2) {
          Seq.empty
        } else {
          Seq(Substitution(y, x))
        }
      case (x: TypeVar, TypeApp(f, args)) if !occurs(x, t2) => Seq(Substitution(x, t2))
      case (TypeApp(f, args), x: TypeVar) if !occurs(x, t1) => Seq(Substitution(x, t1))
      case _ if t1 == t2 => Seq.empty //two equal typeApps
      case (TypeApp(f, args1), TypeApp(g, args2)) if f == g && args1.size == args2.size =>
        val u1 = unify(args1.head, args2.head)
        val nArgs1 = args1.tail.map(substitute(u1, _))
        val nArgs2 = args2.tail.map(substitute(u1, _))
        val u2 = unify(TypeApp(f, nArgs1), TypeApp(f, nArgs2))
        val u = compose(u1, u2)
        u
//      case o =>
//        ???
    }

  }


  sealed trait TypeScheme

  case class Forall(name: String, typeScheme: TypeScheme) extends TypeScheme {
    override def toString: String = s"∀$name.$typeScheme"
  }

  case class Type(typeTerm: TypeTerm) extends TypeScheme {
    override def toString: String = typeTerm.toString
  }

  //fbtsvs
  def freeAndBoundTypeVariables(typeScheme: TypeScheme)(free: Seq[TypeVar], bound: Seq[TypeVar]): (Seq[TypeVar], Seq[TypeVar]) = {
    typeScheme match {
      case Forall(name, scheme) => freeAndBoundTypeVariables(scheme)(free, TypeVar(name) +: bound)
      case Type(typeTerm) => freeAndBoundTypeVariables(typeTerm)(free, bound)
    }
  }

  //fbtyvars
  def freeAndBoundTypeVariables(typeTerm: TypeTerm)(free: Seq[TypeVar], bound: Seq[TypeVar]): (Seq[TypeVar], Seq[TypeVar]) = {
    typeTerm match {
      case tv: TypeVar => if (bound.contains(tv)) (free, bound) else (tv +: free, bound)
      case TypeApp(name, terms) =>
        terms match {
          case h_term :: t_terms =>
            val (newFree, newBound) = freeAndBoundTypeVariables(h_term)(free, bound)
            freeAndBoundTypeVariables(TypeApp(name, t_terms))(newFree, newBound)
          case _ if terms.isEmpty =>
            (free, bound)
        }
    }
  }

  //tyvars
  def allTypeVariables(typeTerm: TypeTerm): Seq[TypeVar] = {
    val (f, b) = freeAndBoundTypeVariables(typeTerm)(Seq.empty, Seq.empty)
    f ++ b
  }

  //varno
  def variableNumber(name: String): Int = {
    val letter = name.toSeq.head
    val ticks = name.toSeq.tail
    (letter.toInt - 'a'.toInt) + ticks.size * 26
  }


  private def createVariable(varNum: Int): String = {
    val ticks = "'" * (varNum / 26)
    (varNum % 26 + 'a'.toInt).toChar.toString + ticks
  }

  //newvar
  def newVariable(prevVarNumber: Int): (Int, String) = {
    val nv = prevVarNumber + 1
    (nv, createVariable(nv))
  }


  private def maxVar(default: Int, vars: Seq[TypeVar]): Int = {
    if (vars.isEmpty) {
      default
    } else {
      val maybeMax = vars.map(tv => variableNumber(tv.name)).max
      if (maybeMax > default) {
        maybeMax
      } else {
        default
      }
    }
  }

  //lastusedtsvar
  def lastUsedVar(nv: Int, typeScheme: TypeScheme): Int = {
    val (f, b) = freeAndBoundTypeVariables(typeScheme)(Seq.empty, Seq.empty)
    maxVar(nv, f ++ b)
  }

  //(lastfreetsvar
  def lastFreeVar(nv: Int, typeScheme: TypeScheme): Int = {
    val (f, _) = freeAndBoundTypeVariables(typeScheme)(Seq.empty, Seq.empty)
    maxVar(nv, f)
  }

  //tssubs
  def substituteInTypeSchemes(nv: Int, substitutions: Seq[Substitution], typeScheme: TypeScheme): (Int, TypeScheme) = {
    if (substitutions.isEmpty) {
      (nv, typeScheme)
    } else {
      val (h_s@Substitution(tvar, tterm)) :: t_s = substitutions
      val (freeV, _) = freeAndBoundTypeVariables(tterm)(Seq.empty, Seq.empty)
      def iter(nv: Int, renameSubs: Seq[Substitution], substition: Substitution, typeScheme: TypeScheme): (Int, TypeScheme) = {
        typeScheme match {
          case Forall(name, scheme) =>
            if (name == substition.typeVar.name) {
              (nv, typeScheme)
            } else {
              if (freeV.contains(TypeVar(name))) {
                val (nv2, nName) = newVariable(nv)
                val s2 = Seq(Substitution(TypeVar(name), TypeVar(nName)))
                val (nv3, nTypeScheme) = iter(nv2, compose(s2, renameSubs), h_s, scheme)
                (nv3, Forall(nName, nTypeScheme))
              } else {
                val (nv2, nTypeScheme) = iter(nv, renameSubs, h_s, scheme)
                (nv2, Forall(name, nTypeScheme))
              }
            }
          case Type(term) =>
            val a = substitute(renameSubs, term)
            val b = substitute(Seq(substition), a)
            (nv, Type(b))
        }
      }
      val (nv2, nTypeScheme) = iter(nv, Seq.empty, h_s, typeScheme)
      substituteInTypeSchemes(nv2, t_s, nTypeScheme)
    }
  }

  case class Assumption(varname: String, typeScheme: TypeScheme)

  //fassumvars
  def freeVasrFromAssumptions(ass: Seq[Assumption]): Seq[TypeVar] = {
    ass.flatMap {
      case Assumption(_, typeScheme) =>
        val (f, _) = freeAndBoundTypeVariables(typeScheme)(Seq.empty, Seq.empty)
        f
    }.distinct
  }

  //assumvars
  def varsFromAssumptions(ass: Seq[Assumption]): Seq[TypeVar] = {
    ass.flatMap {
      case Assumption(_, typeScheme) =>
        val (f, b) = freeAndBoundTypeVariables(typeScheme)(Seq.empty, Seq.empty)
        f ++ b
    }.distinct
  }

  //lastfreeassumvar
  def lastFreeAssumVarNo(ass: Seq[Assumption]): Int = {
    (Seq(-1) ++ ass.map {
      case Assumption(_, typeScheme) =>
        lastFreeVar(-1, typeScheme)
    }).max
  }

  def substituteInAssumps(nv: Int, subs: Seq[Substitution], ass: Seq[Assumption]): (Int, Seq[Assumption]) = {
    def iter(ax: Seq[Assumption], nv: Int, ass: Seq[Assumption]): (Int, Seq[Assumption]) = {
      ass match {
        case Assumption(varname, typeScheme) :: t_ass =>
          val (nv2, nTypeScheme) = substituteInTypeSchemes(nv, subs, typeScheme)
          val nAx = Assumption(varname, nTypeScheme) +: ax
          iter(nAx, nv2, t_ass)
        case _ if ass.isEmpty =>
          (nv, ax.reverse)
      }
    }
    iter(Seq.empty, nv, ass)
  }

  def typeSchemeClosure(ass: Seq[Assumption], typeTerm: TypeTerm): TypeScheme = {
    val freeInAss = freeVasrFromAssumptions(ass)
    val (f, _) = freeAndBoundTypeVariables(typeTerm)(Seq.empty, Seq.empty)
    def isFreeInAss(typeVar: TypeVar): Boolean = freeInAss.contains(typeVar)
    def iter(bounded: Seq[TypeVar], yetFree: Seq[TypeVar]): TypeScheme = {
      yetFree match {
        case head :: tail =>
          if(isFreeInAss(head) || bounded.contains(head)){
            iter(bounded, tail)
          } else {
            Forall(head.name, iter(head +: bounded, tail))
          }
        case _ if yetFree.isEmpty =>
          Type(typeTerm)
      }
    }
    iter(Seq.empty, f)
  }

  //
  sealed trait ExprW {
    private var _typeTerm: Option[TypeTerm] = None


    def typeTerm() = _typeTerm
  }

  case class VarW(n: String) extends ExprW
  case class CombW(e1: ExprW, e2: ExprW) extends ExprW
  case class AbsW(n: String, e: ExprW) extends ExprW
  case class LetW(n: String, e1: ExprW, e2: ExprW) extends ExprW


  def -> (t1: TypeTerm, t2: TypeTerm) = TypeApp("fun", Seq(t1, t2))
  def --> (t1: TypeTerm, t2: TypeTerm, t3: TypeTerm) = TypeApp("fun", Seq(t1, t2, t3))
  def ---> (t1: TypeTerm, t2: TypeTerm, t3: TypeTerm, t4: TypeTerm) = TypeApp("fun", Seq(t1, t2, t3, t4))

  def lookupTypeSchemeInAss(ass: Seq[Assumption], name: String): TypeScheme = {
    ass.find(_.varname == name).map(_.typeScheme).getOrElse{
      !!!(s"variable $name not found in Assumptions: $ass")
    }
  }

  def W(nv: Int, gamma: Seq[Assumption], e: ExprW): (Int, (Seq[Substitution], TypeTerm))= {
    AlgorithmW.doIt(nv, gamma, e)
  }



  def principalts(gamma: Seq[Assumption], e: ExprW): TypeScheme = {
    val (nv, (s, tau)) = W(lastFreeAssumVarNo(gamma), gamma, e)
    val (_, sgamma) = substituteInAssumps(nv, s, gamma)
    typeSchemeClosure(sgamma, tau)
  }

  def principalts2(gamma: Seq[Assumption], e: ExprW): (TypeScheme, Seq[Substitution]) = {
    val (nv, (s, tau)) = W(lastFreeAssumVarNo(gamma), gamma, e)
    val (_, sgamma) = substituteInAssumps(nv, s, gamma)
    (typeSchemeClosure(sgamma, tau), s)
  }



  def !!!(msg: String) = throw new Exception(msg)

}



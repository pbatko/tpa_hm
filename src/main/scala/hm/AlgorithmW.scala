package hm

import hm.HmInference._

object AlgorithmW {

  def doIt(nv: Int, gamma: Seq[Assumption], e: ExprW): (Int, (Seq[Substitution], TypeTerm))= {
    val res = e match {
      case VarW(n) =>
        ???
      case CombW(e1, e2) =>
        val (nv2, (s1, tau1)) = doIt(nv, gamma, e1)
        val (nv3, s1gamma) = substituteInAssumps(nv2, s1, gamma)
        val (nv4, (s2, tau2)) = doIt(nv3, s1gamma, e2)
        val s2tau1 = substitute(s2, tau1)
        val (nv5, beta) = newVariable(nv4)
        val V = unify(s2tau1, ->(tau2, TypeVar(beta)))
        val Vbeta = substitute(V, TypeVar(beta))
        val VS2S1 = compose(compose(s1, s2), V)
        (nv5, (VS2S1, Vbeta))
      case AbsW(n, e) =>
        ???
      case LetW(n, e1, e2) =>
        ???
    }
    res
  }

}

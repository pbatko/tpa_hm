package hm

import hm.HmInference._

object HmUtils {

  val doubleLiteral = "doubleLiteral"
  val booleanLiteral = "booleanLiteral"
  val stringLiteral = "stringLiteral"
  val emptyListLiteral = "emptyListLiteral"
  val oneElementListCtor = "oneElementListCtor"
  val forLoopLit = "forLoopLit"
  val consLit = "cons"
  val unitLit = "unit"
  val nilLiteral = "nilLiteral"

  def structLiteral(name: String): String = name + "StructLiteral"
  def structCtorLiteral(name: String): String = name + "StructCtorLiteral"
  def structPatternMatchCastLiteral(name: String): String = name + "StructPatternMatchCastLiteral"

  def getAssumptions: Seq[Assumption] = gamma


  def nilLiteralW() = VarW(nilLiteral)

  def ifW(cond: ExprW, ifTrue: ExprW, ifFalse: ExprW) = CombW(CombW(CombW(VarW("if"), cond), ifTrue), ifFalse)
  def forW(loopIndex: String, loopIndexInit: ExprW, endCond: ExprW, indexChange: ExprW, body: ExprW) = {
//    LetW(loopIndex, CombW(CombW(CombW(VarW("forLoopLit"), loopIndexInit), endCond), indexChange), //make loopIndex visible in body
//      body) //enforce types of for loop components
    LetW(loopIndex, loopIndexInit, //make loopIndex visible in body
      CombW(CombW(CombW(CombW(VarW("forLoopLit"), loopIndexInit), endCond), indexChange), body)) //enforce types of for loop components
  }
  def callLambda(f: ExprW, args: ExprW*): ExprW = {
    if(args.isEmpty){
      CombW(f, VarW(unitLit))
    } else {
      (f +: args).reduce {
        (ax, bx) =>
        CombW(ax, bx)
      }
    }
  }
  def lambda(args: Seq[String], body: ExprW){
    if(args.isEmpty){
      !!!("???")
    } else {
      args.foldLeft(body){
        (ax, bx) =>
        AbsW(bx, ax)
      }
    }
  }

  def double = TypeApp("Double")
  def unit = TypeApp("Unit")
  def boolean = TypeApp("Boolean")
  def string = TypeApp("String")
  def listT(a: TypeVar) = TypeApp("list", Seq(a))
  def emptyListT = Forall("b", Type(TypeApp("list", Seq(b))))
//  def forLoopT = Forall("b", Type(->(double, ->(boolean, ->(double, b)))))
  def forLoopT = Forall("b", Type(->(double, ->(boolean, ->(double, ->(b, double))))))

  def patternMatchCastFunT(targetType: TypeTerm) = Forall("b", Type(->(b, targetType)))

  def getHeadT = Forall("b", Type(->(listT(b), b)))
  def getTailT = Forall("b", Type(->(listT(b), listT(b))))
  def consT = Forall("b", Type(->(b, ->(listT(b), listT(b)))))
  def oneElementListCtorT = Forall("b", Type(->(b, listT(b))))
  def emptyListCtorT = Forall("b", Type(listT(b)))
  def nilStructT = Forall("b", Type(b))
  def isEmptyT = Forall("b", Type(->(listT(b), boolean)))
  def opPlusT = Type(->(double, ->(double, double)))
  def opLessThanT = Type(->(double, ->(double, boolean)))
  def opMinusT = Type(->(double, ->(double, double)))
  def opMultT = Type(->(double, ->(double, double)))
  def opConsT(b: TypeVar) = Forall(b.name, Type(->(b, ->(listT(b), listT(b)))))
  def opStrConcatT = Type(->(string, ->(string, string)))
  def opEqT(b: TypeVar) = Forall(b.name, Type(->(b, ->(b, boolean))))
  def ifExprT(b: TypeVar) = Forall(b.name, Type(->(boolean, ->(b, ->(b, b)))))
  def seqOpT = Forall(b.name, Forall(c.name, (Type(->(b, ->(c, c))))))
  def b = TypeVar("b")
  def c = TypeVar("c")
  def d = TypeVar("d")

  def h = TypeVar("h")
  def l = TypeVar("l")
  def j = TypeVar("j")
  def g = TypeVar("g")
  def f = TypeVar("f")

//  def fix = Var("fix")
//  def nil = Var("nil")
  def fixT(b: TypeVar) = Forall(b.name, Type(->(->(b, b), b)))

  def gamma = Seq(
    Assumption("hd", getHeadT),
    Assumption("tl", getTailT),
    Assumption("+", opPlusT),
    Assumption("-", opMinusT),
    Assumption("*", opMultT),
    Assumption("<", opLessThanT),
    Assumption("::", opConsT(b)),
    Assumption("^", opStrConcatT),
    Assumption("==", opEqT(b)),
    Assumption("if", ifExprT(b)),
    Assumption(doubleLiteral, Type(double)),
    Assumption(booleanLiteral, Type(boolean)),
    Assumption(stringLiteral, Type(string)),
    Assumption(emptyListLiteral, emptyListT),
    Assumption(oneElementListCtor, oneElementListCtorT),
    Assumption(forLoopLit, forLoopT),
    Assumption("empty?", isEmptyT),
    Assumption(consLit, consT),
    Assumption("nil", emptyListCtorT),
    Assumption("Nil", nilStructT),
    Assumption(nilLiteral, nilStructT),
    Assumption("fix", fixT(b)),
    Assumption("seq", seqOpT),
    Assumption(unitLit, Type(unit))
  )
}

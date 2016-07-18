package hm

import hm.HmInference._
import org.scalatest.{FlatSpec, _}

abstract class BaseTest extends FlatSpec with ShouldMatchers

class HmInferenceTest extends BaseTest {


  it should "occurs" in {

    HmInference.occurs(TypeVar("a"), TypeApp("b", Seq(TypeVar("a")))) should be(true)
    HmInference.occurs(TypeVar("a"), TypeVar("a")) should be(false)
    HmInference.occurs(TypeVar("a"), TypeApp("b", Seq())) should be(false)

  }

  it should "substitute" in {


    val substitutions = Seq(Substitution(TypeVar("'a"), TypeApp("string")))

    HmInference.substitute(substitutions, TypeVar("'a")) shouldBe TypeApp("string", Seq.empty)
    HmInference.substitute(substitutions, TypeApp("'b", Seq(TypeVar("'a")))) shouldBe TypeApp("'b", Seq(TypeApp("string", Seq.empty)))

    val t2 = TypeApp("f", Seq(TypeApp("a"), TypeVar("y")))
    val t1 = TypeApp("f", Seq(TypeApp("a"), TypeApp("f", Seq(TypeApp("b"), TypeVar("x")))))

    val s1: Substitution = Substitution(TypeVar("x"), TypeApp("a"))
    val s2: Substitution = Substitution(TypeVar("y"), TypeApp("f", Seq(TypeApp("b"), TypeApp("a"))))
    val subs2 = Seq(s1, s2)

    val unified = TypeApp("f", Seq(TypeApp("a"), TypeApp("f", Seq(TypeApp("b"), TypeApp("a")))))
    HmInference.substitute(subs2, t1) shouldBe unified
    HmInference.substitute(subs2, t2) shouldBe unified

  }

  it should "compose substitutions" in {
    val s1 = Substitution(TypeVar("x"), TypeApp("a"))
    val s2 = Substitution(TypeVar("y"), TypeApp("f", Seq(TypeApp("b"), TypeVar("x"))))
    val composed = Seq(
      s1,
      Substitution(TypeVar("y"), TypeApp("f", Seq(TypeApp("b"), TypeApp("a"))))
    )

    HmInference.compose(Seq(s2), Seq(s1)) shouldBe composed
    println(composed)
  }

  it should "unify 1 " in {
    def j(a: TypeTerm, b: TypeTerm) = TypeApp("j", Seq(a, b))
    def f(a: TypeTerm, b: TypeTerm) = TypeApp("f", Seq(a, b))
    def a = TypeApp("a")
    def y = TypeVar("y")
    def z = TypeVar("z")

    val t1 = j(y, z)
    val t2 = j(f(z, z), f(a, a))

    val unifier: Seq[Substitution] = HmInference.unify(t2, t1)
    println(unifier)
    //TODO
  }

  it should "unify 2" in {
    def j(a: TypeTerm, b: TypeTerm, c: TypeTerm) = TypeApp("j", Seq(a, b, c))
    def f(a: TypeTerm, b: TypeTerm) = TypeApp("f", Seq(a, b))
    def a = TypeApp("a")
    def x = TypeVar("x")
    def y = TypeVar("y")
    def z = TypeVar("z")

    val t1 = j(x, y, z)
    val t2 = j(f(y, y), f(z, z), f(a, a))

    println(t1)
    println(t2)
    val unifier: Seq[Substitution] = HmInference.unify(t2, t1)
    println(unifier)

    //TODO

    //expected:: [ f ( f ( f (a, a), f (a, a)), f ( f (a, a), f (a, a)))/x, f ( f (a, a), f (a, a))/y, f (a, a)/z]
  }

  it should "unify 3" in {
    def a = TypeApp("a")
    def b = TypeVar("b")
    def c = TypeVar("c")
    val list = TypeApp("list", Seq(TypeVar("a")))
    def f(a: TypeTerm, b: TypeTerm) = TypeApp("f", Seq(a, b))

    val t1 = f(list, a)
    val t2 = f(b, c)

    println(t1)
    println(t2)
    val unifier: Seq[Substitution] = HmInference.unify(t2, t1)
    val unifier2: Seq[Substitution] = HmInference.unify(t1, t2)
    println(unifier)
    unifier shouldBe unifier2
    //TODO
  }

  it should "find free type vars in type term" in {
    val tterm = TypeApp("a")

    val (f, b) = HmInference.freeAndBoundTypeVariables(tterm)(Seq.empty, Seq.empty)

    f shouldBe Seq()
    b shouldBe Seq()
  }

  it should "find free type vars in type term 2" in {
    val tterm = TypeVar("a")

    val (f, b) = HmInference.freeAndBoundTypeVariables(tterm)(Seq.empty, Seq.empty)

    f shouldBe Seq(TypeVar("a"))
    b shouldBe Seq()
  }

  it should "find free type vars in type term 3" in {
    val tterm = TypeApp("b", Seq(TypeVar("a")))

    val (f, b) = HmInference.freeAndBoundTypeVariables(tterm)(Seq.empty, Seq.empty)

    f shouldBe Seq(TypeVar("a"))
    b shouldBe Seq()
  }
  //fbtyvars [] [] (Tyapp("string", []))
  //-> [] []
  //
  //fbtyvars [] [] (Tyvar("string"))
  //-> ["string"] []
  //
  //fbtyvars [] [] (Tyapp("a", [Tyvar("b")]))
  //-> (["b"],[])


  it should "rename free type vars" in {

    val scheme = Forall("a", Type(TypeVar("b")))
    val subs = Seq(Substitution(TypeVar("b"), TypeApp("string")))

    val (_, nScheme) = HmInference.substituteInTypeSchemes(1, subs, scheme)

    val expected = Forall("a", Type(TypeApp("string")))
    nScheme shouldBe expected

    //  val sc = Forall("a", Type(Tyvar("b")));
    //  val su = [(Tyapp("string", []), "b")];
    //  ppts (#2 (tssubs 1 su sc));
  }

  it should "rename free type vars 2" in {

    val scheme = Forall("a", Type(TypeVar("a")))
    val subs = Seq(Substitution(TypeVar("a"), TypeApp("string")))

    val (_, nScheme) = HmInference.substituteInTypeSchemes(1, subs, scheme)

    val expected = Forall("a", Type(TypeVar("a")))
    nScheme shouldBe expected
    //
    //      val sc = Forall("a", Type(Tyvar("a")));
    //      val su = [(Tyapp("string", []), "a")];
    //      ppts (#2 (tssubs 1 su sc));
    //      val it = "!a.a" : string

  }

  it should "rename free type vars 3" in {

    val scheme = Forall("c", Type(TypeVar("a")))
    val subs = Seq(Substitution(TypeVar("a"), TypeVar("c"))) // sub to free var, it should be renamed to some new value

    val (_, nScheme) = HmInference.substituteInTypeSchemes(HmInference.variableNumber("c"), subs, scheme)

    val expected = Forall("d", Type(TypeVar("c")))
    nScheme shouldBe expected
    //
    //      val sc = Forall("c", Type(Tyvar("a")));
    //      val su = [(Tyvar("c"), "a")];
    //      ppts (#2 (tssubs (varno("c")) su sc));
    //      val it = "!a.a" : string

  }

  it should "rename free type vars 4" in {

    val scheme = Forall("a", Forall("b", Type(TypeApp("c", Seq(TypeVar("d"), TypeVar("e"))))))
    val subs = Seq(Substitution(TypeVar("d"), TypeVar("a")), Substitution(TypeVar("e"), TypeVar("b"))) // sub to free var, it should be renamed to some new value

    val (_, nScheme) = HmInference.substituteInTypeSchemes(HmInference.variableNumber("e"), subs, scheme)

    val expected = Forall("f", Forall("g", Type(TypeApp("c", Seq(TypeVar("a"), TypeVar("b"))))))
    nScheme shouldBe expected

    //    val sc = Forall("a", Forall("b", Type(Tyapp("c", [Tyvar("d"), Tyvar("e")] ) ) ) );
    //    val su =[(Tyvar("a"), "d"), (Tyvar("b"), "e")];
    //    ppts(#2(tssubs(varno("e")) su sc));
    //    val it = "!f.!g.(a, b) c": string

  }

  it should "rename free type vars 5" in {

    val scheme = Forall("a", Forall("b", Type(TypeApp("c", Seq(TypeVar("d"), TypeVar("e"))))))
    val subs = Seq(Substitution(TypeVar("d"), TypeVar("a")), Substitution(TypeVar("e"), TypeVar("a"))) // sub to free var, it should be renamed to some new value

    val (_, nScheme) = HmInference.substituteInTypeSchemes(HmInference.variableNumber("e"), subs, scheme)

    val expected = Forall("f", Forall("b", Type(TypeApp("c", Seq(TypeVar("a"), TypeVar("a"))))))
    nScheme shouldBe expected

    //    val sc = Forall("a", Forall("b", Type(Tyapp("c", [Tyvar("d"), Tyvar("e")] ) ) ) );
    //    val su =[(Tyvar("a"), "d"), (Tyvar("a"), "e")];
    //    ppts(#2(tssubs(varno("e")) su sc));
    //    val it = "!f.!b.(a, a) c" : string

  }

  it should "close over type term, making x generic because it is bounded in env and free in term being closed over" in {
    val env = Seq(Assumption("ala123", Forall("x", Type(TypeVar("x")))))
    val term = TypeVar("x")

    val r = HmInference.typeSchemeClosure(env, term)

    r shouldBe Forall("x", Type(term))

    //    tsclosure [("a", Forall("x", Type(Tyvar("x"))))] (Tyvar "x")
    //    val it = Forall ("x",Type (Tyvar "x")) : typescheme
  }

  it should "close over type term, making x generic because it is bounded in env and free in term being closed over, there are to x vars, but only one forall should be inserted" in {
    val env = Seq(Assumption("ala123", Forall("x", Type(TypeVar("x")))))
    val term = TypeApp("f", Seq(TypeVar("x"), TypeVar("x")))

    val r = HmInference.typeSchemeClosure(env, term)

    r shouldBe Forall("x", Type(term))
    println(r)

    //    tsclosure [("a", Forall("x", Type(Tyvar("x"))))] (Tyapp("f", [(Tyvar "x"),(Tyvar "x")]))
    //    val it = Forall ("x",Type (Tyapp ("f",[Tyvar "x",Tyvar "x"]))) : typescheme
  }

  it should "close over type term, not making x generic because it is free in env" in {
    val env = Seq(Assumption("ala123", Type(TypeVar("x"))))
    val term = TypeApp("f", Seq(TypeVar("x"), TypeVar("x")))

    val r = HmInference.typeSchemeClosure(env, term)
    println(r)

    r shouldBe Type(term)

    //    tsclosure [("a", Type(Tyvar("x")))] (Tyapp("f", [(Tyvar "x"),(Tyvar "x")]))
    //    val it = Type (Tyapp ("f",[Tyvar "x",Tyvar "x"])) : typescheme
  }

  it should "infer type of function getting anything and returning list of different anything" in {
    val listType: TypeApp = TypeApp("list", Seq(TypeVar("b")))
    val gamma = Seq(
      Assumption("z", Type(listType))
    )
    val e = AbsW("y", LetW("x", VarW("y"), VarW("z")))

    val r = HmInference.principalts(gamma, e)

    val expected = Forall("c", Type(->(TypeVar("c"), listType)))
    r shouldBe expected
    println(r)

    //    val gamma = [("z", (Type(Tyapp("list", [(Tyvar("b"))]))))];
    //    val e = (Abs("y", (Let(("x",Var("y")), Var("z")))));
    //    ppts (principalts gamma e);
    //    val it = "!c.c -> b list" : string

  }

  it should "infer type of function getting anything and returning list of different anything, when list's type var collide with next new var" in {
    val listType: TypeApp = TypeApp("list", Seq(TypeVar("a")))
    val gamma = Seq(
      Assumption("z", Type(listType))
    )
    val e = AbsW("y", VarW("z"))

    val r = HmInference.principalts(gamma, e)

    val expected = Forall("b", Type(->(TypeVar("b"), listType)))
    r shouldBe expected
    println(r)

  }

}

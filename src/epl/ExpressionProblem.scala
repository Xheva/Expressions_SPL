package epl

/**
  * Created by Tërnava.
  */
// Base interface
trait ExpAlg[E] {
  def lit(x: Int): E
}

// Evolution A: Adding pretty printing
trait Echo {
  def print(): String
}

trait PrintExpAlg extends ExpAlg[Echo] {
  def lit(x: Int) = new Echo() {
    def print() = x.toString()
  }
}

// Evolution 1: Adding subtraction
trait AddExpAlg[E] extends ExpAlg[E] {
  def add(e1: E, e2: E): E
}
// Evolution 2: Adding adding
trait SubExpAlg[E] extends ExpAlg[E] {
  def sub(e1: E, e2: E): E
}
// Updating evaluations:
trait PrintAddExpAlg extends PrintExpAlg with AddExpAlg[Echo] {
  def add(e1: Echo, e2: Echo) = new Echo {
    def print() = e1.print() + "+" + e2.print()
  }
}
trait PrintSubExpAlg extends PrintExpAlg with SubExpAlg[Echo] {
  def sub(e1: Echo, e2: Echo) = new Echo() {
    def print() = e1.print() + "-" + e2.print()
  }
}

// Evolution B: The evaluation interface
trait Eval {
  def eval(): Int
}
// Updating evaluation:
trait EvalExpAlg extends SubExpAlg[Eval] with AddExpAlg[Eval] {
  def lit(x: Int) = new Eval() {
    def eval() = x
  }

  def add(e1: Eval, e2: Eval) = new Eval() {
    def eval() = e1.eval() +  e2.eval()
  }

  def sub(e1: Eval, e2: Eval) = new Eval() {
    def eval() = e1.eval() - e2.eval()
  }
}

/* An alternative implementation
** of pretty printing that directly computes a string */

trait PrintExpAlg2 extends SubExpAlg[String]{
  def lit(x: Int) = x.toString()
  def add(e1: String, e2: String) = e1 + " + " + e2
  def sub(e1: String, e2: String) = e1 + " - " + e2
}

// Test
object ExpressionProblem {
  def test(): Unit = {
    val ea = new EvalExpAlg() {}
    val pa2 = new PrintExpAlg2() {}
    val tt = pa2.sub(pa2.lit(5), pa2.lit(7)) + " = " + ea.sub(ea.lit(5), ea.lit(7)).eval()
    print(tt)
  }
  def main(args: Array[String]) {
    test
  }
}

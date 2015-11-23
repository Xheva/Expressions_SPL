package epl_v2

/**
  * Created by Tërnava.
  */
//VP_A
class Operations

/*Base definition*/
// VP_B
trait ExpAlg {
  type Opr <: Operations
  def lit(x: Int): Opr
}

/* Extension A1 */
trait Echo extends Operations {
  def print(): String
}

/* Extension A2 */
trait Eval extends Operations {
  def eval(): Int
}

/* Extension B1 */
trait AddExpAlg extends ExpAlg {
  def add(e1: Opr, e2: Opr): Opr
}

/* Extension B2 */
trait SubExpAlg extends ExpAlg {
  def sub(e1: Opr, e2: Opr): Opr
}

// ---------- IMPLEMENTATIONS -------------------

/* Base implementation
 * Feature "Print" for "Lit"
 *  */
trait PrintExpAlg extends ExpAlg {
  type Opr = Echo
  def lit(x: Int) = new Echo() {
    def print() = x.toString()
  }
}

/* Feature "Print" for "Add" */
trait PrintAddExpAlg extends PrintExpAlg with AddExpAlg {
  def add(e1: Echo, e2: Echo) = new Echo() {
    def print() = e1.print() + "+" + e2.print()
  }
}

/* Feature "Print" for "Sub" */
trait PrintSubExpAlg extends PrintExpAlg with SubExpAlg {
  def sub(e1: Echo, e2: Echo) = new Echo() {
    def print() = e1.print() + "-" + e2.print()
  }
}

/* Base implementation
 * Feature "Eval" for "Lit"
 *  */
trait EvalExpAlg extends ExpAlg {
  type Opr = Eval
  def lit(x: Int) = new Eval() {
    def eval() = x
  }
}

/* feature "Eval" for "Add"*/
trait EvalAddExpAlg extends EvalExpAlg with AddExpAlg {
  def add(e1: Eval, e2: Eval) = new Eval() {
    def eval() = e1.eval() + e2.eval()
  }
}

/* Feature "Eval" for "Sub" */
trait EvalSubExpAlg extends EvalExpAlg with SubExpAlg {
  def sub(e1: Eval, e2: Eval) = new Eval() {
    def eval() = e1.eval() - e2.eval()
  }
}

/* Mandatory features ("Lit" and "Print")
 * are shown as defaults in a "Base" type
 */
trait Base extends PrintExpAlg

/* a test */
object ExpressionProblem {

  class EvalAddExpAlgC extends EvalAddExpAlg with EvalSubExpAlg
  class PrintAddExpAlgC extends PrintAddExpAlg with PrintSubExpAlg

  def main(args: Array[String]) {
    val eval = new EvalAddExpAlgC
    val echo = new PrintAddExpAlgC
    val evalprintexp = echo.add(echo.lit(2), echo.lit(3)).print() + " = " + eval.add(eval.lit(2), eval.lit(3)).eval()
    println(evalprintexp)
  }

}
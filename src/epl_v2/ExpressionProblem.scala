package epl_v2

class Operations

/*Base definition*/
trait ExpAlg {
  type Opr <: Operations
  def lit(x: Int): Opr
}

/* Extension 1*/
trait PPrint extends Operations {
  def print(): String
}

/* Extension 2*/
trait Eval extends Operations {
  def eval(): Int
}

/* Extension 3*/
trait AddExpAlg extends ExpAlg {
  def add(e1: Opr, e2: Opr): Opr
}

/* Extension 4*/
trait SubExpAlg extends ExpAlg {
  def sub(e1: Opr, e2: Opr): Opr
}

// ---------- IMPLEMENTATIONS -------------------

/* Base implementation
 * Feature "Print" for "Lit"
 *  */
trait PrintExpAlg extends ExpAlg {
  type Opr = PPrint
  def lit(x: Int) = new PPrint() {
    def print() = x.toString()
  }
}

/* Feature "Print" for "Add" */
trait PrintAddExpAlg extends PrintExpAlg with AddExpAlg {
  def add(e1: PPrint, e2: PPrint) = new PPrint() {
    def print() = e1.print() + "+" + e2.print()
  }
}

/* Feature "Print" for "Sub" */
trait PrintSubExpAlg extends PrintExpAlg with SubExpAlg {
  def sub(e1: PPrint, e2: PPrint) = new PPrint() {
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

  class PrintWithAdd extends PrintAddExpAlg
  class PrintWithSub extends PrintSubExpAlg

  class EvalWithAdd extends EvalAddExpAlg
  class EvalWithSub extends EvalSubExpAlg

  class Core extends Base
  class EvalBase extends EvalExpAlg

  val pwa = new PrintWithAdd
  val pws = new PrintWithSub

  val ewa = new EvalWithAdd
  val ews = new EvalWithSub

  val cr = new Core {
    new PrintAddExpAlg {
      val l = lit(5)
      add(l, l).print()
    }
    
    new PrintSubExpAlg {

    }
  }
    
  val eb = new EvalBase

 println(cr)
  
  //println(cr)

  //else---------------------------
  new PrintAddExpAlg {
    add(pwa.lit(2), pwa.lit(8))
  }

  //else---------------------------
  object Base extends Base
  val in = Base lit 10 print ()

 // val basewithadd = new Core with PrintAddExpAlg -------------------------------
  //println(basewithadd.add(basewithadd.lit(15), basewithadd.lit(15)).print())----

  //else---------------------------
  class EvalAddExpAlgC extends EvalAddExpAlg with EvalSubExpAlg
  class EvalSubExpAlgC extends EvalSubExpAlg

  class PrintAddExpAlgC extends PrintAddExpAlg
  class PrintSubExpAlgC extends PrintSubExpAlg

  def main(args: Array[String]) {

    val eeac = new EvalAddExpAlgC

    val l1 = eeac.lit(2)
    val l2 = eeac.lit(3)

    val e1 = eeac.lit(10)
    val e2 = eeac.add(l1, l2).eval()
    val e3 = eeac.sub(l1, l2).eval()
    println(e1.eval())
    println(e2)
    println(e3)

  }

}

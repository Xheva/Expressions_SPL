package epl_v3

/**
  * Created by Tërnava.
  */

sealed trait Expr 
case class Lit(n: Int) extends Expr
case class Add(l: Expr, r: Expr) extends Expr
case class Sub(l: Expr, r: Expr) extends Expr

class Eval {
  def eval(e: Expr): Int = e match {
    case Lit(n) => n
    case Add(l, r) => eval(l) + eval(r)
    case Sub(l, r) => eval(l) - eval(r)
  }
}

class Echo {
  def echo(e: Expr): Unit = e match {
    case Lit(n) => print(" " + n + " ")
    case Add(l, r) => echo(l); print("+"); echo(r)
    case Sub(l, r) => echo(l); print("-"); echo(r)
  }
}

//test
object ExpressionProblem { 
  def main(args: Array[String]) {

    val eval = new Eval
    val echo = new Echo

    val expr1 =
      Sub(Add(Lit(4), Sub(Lit(7), Lit(10))),
        Add(Lit(4), Sub(Lit(7), Lit(10))))

    val x = eval.eval(expr1)
    val p = echo.echo(expr1)

    p + "" + println(" = " + x)

  }
}
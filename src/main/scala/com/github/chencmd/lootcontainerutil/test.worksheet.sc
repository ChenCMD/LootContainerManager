enum Expression {
  case Variable(val identifier: String, val location: Int) extends Expression
  case Constant(val value: String, val location: Int)      extends Expression
  case BinaryOp[A <: ExpressionElement](val expr: A)       extends Expression
  case SomethingSyntaxSugar(val hoge: Any)                 extends Expression
}
import Expression.*

sealed trait ExpressionElement {
  type Elem <: Expression
  val value: Elem
}
object ExpressionElement       {
  case class Phase1Expr(val value: Phase1Expressions) extends ExpressionElement { type Elem = Phase1Expressions }
  case class Phase2Expr(val value: Phase2Expressions) extends ExpressionElement { type Elem = Phase2Expressions }
}

type Phase1Expressions = Variable | Constant | BinaryOp[ExpressionElement.Phase1Expr] | SomethingSyntaxSugar
type Phase2Expressions = Variable | Constant | BinaryOp[ExpressionElement.Phase2Expr]

def f = {
  def toPhase2(expr: Phase1Expressions): Phase2Expressions = expr match {
    case v: Variable                                     => v
    case c: Constant                                     => c
    case b: BinaryOp[ExpressionElement.Phase1Expr] => b.expr match {
        case ExpressionElement.Phase1Expr(value) => toPhase2(value)
      }
    case SomethingSyntaxSugar(hoge)                      => ???
  }

  def toResult(expr: Phase2Expressions): String = expr match {
    case Variable(id, loc)    => s"$id: $loc"
    case Constant(value, loc) => s"$value: $loc"
    case BinaryOp(expr)       => expr match {
        case ExpressionElement.Phase2Expr(value) => toResult(value)
      }
  }

  val initialAST: Phase1Expressions = ???
  val phase1result                  = toPhase2(initialAST)
  val phase2result                  = toResult(phase1result)
}

package expr


sealed trait ExpressionTree
case class Constant(value: Double) extends ExpressionTree
case class Plus(left: ExpressionTree, right: ExpressionTree) extends ExpressionTree
case class Minus(left: ExpressionTree, right: ExpressionTree) extends ExpressionTree
case class UnaryMinus(expr: ExpressionTree) extends ExpressionTree
case class Times(left: ExpressionTree, right: ExpressionTree) extends ExpressionTree
case class Divide(left: ExpressionTree, right: ExpressionTree) extends ExpressionTree
case class Power(base: ExpressionTree, exponent: ExpressionTree) extends ExpressionTree

object Expression {
	def expressionToString(expr: ExpressionTree): String = expr match {
		case Constant(value) => s"(${value.toString})"
		case Plus(left, right) => s"${expressionToString(left)} + ${expressionToString(right)}"
		case Minus(left, right) => s"${expressionToString(left)} - ${expressionToString(right)}"
		case UnaryMinus(e) => s"-${expressionToString(e)}"
		case Times(left, right) => s"(${expressionToString(left)}) * (${expressionToString(right)})"
		case Divide(left, right) => s"(${expressionToString(left)}) / (${expressionToString(right)})"
		case Power(base, exponent) => s"(${expressionToString(base)}) ** (${expressionToString(exponent)})"
	}

	private def precedence( expr: ExpressionTree): Int = expr match {
		case Constant(_) => 5
		case UnaryMinus(_) => 4
		case Power(_, _) => 3
		case Times(_, _) | Divide(_, _) => 2
		case Plus(_, _) | Minus(_, _) => 1
	}

	private def isRightAssoc(expr: ExpressionTree): Boolean = expr match {
		case Power(_, _) => true
		case UnaryMinus(_) => true
		case _ => false
	}

	private def needsParens(operand: ExpressionTree, parentPrec: Int, parentAssocRight: Boolean, isLeftOperand: Boolean): Boolean = {
		val opPrec = precedence(operand)
		if (opPrec < parentPrec) {
			true
		} else if (opPrec > parentPrec) {
			false
		} else {
			if (parentAssocRight) !isLeftOperand else isLeftOperand
				
		}
	}

	private def pretty(operand: ExpressionTree, parentPrec: Int, parentAssocRight: Boolean, isLeftOperand: Boolean): String = operand match {
		case UnaryMinus(_) if parentPrec != 0 => s"(${expressionToPrettyString(operand)})"
		case _ if needsParens(operand, parentPrec, parentAssocRight, isLeftOperand) => s"(${expressionToPrettyString(operand)})"
		case _ => expressionToPrettyString(operand)
	}

	private def maybeParens(expr: ExpressionTree, parentPrec: Int, parentAssocRight: Boolean, isLeftOperand: Boolean): String = {
		if (needsParens(expr, parentPrec, parentAssocRight, isLeftOperand)) {
			s"(${expressionToPrettyString(expr)})"
		} else {
			expressionToPrettyString(expr)
		}
	}

	def expressionToPrettyString(expr: ExpressionTree): String = expr match {
		case Constant(value) => value.toString
		case Plus(left, right) =>
			s"${maybeParens(left, 1, false, true)} + ${maybeParens(right, 1, false, false)}"
		case Minus(left, right) =>
			s"${maybeParens(left, 1, false, true)} - ${maybeParens(right, 1, false, false)}"
		case Times(left, right) =>
			s"${maybeParens(left, 2, false, true)} * ${maybeParens(right, 2, false, false)}"
		case Divide(left, right) =>
			s"${maybeParens(left, 2, false, true)} / ${maybeParens(right, 2, false, false)}"
		case Power(base, exponent) =>
			s"${maybeParens(base, 3, true, true)} ** ${maybeParens(exponent, 3, true, false)}"
		case UnaryMinus(e) =>
			val eStr = maybeParens(e, 4, true, false)
			s"-$eStr"
	}


}

object ExpressionTest extends App {
  import Expression._

  val expr1 = Plus(Constant(3), Times(Constant(4), Constant(5)))
  println(expressionToString(expr1))         // Output: "(3.0) + ((4.0) * (5.0))"
  println(expressionToPrettyString(expr1))   // Output: "3.0 + 4.0 * 5.0"

  val expr2 = Times(Plus(Constant(3), Constant(4)), Constant(5))
  println(expressionToString(expr2))         // Output: "((3.0) + (4.0)) * (5.0)"
  println(expressionToPrettyString(expr2))   // Output: "(3.0 + 4.0) * 5.0"

  val expr3 = Power(Constant(2), Plus(Constant(3), Constant(4)))
  println(expressionToString(expr3))         // Output: "(2.0) ** ((3.0) + (4.0))"
  println(expressionToPrettyString(expr3))   // Output: "2.0 ** (3.0 + 4.0)"

  val expr4 = UnaryMinus(Plus(Constant(3), Constant(4)))
  println(expressionToString(expr4))         // Output: "-((3.0) + (4.0))"
  println(expressionToPrettyString(expr4))   // Output: "-(3.0 + 4.0)"

  val expr5 = Minus(Constant(5), Times(Constant(2), Constant(3)))
  println(expressionToString(expr5))         // Output: "(5.0) - ((2.0) * (3.0))"
  println(expressionToPrettyString(expr5))   // Output: "5.0 - 2.0 * 3.0"

}

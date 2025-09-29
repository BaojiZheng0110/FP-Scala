
import expr.Expression._
import expr.{ExpressionTree, Constant, Plus, Minus, Times, Divide, Power, UnaryMinus}

@main def runExpressionTests(): Unit = {
  val exprs = List(
    Plus(Constant(1), Constant(2)),
    Times(Constant(7), Minus(Constant(8), Constant(3))),
    Divide(Constant(10), Constant(2)),
    Power(Constant(2), Constant(8)),
    Plus(Times(Constant(2), Constant(3)), Divide(Constant(9), Constant(3))),
    Minus(Power(Constant(5), Constant(2)), Times(Constant(3), Constant(4))),
    UnaryMinus(Divide(Constant(6), Plus(Constant(1), Constant(2)))),
    Times(Plus(Constant(1), Constant(2)), Power(Constant(3), Constant(2))),
    Divide(Minus(Constant(10), Constant(4)), Times(Constant(2), Constant(3))),
    Power(Plus(Constant(2), Constant(3)), Minus(Constant(4), Constant(1))),
    Plus(
      Times(Constant(2), Power(Constant(3), Constant(2))),
      Divide(Constant(8), UnaryMinus(Constant(2)))
    )
  )

  // Expression 1: Plus(Constant(1), Constant(2))
  println(expressionToString(exprs(0)))         // Output: "(1.0) + (2.0)"
  println(expressionToPrettyString(exprs(0)))   // Output: "1.0 + 2.0"

  // Expression 2: Times(Constant(7), Minus(Constant(8), Constant(3)))
  println(expressionToString(exprs(1)))         // Output: "(7.0) * ((8.0) - (3.0))"
  println(expressionToPrettyString(exprs(1)))   // Output: "7.0 * (8.0 - 3.0)"

  // Expression 3: Divide(Constant(10), Constant(2))
  println(expressionToString(exprs(2)))         // Output: "(10.0) / (2.0)"
  println(expressionToPrettyString(exprs(2)))   // Output: "10.0 / 2.0"

  // Expression 4: Power(Constant(2), Constant(8))
  println(expressionToString(exprs(3)))         // Output: "(2.0) ** (8.0)"
  println(expressionToPrettyString(exprs(3)))   // Output: "2.0 ** 8.0"

  // Expression 5: Plus(Times(Constant(2), Constant(3)), Divide(Constant(9), Constant(3)))
  println(expressionToString(exprs(4)))         // Output: "((2.0) * (3.0)) + ((9.0) / (3.0))"
  println(expressionToPrettyString(exprs(4)))   // Output: "2.0 * 3.0 + 9.0 / 3.0"

  // Expression 6: Minus(Power(Constant(5), Constant(2)), Times(Constant(3), Constant(4)))
  println(expressionToString(exprs(5)))         // Output: "((5.0) ** (2.0)) - ((3.0) * (4.0))"
  println(expressionToPrettyString(exprs(5)))   // Output: "5.0 ** 2.0 - 3.0 * 4.0"

  // Expression 7: UnaryMinus(Divide(Constant(6), Plus(Constant(1), Constant(2))))
  println(expressionToString(exprs(6)))         // Output: "-((6.0) / ((1.0) + (2.0)))"
  println(expressionToPrettyString(exprs(6)))   // Output: "-(6.0 / (1.0 + 2.0))"

  // Expression 8: Times(Plus(Constant(1), Constant(2)), Power(Constant(3), Constant(2)))
  println(expressionToString(exprs(7)))         // Output: "((1.0) + (2.0)) * ((3.0) ** (2.0))"
  println(expressionToPrettyString(exprs(7)))   // Output: "(1.0 + 2.0) * 3.0 ** 2.0"

  // Expression 9: Divide(Minus(Constant(10), Constant(4)), Times(Constant(2), Constant(3)))
  println(expressionToString(exprs(8)))         // Output: "((10.0) - (4.0)) / ((2.0) * (3.0))"
  println(expressionToPrettyString(exprs(8)))   // Output: "(10.0 - 4.0) / (2.0 * 3.0)"

  // Expression 10: Power(Plus(Constant(2), Constant(3)), Minus(Constant(4), Constant(1)))
  println(expressionToString(exprs(9)))         // Output: "((2.0) + (3.0)) ** ((4.0) - (1.0))"
  println(expressionToPrettyString(exprs(9)))   // Output: "(2.0 + 3.0) ** (4.0 - 1.0)"

  // Expression 11: Plus(Times(Constant(2), Power(Constant(3), Constant(2))), Divide(Constant(8), UnaryMinus(Constant(2))))
  println(expressionToString(exprs(10)))        // Output: "((2.0) * ((3.0) ** (2.0))) + ((8.0) / (-(2.0)))"
  println(expressionToPrettyString(exprs(10)))  // Output: "2.0 * 3.0 ** 2.0 + 8.0 / -2.0"

  // Expression 12: Constant only
  println(expressionToString(Constant(42)))         // Output: "(42.0)"
  println(expressionToPrettyString(Constant(42)))   // Output: "42.0"

  // Expression 13: Nested Plus and Minus
  val expr13 = Plus(Minus(Constant(10), Constant(3)), Plus(Constant(2), Constant(5)))
  println(expressionToString(expr13))               // Output: "((10.0) - (3.0)) + ((2.0) + (5.0))"
  println(expressionToPrettyString(expr13))         // Output: "(10.0 - 3.0) + (2.0 + 5.0)"

  // Expression 14: Nested Times and Divide
  val expr14 = Divide(Times(Constant(6), Constant(7)), Constant(2))
  println(expressionToString(expr14))               // Output: "((6.0) * (7.0)) / (2.0)"
  println(expressionToPrettyString(expr14))         // Output: "6.0 * 7.0 / 2.0"

  // Expression 15: Power with UnaryMinus
  val expr15 = Power(UnaryMinus(Constant(3)), Constant(2))
  println(expressionToString(expr15))               // Output: "(-(3.0)) ** (2.0)"
  println(expressionToPrettyString(expr15))         // Output: "-3.0 ** 2.0"

  // Expression 16: Complex nesting
  val expr16 = Plus(
    Times(Constant(2), Power(Minus(Constant(8), Constant(3)), Constant(2))),
    Divide(UnaryMinus(Constant(9)), Plus(Constant(1), Constant(2)))
  )
  println(expressionToString(expr16))               // Output: "((2.0) * (((8.0) - (3.0)) ** (2.0))) + ((-(9.0)) / ((1.0) + (2.0)))"
  println(expressionToPrettyString(expr16))         // Output: "2.0 * (8.0 - 3.0) ** 2.0 + -9.0 / (1.0 + 2.0)"

}

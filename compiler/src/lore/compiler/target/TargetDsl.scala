package lore.compiler.target

import lore.compiler.target.Target._

object TargetDsl {

  implicit class VariableExtension(variable: Variable) {
    def declareAs(value: TargetExpression): VariableDeclaration = {
      VariableDeclaration(variable.name, value)
    }

    def declareMutableAs(value: TargetExpression): VariableDeclaration = {
      VariableDeclaration(variable.name, value, isMutable = true)
    }

    def exportAs(value: TargetExpression): VariableDeclaration = {
      VariableDeclaration(variable.name, value, shouldExport = true)
    }
  }

  implicit class ExpressionExtension(expression: TargetExpression) {
    def call(arguments: TargetExpression*): Call = Target.Call(expression, arguments.toVector)
    def `new`(arguments: TargetExpression*): New = Target.New(expression, arguments.toVector)
    def prop(name: String): TargetExpression = Target.PropertyAccess(expression, name.asName)
    def assign(value: TargetExpression): Assignment = Target.Assignment(expression, value)
    def element(key: TargetExpression): TargetExpression = Target.ListAccess(expression, key)
    def iterateWithIndex(length: TargetExpression)(inner: (TargetExpression, TargetExpression) => TargetStatement): TargetStatement = {
      val varIndex = "i".asVariable
      Target.For(
        varIndex.declareMutableAs(Target.IntLiteral(0)),
        TargetOperator.LessThan(varIndex, length),
        varIndex.assign(TargetOperator.Addition(varIndex, Target.IntLiteral(1))),
        inner(expression.element(varIndex), varIndex)
      )
    }
  }

  implicit class OperatorExtension(operator: TargetOperator) {
    def apply(operands: TargetExpression*): Operation = Target.Operation(operator, operands.toVector)
  }

  implicit class StringExtension(val value: String) {
    def asName: TargetName = new TargetName(value)
    def asLiteral: StringLiteral = StringLiteral(value)
    def asVariable: Variable = value.asName.asVariable
    def asParameter: Parameter = value.asName.asParameter
  }

}

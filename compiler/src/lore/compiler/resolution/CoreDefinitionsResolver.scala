package lore.compiler.resolution

import lore.compiler.feedback.{CoreFeedback, Reporter}
import lore.compiler.semantics.Registry
import lore.compiler.semantics.core.{CoreDefinitions, CoreMultiFunction, CoreTrait}
import lore.compiler.semantics.modules.GlobalModule
import lore.compiler.types.{BasicType, TraitSchema, TupleType, Type}

object CoreDefinitionsResolver {

  /**
    * Resolves all core definitions. If some definitions cannot be found, the resolver reports appropriate errors. They
    * are still added to CoreDefinitions, just without their underlying binding.
    */
  def resolve()(implicit registry: Registry, reporter: Reporter): CoreDefinitions = {
    implicit val coreModule: GlobalModule = registry.getOrCreateModule(CoreDefinitions.modulePath)
    val inputAny = TupleType(BasicType.Any)
    val inputAnyAny = TupleType(BasicType.Any, BasicType.Any)

    val Type: CoreTrait = resolveTrait("Type")

    new CoreDefinitions(
      Type = Type,
      equal = resolveMultiFunction("equal?", inputAnyAny, BasicType.Boolean),
      less_than = resolveMultiFunction("less_than?", inputAnyAny, BasicType.Boolean),
      less_than_equal = resolveMultiFunction("less_than_equal?", inputAnyAny, BasicType.Boolean),
      hash = resolveMultiFunction("hash", inputAny, BasicType.Int),
      to_string = resolveMultiFunction("to_string", inputAny, BasicType.String),
    )
  }

  private def resolveTrait(simpleName: String)(implicit coreModule: GlobalModule, reporter: Reporter): CoreTrait = {
    val name = CoreDefinitions.modulePath + simpleName
    val schema = coreModule.getSchema(simpleName) match {
      case Some(schema) => schema match {
        case schema: TraitSchema => Some(schema)
        case _ =>
          reporter.error(CoreFeedback.Trait.TraitExpected(name))
          None
      }

      case None =>
        reporter.error(CoreFeedback.Trait.NotFound(name))
        None
    }
    new CoreTrait(name, schema)
  }

  private def resolveMultiFunction(
    simpleName: String,
    expectedInputType: TupleType,
    expectedOutputType: Type,
  )(implicit coreModule: GlobalModule, reporter: Reporter): CoreMultiFunction = {
    val name = CoreDefinitions.modulePath + simpleName
    val mf = coreModule.getMultiFunction(simpleName) match {
      case Some(mf) =>
        val min = mf.min(expectedInputType)
        if (min.isEmpty) {
          reporter.error(CoreFeedback.MultiFunction.NotFound(name, expectedInputType))
          None
        } else if (min.exists(_.signature.outputType </= expectedOutputType)) {
          reporter.error(CoreFeedback.MultiFunction.IllegalOutputType(name, expectedInputType, expectedOutputType))
          None
        } else Some(mf)

      case None =>
        reporter.error(CoreFeedback.MultiFunction.NotFound(name, expectedInputType))
        None
    }
    new CoreMultiFunction(name, expectedOutputType, mf)
  }

}

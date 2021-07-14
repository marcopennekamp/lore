package lore.lsp.index

import lore.compiler.core.Fragment
import lore.compiler.feedback.MemoReporter
import lore.compiler.phases.parsing.ParsingPhase
import lore.compiler.syntax.{DeclNode, TypeDeclNode}
import lore.compiler.utils.CollectionExtensions.VectorExtension

object FragmentChangeHandler {

  /**
    * Processes the given changed fragment and works all changes into the global index.
    *
    * Only member usages cannot be updated this way, because instance type information is required for each member
    * access. The member usage index is currently only populated once when building the index from a newly created
    * registry.
    */
  def process(fragment: Fragment)(implicit globalIndex: GlobalIndex): Unit = {
    fragment.uri.foreach { fragmentUri =>
      implicit val reporter: MemoReporter = MemoReporter()
      val nodes = ParsingPhase.process(fragment)

      val multiFunctionDeclarations = nodes.filterType[DeclNode.FunctionNode].groupBy(_.name).map {
        case (name, functionNodes) => IndexBuilder.updateBindingDeclarationByFragments(name, functionNodes.map(_.position))
      }

      val typeDeclarations = nodes.filterType[TypeDeclNode].map {
        case TypeDeclNode.AliasNode(nameNode, _, position) =>
          IndexBuilder.updateTypeDeclaration(nameNode.value, Vector.empty, position)

        case TypeDeclNode.StructNode(nameNode, _, properties, position) =>
          IndexBuilder.updateTypeDeclaration(nameNode.value, properties.map(p => (p.name, p.nameNode.position)), position)

        case TypeDeclNode.TraitNode(nameNode, _, position) =>
          IndexBuilder.updateTypeDeclaration(nameNode.value, Vector.empty, position)
      }

      // Sometimes a declaration will be removed from a fragment.
      val declarations = multiFunctionDeclarations ++ typeDeclarations
      val removedDeclarations = globalIndex.fragmentDeclarationsCache.get(fragmentUri) -- declarations
      removedDeclarations.foreach { declaration =>
        globalIndex.fragmentDeclarationsCache.remove(declaration, fragmentUri)
      }
    }
  }

}

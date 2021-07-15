package lore.lsp.index

import lore.lsp.index.IndexTypeDeclaration.IndexMemberDeclaration
import org.eclipse.lsp4j.{Location, Range}

case class IndexUsage(declaration: IndexDeclaration, range: Range)

case class IndexMemberUsage(declarations: Vector[IndexMemberDeclaration], location: Location)

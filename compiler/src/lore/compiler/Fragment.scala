package lore.compiler

import fastparse.ParserInput
import lore.compiler.ast.DeclNode

class Fragment(val name: String, val input: ParserInput, val declarations: List[DeclNode])

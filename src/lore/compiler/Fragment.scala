package lore.compiler

import fastparse.ParserInput
import lore.ast.DeclNode

class Fragment(val name: String, val input: ParserInput, val declarations: List[DeclNode])

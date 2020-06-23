package lore.types

/**
  * This object supports calculating arbitrary binary relations between types.
  */
object TypeRelations {

  /**
    * We define the calculation of a type relation in terms of rules that possibly match a pair of types
    * and then decides whether these types are in the relation or not. We define rules as partial functions
    * so that we can support preconditions.
    */
  type Rule = PartialFunction[(Type, Type), Boolean]

  /**
    * Decides whether t1 and t2 are in the relation described by the given rules.
    */
  def inRelation(rules: List[Rule])(t1: Type, t2: Type): Boolean = {
    // TODO: Hide this code behind a feature switch so that it doesn't get run or even compiled when we need
    //  performance. This is only for reporting compiler bugs, really.
    if (!rules.exists(_.isDefinedAt((t1, t2)))) {
      // TODO: Use a logger with a log level of DEBUG or TRACE.
      //println(s"A decision about a relation between types $t1 and $t2 was attempted, but none of the rules match.")
    }

    // t1 is in a relationship with  t2 if any of the rules are true.
    rules.exists(rule => rule.isDefinedAt((t1, t2)) && rule.apply((t1, t2)))
  }

}

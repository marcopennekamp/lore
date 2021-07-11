package lore.compiler.feedback

object FeedbackExtensions {

  implicit class FilterDuplicatesExtension[A](vector: Vector[A]) {
    /**
      * Filters out duplicate elements in respect to the key provided by the `key` function. For any group of
      * duplicates, the `duplicate` function is consulted with a representative element of that group to report an
      * error.
      */
    def filterDuplicates[K](key: A => K, duplicate: A => Feedback.Error)(implicit reporter: Reporter): Vector[A] = {
      vector.groupBy(key).values.toVector.flatMap {
        case Vector(a) => Some(a)
        case group if group.size > 1 =>
          reporter.error(duplicate(group.head))
          None
      }
    }

    /**
      * Verifies that the vector contains no duplicate elements in respect to the key provided by the `key` function.
      * For any group of duplicates, the `duplicate` function is consulted with a representative element of that group
      * to report an error.
      */
    def verifyUnique[K](key: A => K, duplicate: A => Feedback.Error)(implicit reporter: Reporter): Unit = {
      filterDuplicates(key, duplicate)
    }
  }

}

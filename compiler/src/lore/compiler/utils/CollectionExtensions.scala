package lore.compiler.utils

import scala.collection.{Factory, SeqOps}
import scala.reflect.ClassTag

object CollectionExtensions {
  implicit class FilterTypeExtension[A, That](seqOps: SeqOps[A, Iterable, _]) {
    def filterType[T <: A](implicit tag: ClassTag[T], factory: Factory[T, That]): That = {
      factory.fromSpecific(seqOps.flatMap {
        case value: T => Some(value)
        case _ => None
      })
    }
    def filterNotType[T <: A](implicit tag: ClassTag[T], factory: Factory[A, That]): That = {
      factory.fromSpecific(seqOps.flatMap {
        case _: T => None
        case value => Some(value)
      })
    }
  }
}

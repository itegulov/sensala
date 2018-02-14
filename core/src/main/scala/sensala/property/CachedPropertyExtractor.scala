package sensala.property

import scala.collection.mutable

case class CachedPropertyExtractor(propertyExtractor: PropertyExtractor) extends PropertyExtractor {
  private val cache = mutable.WeakHashMap.empty[String, List[Property]]
  
  override def extractProperties(word: String, maxLevel: Int = 2): List[Property] =
    cache.getOrElseUpdate(word, propertyExtractor.extractProperties(word, maxLevel))
}

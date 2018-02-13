package sensala.property

trait PropertyExtractor {
  def extractProperties(word: String, maxLevel: Int = 2): List[Property]
}

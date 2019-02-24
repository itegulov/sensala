package sensala.property

import com.typesafe.scalalogging.Logger
import net.didion.jwnl.JWNL
import net.didion.jwnl.data._
import net.didion.jwnl.data.list._
import net.didion.jwnl.dictionary.Dictionary
import org.aossie.scavenger.expression.Sym
import sensala.structure._

import scala.collection.convert.ImplicitConversionsToScala._
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

object WordNetPropertyExtractor {
  val logger = Logger[this.type]

  JWNL.initialize(getClass.getResourceAsStream("/jwnl-properties.xml"))
  val dictionary   = Dictionary.getInstance()
  val pointerUtils = PointerUtils.getInstance()

  private def getPointers(synset: Synset, typ: PointerType): Array[Pointer] = {
    val list     = ArrayBuffer.empty[Pointer]
    val pointers = synset.getPointers()
    for (pointer <- pointers) {
      if (pointer.getType == typ) list += pointer
    }
    list.toArray
  }

  private def getTargets(synset: Synset, typ: PointerType): Array[PointerTarget] = {
    val pointers = getPointers(synset, typ)
    val targets  = new Array[PointerTarget](pointers.length)
    for ((pointer, i) <- pointers.zipWithIndex) {
      targets(i) = pointer.getTarget
    }
    targets
  }

  private def makePointerTargetTreeList(
    synset: Synset,
    searchTypes: Array[PointerType],
    labelType: PointerType,
    depth: Int,
    allowRedundancies: Boolean,
    parent: PointerTargetTreeNode
  ): PointerTargetTreeNodeList = {
    val list = new PointerTargetTreeNodeList
    for (typ <- searchTypes) {
      val targets = new PointerTargetNodeList(getTargets(synset, typ))
      if (targets.size > 0) {
        for (itr <- targets.toList.map(_.asInstanceOf[PointerTargetNode])) {
          val node = new PointerTargetTreeNode(
            itr.getPointerTarget,
            if (labelType == null) typ else labelType,
            parent
          )
          if (allowRedundancies || !list.contains(node)) {
            if (depth != 1) {
              node.setChildTreeList(
                makePointerTargetTreeList(
                  node.getSynset,
                  searchTypes,
                  labelType,
                  depth - 1,
                  allowRedundancies,
                  node
                )
              )
            }
            list.add(node)
          }
        }
      }
    }
    list
  }

  private def makePointerTargetTreeList(set: Synset,
                                        searchType: PointerType,
                                        depth: Int): PointerTargetTreeNodeList = {
    val searchTypes = new Array[PointerType](1)
    searchTypes(0) = searchType
    makePointerTargetTreeList(set, searchTypes, null, depth, true, null)
  }

  private def getHypernymTree(synset: Synset): PointerTargetTree =
    new PointerTargetTree(
      synset,
      makePointerTargetTreeList(synset, PointerType.HYPERNYM, Integer.MAX_VALUE)
    )

  def extractProperties(word: String): List[Property] = {
    val result = ArrayBuffer.empty[Property]
    for (indexWord <- dictionary.lookupAllIndexWords(word).getIndexWordArray) {
      for (sense <- indexWord.getSenses) {
        logger.debug(s"Trying sense for $word: ${sense.getGloss}")
        val tree = Try(getHypernymTree(sense))
        for (pathAny <- tree.map(tree => tree.toList.toList).getOrElse(List.empty)) {
          val path = pathAny.asInstanceOf[PointerTargetNodeList]
          for (nodeAny <- path.toList) {
            val node       = nodeAny.asInstanceOf[PointerTargetNode]
            val properties = node.getSynset.getWords.map(_.getLemma)
            logger.debug(properties.mkString(", "))
            result ++= properties.map(lemma => Property(x => Sym(lemma)(x)))
          }
        }
        logger.debug("\n")
      }
    }
    result.toList
  }

  def main(args: Array[String]): Unit =
    extractProperties("farmer")
}

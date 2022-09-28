package caseClasses

trait CustomTree{
  def elemStr: String
}
case class NodeAny(override val elemStr: String, head: CustomTree, tail: CustomTree) extends CustomTree
case class LeafInput(override val elemStr: String, inputCode: String) extends CustomTree
case class LeafIds(override val elemStr: String, idsElems: String) extends CustomTree
case class LeafStroke(override val elemStr: String, strokeMapResult: String) extends CustomTree
case class LeafShape(override val elemStr: String, shapeMapResult: String) extends CustomTree
case class LeafNone(override val elemStr: String) extends CustomTree
case class CharContent(override val elemStr: String,
                       freqInfo: List[String],
                       officialInputCodes: List[String],
                       binaryTree: CustomTree,
                       flattenedTree: List[CustomTree],
                       problemElems: List[CustomTree],
                       originalShapeLookup: String) extends CustomTree

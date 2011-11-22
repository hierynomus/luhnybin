import collection.immutable.IndexedSeq
import collection.mutable.ListBuffer

object LuhnCheck {
  val lookup : Map[Char, Int] = Map('0'->0, '1'->2, '2'->4, '3'->6, '4'->8, '5'->1, '6'->3, '7'->5, '8'->7, '9'->9)
  case class LuhnChar(c: Char, var mask: Boolean) {
    def char = if (mask) 'X' else c
  }

  class LuhnWindow(val size: Int) {
    val window : ListBuffer[LuhnChar] = ListBuffer()

    def push(lc: LuhnChar) {
      if (!lc.c.isDigit) return
      
      if (full)
        window.remove(size - 1)

      window.prepend(lc)
      if (containsLuhn) window.foreach(_.mask = true)
    }

    def clear { window.clear() }
    def full = window.size == size

    def containsLuhn = {
      if (!full)
        false
      else
        window.zipWithIndex.map(t => if (t._2 % 2 == 1) lookup(t._1.c) else t._1.c.getNumericValue).sum % 10 == 0
    }
  }

  def main(args: Array[String]) {
    Iterator.continually(Console.readLine()).takeWhile(t => t != "" && t != null).foreach(l => println(maskLuhn(l)))
  }

  def maskLuhn(line: String) = {
    val seq: IndexedSeq[LuhnChar] = line.map(LuhnChar(_, false))
    val luhnWindow = Seq(new LuhnWindow(16), new LuhnWindow(15), new LuhnWindow(14))
    for (lc <- seq) {
      if (partOfLuhn(lc)) luhnWindow.foreach(_.push(lc)) else luhnWindow.foreach(_.clear)
    }
    seq.map(_.char).mkString
  }


  def partOfLuhn(l: LuhnChar) = l.c == ' ' || l.c == '-' || l.c.isDigit

}
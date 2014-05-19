package elegans

import Cells._

object Graphs {
  val drawSimplifiedGraphs = true

  class DotConverter(cells: List[Cell]) {
    override def toString: String = {
      val res = new StringBuffer()

      res append "digraph {\n"
        
      for (cell <- cells) {
        drawCell(cell, res)
      }

      for (cell <- cells) {
        drawEdges(cell, res)
      }

      res append "}\n"

      res.toString
    }

    def c2s(cell: Cell) = cell.name + Helpers.uniqueName(cell)
    def n2s(node: Node) = node.name + Helpers.uniqueName(node)
    def p2s(port: Port) = port.name + Helpers.uniqueName(port)

    def drawCell(cell: Cell, res: StringBuffer) {
      res append """
  subgraph cluster_""" + c2s(cell) + """{
    style=rounded;
    node [shape=box style=rounded];
    """
      
      for (n <- cell.N) {
        if (drawSimplifiedGraphs) 
          drawNodeSimplified(n, res)
        else
          drawNode(n, res)
      }

      res append """
    label=""" + "\"" + cell.name + """"
  }"""
    }

    def drawNode(node: Node, res: StringBuffer) {
      res append """
    subgraph cluster_""" + n2s(node) + """{
      label=""" + "\"" + node.name + "\";\n"
      for (p <- node.P) {
        drawPort(p, res)
      }
      res append """
    }"""

    }

    def drawNodeSimplified(node: Node, res: StringBuffer) {
      val enabledNow = node.outputPorts.exists(_.enabledNow)
      val color = if (enabledNow) "green" else "black"
      res append (n2s(node) + " [label=\"" + node.name + "\" color=" + color + "];\n")
    }

    def drawPort(port: Port, res: StringBuffer) {
      res append (p2s(port) + " [label=\"" + port.name + "\"];\n")
    }

    def drawEdges(cell: Cell, res: StringBuffer) {
      for (n <- cell.N)
        if (drawSimplifiedGraphs)
          drawEdgesSimplified(n, res)
        else
          drawEdges(n, res)
    }

    def drawEdges(node: Node, res: StringBuffer) {
      for (p <- node.P)
        drawEdges(p, res)
    }

    def drawEdgesSimplified(node: Node, res: StringBuffer) {
      def findNodeByPort(p: Port) = {
        var ret: Option[Node] = None
        for (c <- cells) {
          for (n <- c.N) {
            if (n.P.contains(p))
              ret = Some(n)
          }
        }
        ret
      }


      for (p <- node.P) {
        for (e <- p.delayedEdges) {
          val (activeColor, arrowHead) = e.role match {
            case Activating => ("green", "normal")
            case Inhibiting => ("red", "tee")
          }
          val passiveColor = "black"
          val colorStyle = "color=" + (if (p.enabledNow) activeColor else passiveColor)
          val arrowStyle = "arrowhead=" + arrowHead
          val styles = List(colorStyle, arrowStyle)

          findNodeByPort(e.dest) match {
            case Some(dstNode) =>
              res append Helpers.arrow(n2s(node), n2s(dstNode), styles)
            case None =>
          }
        }
        for (e <- p.nonDelayedEdges) {
          val (activeColor, arrowHead) = e.role match {
            case Activating => ("green", "normal")
            case Inhibiting => ("red", "tee")
          }
          val passiveColor = "black"
          val colorStyle = "color=" + (if (p.enabledNow) activeColor else passiveColor)
          val arrowStyle = "arrowhead=" + arrowHead
          val styles = List(colorStyle, arrowStyle)

          findNodeByPort(e.dest) match {
            case Some(dstNode) =>
              res append Helpers.arrow(n2s(node), n2s(dstNode), 
                "style=\"setlinewidth(3)\"" :: styles)
            case None =>
          }
        }
      }
    }

    def drawEdges(port: Port, res: StringBuffer) {
      val color = if (port.enabledNow) "green" else "black"
      for (e <- port.E) {
        val source = e.source
        val dest = e.dest
        res append Helpers.arrow(p2s(source), p2s(dest), List("color=" + color))
      }
    }

    def writeFile(filename: String) {
      import java.io.{BufferedWriter,FileWriter}
      val out = new BufferedWriter(new FileWriter(filename))
      out.write(toString)
      out.close
    }

  }

  object Helpers {
    private var _nextID = 0
    private def nextID = {
      _nextID += 1
      _nextID.toString
    }

    private var names = Map[AnyRef,String]()

    def uniqueName(obj: AnyRef): String = {
      if (!names.contains(obj))
        names += (obj -> nextID)

      names(obj)
    }

    def arrow(src: String, dst: String, opts: List[String] = Nil) = {
      src + " -> " + dst + opts.mkString("[", " ", "]") + ";\n"
    }
  }
}

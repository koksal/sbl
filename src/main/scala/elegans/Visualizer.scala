package elegans

import Cells._
import Model._
import Schedules._

import scala.collection.mutable.ListBuffer

import scala.swing._
import scala.swing.event._

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

object Visualization {
  class Visualizer {
    val images = new ListBuffer[BufferedImage]()
    val macrosteps = new ListBuffer[MacroStep]()
    val microsteps = new ListBuffer[MicroStep]()

    def microSnapshot(cells: List[Cell], schedule: MicroStep) {
      snapshot(cells, Left(schedule))
    }

    def macroSnapshot(cells: List[Cell], schedule: MacroStep) {
      snapshot(cells, Right(schedule))
    }

    private def snapshot(cells: List[Cell], schedule: Either[MicroStep, MacroStep]) {
        import Graphs._

        val graph = new DotConverter(cells)

        val dotPath = "graph.dot"
        val imagePath = "graph.png"

        graph.writeFile("graph.dot")

        import scala.sys.process._

        ("dot -Tpng -o " + imagePath + " " + dotPath).!!

        if (Settings.resizeImages) {
          val size = java.awt.Toolkit.getDefaultToolkit().getScreenSize()
          ("convert -resize " + (size.getWidth().toDouble * Settings.resizeRatio) + "x" + (size.getHeight() * 4 / 5) + " " + imagePath + " " + imagePath).!!
        }

        images append ImageIO.read(new File(imagePath)) 

        schedule match {
          case Left(micro) => microsteps append micro
          case Right(macro) => macrosteps append macro
        }

        ("rm -f " + dotPath + " " + imagePath).!!
    }

    def show() {
      val history: Either[FineSchedule, CoarseSchedule] = if (macrosteps.isEmpty)
        Left(microsteps.toList) else Right(macrosteps.toList)
      new Interface(images.toList, history).main(Array())
    }

  }

  class ImagePanel extends Panel {
    private var _bufferedImage: BufferedImage = null

    def bufferedImage = _bufferedImage

    def bufferedImage_=(buf: BufferedImage) {
      _bufferedImage = buf
    }


    override def paintComponent(g: Graphics2D) = {
      if (null != bufferedImage) g.drawImage(bufferedImage, 0, 0, null)
    }
  }

  def labelText(schedule: Either[FineSchedule, CoarseSchedule], 
      highlightIndex: Int): String = {
    val listOfEithers: List[Either[MicroStep, MacroStep]] = schedule match {
      case Left(fineSch) => fineSch map (Left(_))
      case Right(coarseSch) => coarseSch map (Right(_))
    }
    "<html>" + 
    (for (i <- 0 until listOfEithers.size) yield {
      val str = listOfEithers(i) match {
        case Left(microstep) => {
          val mapped = microstep.map {
            case false => "0"
            case true => "1"
          }
          mapped.mkString("[","","]")
        }
        case Right(macrostep) => {
          val mapped = macrostep.map {
            case Disabled => "-"
            case EnabledLeft => "&lt;"
            case EnabledRight => ">"
          }
          mapped.mkString("["," ","]")
        }
      }
      val color = if (i == highlightIndex) "red" else "black"
      "<font color=" + color + ">" + str + "</font>"
    }).toList.mkString("<br/>") +
    "<html/>"
  }

  class Interface(images: List[BufferedImage], 
      schedule: Either[FineSchedule, CoarseSchedule]) extends SimpleSwingApplication {
    def top = new MainFrame {
      title = "Visualizer"

      val slider = new Slider {
        min = 0
        max = images.size - 1
        value = 0
        snapToTicks = true
        // paintLabels = true
        // labels = ((1 to images.size) map (i => (i, new Label(i.toString)))).toMap
      }

      val imagePanel = new ImagePanel {
        bufferedImage = images.head
        val (w, h) = (bufferedImage.getWidth(), bufferedImage.getHeight())
        minimumSize   = new Dimension(w, h)
        maximumSize   = new Dimension(w, h)
        preferredSize = new Dimension(w, h)
      }
      
      val scheduleLabel = new Label(labelText(schedule, 0)) {
        minimumSize   = new Dimension(200, 800)
        maximumSize   = new Dimension(200, 800)
        preferredSize = new Dimension(200, 800)
      }

      val schedulePanel = new BorderPanel {
        layout(scheduleLabel) = BorderPanel.Position.Center
        minimumSize   = new Dimension(300, 800)
        maximumSize   = new Dimension(300, 800)
        preferredSize = new Dimension(300, 800)
      }

      contents = new BorderPanel {
        layout(slider) = BorderPanel.Position.North
        layout(imagePanel) = BorderPanel.Position.Center
        layout(schedulePanel) = BorderPanel.Position.East

        listenTo(slider)
        reactions += {
          case vc: ValueChanged => 
            val newIndex = vc.source.asInstanceOf[Slider].value
            imagePanel.bufferedImage = images(newIndex)
            scheduleLabel.text = labelText(schedule, newIndex)
            repaint()
        }

      }

      val dim = new Dimension(imagePanel.size.width + schedulePanel.size.width + 4, imagePanel.size.height + slider.size.height + 20)
      minimumSize = dim
      maximumSize = dim
      preferredSize = dim

      override def closeOperation() {
        sys.exit(0)
      }
    }
  }
}

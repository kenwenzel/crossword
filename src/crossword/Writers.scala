package crossword

import scala.xml.NodeSeq
import scala.xml.PrettyPrinter
import java.nio.channels.Channels
import java.io.FileOutputStream

object Writers {
  val Encoding = "UTF-8"
  def toFile(ns: NodeSeq, filename: String) {
    val pp = new PrettyPrinter(200, 2)
    val fos = new FileOutputStream(filename)
    val writer = Channels.newWriter(fos.getChannel, Encoding)
    try {
      writer.write("<?xml version='1.0' encoding='" + Encoding + "'?>\n")
      writer.write(pp.formatNodes(ns))
    } finally {
      writer.close
    }
  }

  def toHtml(cws: List[Crossword], grid: Int): NodeSeq = {
    <html>
      { for (cw <- cws) yield <div style="margin-bottom:50px">{ toSvg(cw, grid) }</div> }
    </html>
  }

  def toSvg(cw: Crossword, grid: Int): NodeSeq = {
    implicit def intToStr(i: Int) = i.toString

    <svg width={ cw.width * grid } height={ cw.height * grid }>
      {
        for (((x, y), c) <- cw.placedChars) yield {
          <rect x={ x * grid } y={ y * grid } width={ grid } height={ grid } style="stroke:black;stroke-width:1;fill:none"/>
          <text x={ ((x + .5) * grid).toInt } y={ ((y + .5) * grid).toInt } font-size={ (grid * 0.7).toInt } style="dominant-baseline:central;text-anchor:middle">{ c }</text>
        }
      }
    </svg>
  }
}
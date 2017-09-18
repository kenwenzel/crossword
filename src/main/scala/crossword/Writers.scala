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

    <svg width={ (cw.width + 1) * grid } height={ (cw.height + 1) * grid }>
      <g transform={ s"translate($grid $grid)" }>
        <g id="grid">
          >
          {
            for (((x, y), c) <- cw.placedChars) yield {
              <rect x={ x * grid } y={ y * grid } width={ grid } height={ grid } style="stroke:black;stroke-width:1;fill:none"/>
            }
          }
        </g>
        <g id="chars">
          {
            for (((x, y), c) <- cw.placedChars) yield {
              <text x={ ((x + .5) * grid).toInt } y={ ((y + .5) * grid).toInt } font-size={ (grid * 0.7).toInt } style="dominant-baseline:central;text-anchor:middle">{ c }</text>
            }
          }
        </g>
        <g>
          {
            for ((i, (x, y, horizontal)) <- cw.placedWords) yield {
              val w = cw.spec.words(i)
              (if (horizontal) (x - 1, y) else (x, y - 1)) match {
                case field if cw.placedChars.contains(field) =>
                case (x1, y1) => {
                  val xpos = x1 * grid
                  val ypos = y1 * grid
                  val fontsize = (grid * 0.5).toInt
                  <g>
                    <rect x={ xpos } y={ ypos } width={ grid } height={ grid } style="stroke:black;stroke-width:1;fill:black"/>
                    <polygon points={
                      if (horizontal) {
                        (xpos + .7 * grid).toInt + "," + ypos + " " + (xpos + grid) + "," + (ypos + grid / 2) + " " + (xpos + .7 * grid).toInt + "," + (ypos + grid)
                      } else {
                        xpos + "," + (ypos + .7 * grid).toInt + " " + (xpos + grid / 2) + "," + (ypos + grid) + " " + (xpos + grid) + "," + (ypos + .7 * grid).toInt
                      }
                    } style="fill:lightgray;stroke:none"/>
                    <text x={ if (horizontal) xpos + 2 else (xpos + .5 * grid).toInt } y={ if (horizontal) (ypos + .5 * grid).toInt else (ypos + fontsize + 2) } font-size={ fontsize } font-weight="bold" style={
                      (if (horizontal) "dominant-baseline:central" else "text-anchor:middle") + ";fill:white"
                    }>{ i + 1 }</text>
                  </g>
                }
              }
            }
          }
        </g>
      </g>
    </svg>
  }
}
package crossword

import java.util.Random
import java.util.ArrayList
import scala.io.Source
import scala.collection.immutable.BitSet
import scala.collection.mutable
import scala.collection.JavaConversions._
import org.uncommons.watchmaker.framework.EvolutionaryOperator
import org.uncommons.watchmaker.framework.FitnessEvaluator
import org.uncommons.watchmaker.framework.factories.AbstractCandidateFactory
import org.uncommons.watchmaker.framework.operators.AbstractCrossover
import org.uncommons.maths.random.MersenneTwisterRNG
import org.uncommons.watchmaker.framework.operators.EvolutionPipeline
import org.uncommons.watchmaker.framework.GenerationalEvolutionEngine
import org.uncommons.watchmaker.framework.selection.RouletteWheelSelection
import org.uncommons.watchmaker.framework.termination.TargetFitness
import org.uncommons.watchmaker.framework.termination.ElapsedTime

class CWEvaluator extends FitnessEvaluator[Crossword] {
  override def getFitness(candidate: Crossword, population: java.util.List[_ <: Crossword]): Double = {
    val conflicts = candidate.placedAtOrigin
    conflicts * (candidate.spec.possibleXPoints.size / candidate.crossed) * (if (candidate.width > candidate.height) candidate.width / candidate.height else candidate.height / candidate.width)
  }

  override def isNatural = false
}

class CWMutation(val spec: CWSpec) extends EvolutionaryOperator[Crossword] {
  override def apply(selectedCandidates: java.util.List[Crossword], rng: Random) = {
    val mutated = new ArrayList[Crossword](selectedCandidates.size)
    for (cw <- selectedCandidates) mutated.add(mutate(cw, rng))
    mutated
  }

  def mutate(cw: Crossword, rng: Random): Crossword = {
    val remove = mutable.Set[XPoint]()
    val add = mutable.Set[XPoint]()
    for (i <- 0 to rng.nextInt(5)) {
      val xpoint = spec.possibleXPointsSeq(rng.nextInt(spec.possibleXPointsSeq.length))
      cw.xpoints.get((xpoint.w1.index, xpoint.w2.index)) match {
        case Some(XPoint(c, _, i1, _, i2)) if xpoint.c == c && xpoint.i1 == i1 && xpoint.i2 == i2 => // exists
        case Some(xp) =>
          remove.add(xp); add.add(xpoint)
        case None => add.add(xpoint)
      }
    }
    var fixed = BitSet()
    val newXPoints = cw.xpoints.filter { entry =>
      if (!remove.contains(entry._2) || rng.nextFloat < .5) {
        fixed += entry._1._1
        fixed += entry._1._2
        true
      } else false
    }

    val newXPoints2 = newXPoints ++ add.map { xpoint => ((xpoint.w1.index, xpoint.w2.index), xpoint) }
    new Crossword(spec, newXPoints2)
  }
}

class CWCrossover(crossoverPoints: Int) extends AbstractCrossover[Crossword](crossoverPoints) {
  override def mate(parent1: Crossword, parent2: Crossword, numberOfCrossoverPoints: Int, rng: Random) = {
    val xpoints1 = parent1.xpoints.toArray.sortBy(_._1)
    val xpoints2 = parent1.xpoints.toArray.sortBy(_._1)
    val minLen = xpoints1.length min xpoints2.length
    val temp = Array.ofDim[((Int, Int), XPoint)](minLen)
    for (i <- 0 until numberOfCrossoverPoints) {
      val index = 1 + rng.nextInt(xpoints1.length min xpoints2.length - 1)
      Array.copy(xpoints1, 0, temp, 0, index)
      Array.copy(xpoints2, 0, xpoints1, 0, index)
      Array.copy(temp, 0, xpoints2, 0, index)
    }
    val result = new ArrayList[Crossword]
    result.add(new Crossword(parent1.spec, xpoints1.toMap))
    result.add(new Crossword(parent2.spec, xpoints2.toMap))
    result
  }
}

class CWFactory(val spec: CWSpec) extends AbstractCandidateFactory[Crossword] {
  override def generateRandomCandidate(rng: Random) = {
    val xpoints = spec.possibleXPoints flatMap {
      // create xpoint with possibility of 70%
      case ((w1, w2), xpoints) if xpoints.nonEmpty && rng.nextFloat < .7 => {
        val index = rng.nextInt(xpoints.length)
        Some(((w1, w2), xpoints(index)))
      }
      case _ => Nil
    }
    new Crossword(spec, xpoints)
  }
}

case class Word(index: Int, chars: String)
case class XPoint(c: Char, w1: Word, i1: Int, w2: Word, i2: Int)

class CWSpec(val words: Array[Word], val possibleXPoints: Map[(Int, Int), IndexedSeq[XPoint]]) {
  val possibleXPointsSeq = possibleXPoints.values.toIndexedSeq.flatten
}

class Crossword(val spec: CWSpec, val xpoints: Map[(Int, Int), XPoint]) {
  val (placement, (width, height), usedXPoints, crossed, placedAtOrigin) = computePlacement

  def print(grid: Map[(Int, Int), Char]) {
    var result = Array.fill[Char](width, height)(' ')
    grid.foreach { case ((x, y), c) => result(x)(y) = c }
    println(result.map(_.mkString(" ")).mkString("\n"))
  }

  def computePlacement = {
    val words = spec.words
    val points4Word = xpoints.toList.map { case ((iw1, iw2), xpoint) => (iw1, xpoint) }.groupBy(_._1).mapValues {
      v => v.map(_._2)
    }

    val placedChars: mutable.Map[(Int, Int), Char] = mutable.Map.empty
    var usedXPoints = 0; var crossed = 0; var placedAtOrigin = 0
    var placed: mutable.Map[Int, Boolean] = mutable.Map.empty
    def place(w: Word, horizontal: Boolean = true, x0: Int = 0, y0: Int = 0): Boolean = {
      if (!placed.contains(w.index)) {
        if (x0 == 0 && y0 == 0) placedAtOrigin += 1
        val xy = horizontal match {
          case true => (x0 until x0 + w.chars.length).map((_, y0))
          case false => (y0 until y0 + w.chars.length).map((x0, _))
        }
        var crossedLocal = 0
        val conflict = w.chars.view.zip(xy).exists {
          case (c, (x, y)) => placedChars.get(x, y) match {
            case Some(existing) if existing == c =>
              crossedLocal += 1; false // no conflict
            case Some(existing) => true // conflict
            case None => false
          }
        }
        if (!conflict) {
          crossed += crossedLocal
          placed(w.index) = horizontal
          for ((c, (x, y)) <- w.chars.view.zip(xy)) placedChars((x, y)) = c
          points4Word.get(w.index).map {
            _.foreach { xpoint =>
              val (x1, y1) = horizontal match {
                case true => (x0 + xpoint.i1, y0)
                case false => (x0, y0 + xpoint.i1)
              }
              placed.get(xpoint.w2.index) match {
                case None =>
                  val (x2, y2) = horizontal match {
                    case false => (x1 - xpoint.i2, y0)
                    case true => (x1, y1 - xpoint.i2)
                  }
                  if (place(xpoint.w2, !horizontal, x2, y2)) usedXPoints += 1
                case Some(_) => // already placed
              }
            }
          }
          true
        } else false
      } else false
    }
    words foreach (place(_))

    var (xMin, yMin) = (Int.MaxValue, Int.MaxValue)
    var (xMax, yMax) = (Int.MinValue, Int.MinValue)
    placedChars.keys.foreach {
      case (x, y) =>
        xMin = xMin.min(x); yMin = yMin.min(y)
        xMax = xMax.max(x); yMax = yMax.max(y)
    }
    val (xDim, yDim) = (xMax - xMin + 1, yMax - yMin + 1)
    (placedChars.map { case ((x, y), c) => ((x - xMin, y - yMin), c) }.toMap, (xDim, yDim), usedXPoints, crossed, placedAtOrigin)
  }
}

object Generator {
  def main(args: Array[String]) = {
    val lines = Source.fromFile("words").getLines
    val words = lines.map(_.trim.toUpperCase).filter(_.nonEmpty).toList.sorted.zipWithIndex.map { case (w, i) => Word(i, w) }
    val junctions = for {
      (w1, wi) <- words.zipWithIndex; w2 <- words.slice(wi + 1, words.length)
    } yield {
      val xpoints = for {
        (c1, i1) <- w1.chars.zipWithIndex; (c2, i2) <- w2.chars.zipWithIndex; if c1 == c2
      } yield XPoint(c1, w1, i1, w2, i2)
      ((w1.index, w2.index), xpoints)
    }
    val possibleXPoints = junctions.groupBy(_._1).mapValues(_.flatMap(_._2).toIndexedSeq)
    val spec = new CWSpec(words.toArray, possibleXPoints)

    //    val cw = new CWFactory(spec).generateRandomCandidate(new Random)
    //    cw.print(cw.placement._1)

    val rng = new MersenneTwisterRNG
    val operators: List[EvolutionaryOperator[Crossword]] = List(
      new CWCrossover(1),
      new CWMutation(spec))

    val pipeline = new EvolutionPipeline[Crossword](operators);

    val engine = new GenerationalEvolutionEngine[Crossword](new CWFactory(spec),
      pipeline,
      new CWEvaluator,
      new RouletteWheelSelection,
      rng)
    val cw = engine.evolve(100,
      3,
      new TargetFitness(0, false), // Continue until a perfect solution is found...
      new ElapsedTime(10 * 1000))
    cw.print(cw.placement)
    println(cw.placedAtOrigin + " at origin, crossed at " + cw.crossed + " points, applied " + cw.usedXPoints + " edges")

  }
}
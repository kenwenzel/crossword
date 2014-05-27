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
    val conflicts = candidate.placement._2
    conflicts
  }

  override def isNatural = false
}

object Orientation extends Enumeration {
  type Orientation = Value
  val OUnknown, OHorizontal, OVertical = Value
}
import Orientation._

class CWMutation(val spec: CWSpec) extends EvolutionaryOperator[Crossword] {
  override def apply(selectedCandidates: java.util.List[Crossword], rng: Random) = {
    val mutated = new ArrayList[Crossword](selectedCandidates.size)
    for (cw <- selectedCandidates) mutated.add(mutate(cw, rng))
    mutated
  }

  def mutate(cw: Crossword, rng: Random): Crossword = {
    val remove = mutable.Set[XPoint]()
    val add = mutable.Set[XPoint]()
    for (i <- 0 to rng.nextInt(5) + 1) {
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
      if (!remove.contains(entry._2) || rng.nextFloat < .7) {
        fixed += entry._1._1
        fixed += entry._1._2
        true
      } else false
    }
    val newOrientations = cw.orientations.clone
    for (i <- cw.orientations.indices) {
      if (!fixed(i)) newOrientations(i) = OUnknown
    }

    val newXPoints2 = newXPoints ++ (add.flatMap { xpoint =>
      val o1 = newOrientations(xpoint.w1.index)
      val o2 = newOrientations(xpoint.w2.index)
      if (o1 == OUnknown || o2 == OUnknown || o1 == o2) {
        // update orientations
        if (o1 == OUnknown) newOrientations(xpoint.w1.index) = o2 match {
          case OUnknown => OHorizontal; case OHorizontal => OVertical; case OVertical => OHorizontal
        }
        if (o2 == OUnknown) newOrientations(xpoint.w2.index) = newOrientations(xpoint.w1.index) match {
          case OHorizontal => OVertical; case OVertical => OHorizontal
        }
        Some(xpoint)
      } else None
    } map { xpoint => ((xpoint.w1.index, xpoint.w2.index), xpoint) })

    new Crossword(spec, newXPoints2, newOrientations)
  }
}

class CWCrossover(crossoverPoints: Int) extends AbstractCrossover[Crossword](crossoverPoints) {
  override def mate(parent1: Crossword, parent2: Crossword, numberOfCrossoverPoints: Int, rng: Random) = {
    null
  }
}

class CWFactory(val spec: CWSpec) extends AbstractCandidateFactory[Crossword] {
  override def generateRandomCandidate(rng: Random) = {
    val orientations = Array.fill(spec.words.length)(OUnknown)
    val xpoints = spec.possibleXPoints flatMap {
      // create xpoint with possibility of 70%
      case ((w1, w2), xpoints) if xpoints.nonEmpty && rng.nextFloat < .7 => {
        val (ow1, ow2) = (orientations(w1), orientations(w2))
        if (ow1 == OUnknown || ow2 == OUnknown || ow1 == ow2) {
          // update orientations
          if (ow1 == OUnknown) orientations(w1) = ow2 match {
            case OUnknown => OHorizontal; case OHorizontal => OVertical; case OVertical => OHorizontal
          }
          if (ow2 == OUnknown) orientations(w2) = orientations(w1) match {
            case OHorizontal => OVertical; case OVertical => OHorizontal
          }
          val index = rng.nextInt(xpoints.length)
          Some(((w1, w2), xpoints(index)))
        } else Nil
      }
      case _ => Nil
    }
    new Crossword(spec, xpoints, orientations)
  }
}

case class Word(index: Int, chars: String)
case class XPoint(c: Char, w1: Word, i1: Int, w2: Word, i2: Int)

class CWSpec(val words: Array[Word], val possibleXPoints: Map[(Int, Int), IndexedSeq[XPoint]]) {
  val possibleXPointsSeq = possibleXPoints.values.toIndexedSeq.flatten
}

class Crossword(val spec: CWSpec, val xpoints: Map[(Int, Int), XPoint], val orientations: Array[Orientation]) {
  def print(grid: Map[(Int, Int), Char]) {
    var (xMin, yMin) = (Int.MaxValue, Int.MaxValue)
    var (xMax, yMax) = (Int.MinValue, Int.MinValue)
    grid.keys.foreach {
      case (x, y) =>
        xMin = xMin.min(x); yMin = yMin.min(y)
        xMax = xMax.max(x); yMax = yMax.max(y)
    }
    val (xDim, yDim) = (xMax - xMin + 1, yMax - yMin + 1)
    var result = Array.fill[Char](xDim, yDim)(' ')
    grid.foreach { case ((x, y), c) => result(x - xMin)(y - yMin) = c }
    println(result.map(_.mkString(" ")).mkString("\n"))
  }

  def placement: (Map[(Int, Int), Char], Int) = {
    val words = spec.words
    val points4Word = xpoints.toList.map { case ((iw1, iw2), xpoint) => (iw1, xpoint) }.groupBy(_._1).mapValues {
      v => v.map(_._2)
    }

    val grid: mutable.Map[(Int, Int), Char] = mutable.Map.empty
    var conflicts = 0
    var placed: Set[Int] = Set.empty
    def place(w: Word, x0: Int = 0, y0: Int = 0) {
      if (!placed.contains(w.index)) {
        placed += w.index
        val orientation = orientations(w.index)
        val xy = orientation match {
          case OHorizontal | OUnknown => (x0 until x0 + w.chars.length).map((_, y0))
          case OVertical => (y0 until y0 + w.chars.length).map((x0, _))
        }
        var conflict = false
        for ((c, (x, y)) <- w.chars.view.zip(xy)) {
          grid.get(x, y) match {
            case Some(existing) if existing == c => // no conflict
            case Some(existing) => // conflict
              conflict = true
              grid((x, y)) = '?'
            case None => grid((x, y)) = if (x == 0 && y == 0) '#' else c
          }
        }
        if (conflict) conflicts += 1
        points4Word.get(w.index).map {
          _.foreach { xpoint =>
            val (x1, y1) = orientation match {
              case OHorizontal => (x0 + xpoint.i1, y0)
              case OVertical => (x0, y0 + xpoint.i1)
            }
            val (x2, y2) = orientations(xpoint.w2.index) match {
              case OHorizontal => (x1 - xpoint.i2, y0)
              case OVertical => (x1, y1 - xpoint.i2)
            }
            place(xpoint.w2, x2, y2)
          }
        }
      }
    }
    words foreach (place(_))
    (grid.toMap, conflicts)
  }
}

object Generator {
  def main(args: Array[String]) = {
    val lines = Source.fromFile("/home/ken/Projects/cwc/words").getLines
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
    //possibleXPoints foreach (println(_))

    //    val cw = new CWFactory(spec).generateRandomCandidate(new Random)
    //    cw.print(cw.placement._1)

    val rng = new MersenneTwisterRNG();
    val operators: List[EvolutionaryOperator[Crossword]] = List(
      // new CWCrossover(1),
      new CWMutation(spec))

    // Mutate the order of cells within individual rows.
    //                operators.add(new SudokuRowMutation(new PoissonGenerator(2, rng),
    //                                                    new DiscreteUniformGenerator(1, 8, rng)));

    val pipeline = new EvolutionPipeline[Crossword](operators);

    val engine = new GenerationalEvolutionEngine[Crossword](new CWFactory(spec),
      pipeline,
      new CWEvaluator,
      new RouletteWheelSelection,
      rng);
    val cw = engine.evolve(100,
      3,
      new TargetFitness(0, false), // Continue until a perfect solution is found...
      new ElapsedTime(5 * 1000));
    val p = cw.placement
    cw.print(p._1)
    println(p._2)

  }
}
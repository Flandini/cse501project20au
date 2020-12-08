
import algebra.lattice.Lattice
import scala.math.BigInt

// Draws heavily from SPA book by Moller and Schwartzbavh and IKOS static analyzer by NASA

trait IntervalLatticeElement
case class Interval(lo: BigInt, hi: BigInt) extends IntervalLatticeElement
case object Bottom extends IntervalLatticeElement

object Interval extends Lattice[IntervalLatticeElement]  {
    val most = BigInt(Long.MaxValue)
    val least = BigInt(Long.MinValue)
    
    def fromInt(i: Int): IntervalLatticeElement = fromBigInts(BigInt(i), BigInt(i))
    def fromInts(lo: Int, hi: Int): IntervalLatticeElement = fromBigInts(BigInt(lo), BigInt(hi))
    def fromBigInts(lo: BigInt, hi: BigInt): IntervalLatticeElement = Interval(lo, hi)
    def min(a: BigInt, b: BigInt, c: BigInt, d: BigInt): BigInt = a.min(b).min(c).min(d)
    def max(a: BigInt, b: BigInt, c: BigInt, d: BigInt): BigInt = a.max(b).max(c).max(d)

    def join(lhs: IntervalLatticeElement, rhs: IntervalLatticeElement): IntervalLatticeElement = (lhs, rhs) match {
        case (Bottom, other) => other
        case (other, Bottom) => other
        case (Interval(a, b), Interval(c, d)) => Interval(a.min(c), b.max(d))
    }

    def meet(lhs: IntervalLatticeElement, rhs: IntervalLatticeElement): IntervalLatticeElement = (lhs, rhs) match {
        case (Bottom, other) => Bottom
        case (other, Bottom) => Bottom
        case (Interval(a,b), Interval(c,d)) => Interval(a.max(c), b.min(d))
    }

    // Pairwise interval widening
    def widen(lhs: IntervalLatticeElement, rhs: IntervalLatticeElement): IntervalLatticeElement = (lhs, rhs) match {
        case (Bottom, other) => other
        case (other, Bottom) => other
        case (Interval(a,b), Interval(c,d)) => {
            val lo = if (a <= c) a else least
            val hi = if (d <= b) b else most
            fromBigInts(lo, hi)
        }
    }

    // Pairwise interval narrowing
    def narrow(lhs: IntervalLatticeElement, rhs: IntervalLatticeElement): IntervalLatticeElement = (lhs, rhs) match {
        case (Bottom, other) => other
        case (other, Bottom) => other
        case (Interval(a,b), Interval(c,d)) => {
            val lo = if (a == least || a == most) c else a
            val hi = if (b == most || b == least) d else b
            fromBigInts(lo, hi)
        }
    }

    def plus(a: IntervalLatticeElement, b: IntervalLatticeElement): IntervalLatticeElement = (a,b) match {
        case (Bottom, _) => Bottom
        case (_, Bottom) => Bottom
        case (Interval(x,y), Interval(u,v)) => Interval(x+u, y+v)
    }

    def minus(a: IntervalLatticeElement, b: IntervalLatticeElement): IntervalLatticeElement = (a,b) match {
        case (Bottom, _) => Bottom
        case (_, Bottom) => Bottom
        case (Interval(x,y), Interval(u,v)) => Interval(x-u, y-v)
    }
    
    def mult(a: IntervalLatticeElement, b: IntervalLatticeElement): IntervalLatticeElement = (a,b) match {
        case (Bottom, _) => Bottom
        case (_, Bottom) => Bottom
        case (Interval(x,y), Interval(u,v)) => {
            val ll: BigInt = x * u
            val lu: BigInt = x * v
            val ul: BigInt = y * u
            val uu: BigInt = y * v
            Interval(min(ll, lu, ul, uu), max(ll, lu, ul, uu))
        }
    }

    def div(a: IntervalLatticeElement, b: IntervalLatticeElement): IntervalLatticeElement = (a,b) match {
        case (Bottom, _) => Bottom
        case (_, Bottom) => Bottom
        case (Interval(x,y), Interval(u,v)) =>
            if (u <= 0 && v <= 0) {
                val splitLower = fromBigInts(u, -1)
                val splitUpper = fromBigInts(1, v)
                join(div(a, splitLower), div(a, splitUpper))
            } else if (x <= 0 && y <= 0) {
                val splitLower = fromBigInts(x, -1)
                val splitHigher = fromBigInts(1, y)
                join(join(div(splitLower, b), div(splitHigher, b)), fromInt(0))
            } else {
                val ll: BigInt = x / u
                val lu: BigInt = x / v
                val ul: BigInt = y / u
                val uu: BigInt = y / v
                Interval(min(ll, lu, ul, uu), max(ll, lu, ul, uu))
            }
    }
}

object IntervalAnalysis {
}


object IntervalAnalysisTest {
    import Interval._
    import IntervalAnalysis._

    def main(args: Array[String]): Unit = {
        println(div(Interval.fromInt(4), Interval.fromInts(-2, 2)))
    }
}
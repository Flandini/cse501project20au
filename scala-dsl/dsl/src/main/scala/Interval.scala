
import algebra.lattice.Lattice
import scala.math.BigInt

// Draws heavily from SPA book by Moller and Schwartzbavh and IKOS static analyzer by NASA

trait IntervalLatticeElement
case class Interval(lo: BigInt, hi: BigInt) extends IntervalLatticeElement
case object Bottom extends IntervalLatticeElement

object Interval extends Lattice[IntervalLatticeElement]  {
    type IntervalBinop = (IntervalLatticeElement, IntervalLatticeElement) => IntervalLatticeElement

    val most = BigInt(Long.MaxValue)
    val least = BigInt(Long.MinValue)
    
    def fromBigInts(lo: BigInt, hi: BigInt): IntervalLatticeElement = Interval(lo, hi)
    def fromInt(i: Int): IntervalLatticeElement = fromBigInts(BigInt(i), BigInt(i))
    def fromInts(lo: Int, hi: Int): IntervalLatticeElement = fromBigInts(BigInt(lo), BigInt(hi))
    def fromSignAndWidth(signed: Boolean, width: Int): IntervalLatticeElement =
        Interval.fromBigInts(
            if (signed) BigInt(-(Math.pow(2, (width - 1)).toInt)) else BigInt(0),
            if (signed) BigInt(Math.pow(2, width-1).toInt - 1) else BigInt(Math.pow(2, width).toInt - 1)
        )
    def defaultLengthInterval: IntervalLatticeElement =
        Interval.fromSignAndWidth(false, 64)

    def min(a: BigInt, b: BigInt, c: BigInt, d: BigInt): BigInt = a.min(b).min(c).min(d)
    def max(a: BigInt, b: BigInt, c: BigInt, d: BigInt): BigInt = a.max(b).max(c).max(d)

    def intervalTop: IntervalLatticeElement = Interval.fromBigInts(least, most) 

    def containsNegatives(i: IntervalLatticeElement): Boolean = i match {
        case Bottom => false
        case Interval(lo, hi) => lo <= 0 || hi <= 0
    }

    def join(lhs: IntervalLatticeElement, rhs: IntervalLatticeElement): IntervalLatticeElement = (lhs, rhs) match {
        case (Bottom, other) => other
        case (other, Bottom) => other
        case (Interval(a, b), Interval(c, d)) => Interval(a.min(c), b.max(d))
    }

    def opt_join(lhs: Option[IntervalLatticeElement], rhs: Option[IntervalLatticeElement]): Option[IntervalLatticeElement] = 
        lhs match {
            case None => rhs
            case Some(l) => rhs match {
                case None => lhs
                case Some(r) => Some(join(l,r))
            }
        }

    def meet(lhs: IntervalLatticeElement, rhs: IntervalLatticeElement): IntervalLatticeElement = (lhs, rhs) match {
        case (Bottom, other) => Bottom
        case (other, Bottom) => Bottom
        case (Interval(a,b), Interval(c,d)) => Interval(a.max(c), b.min(d))
    }

    def meet(lhs: Option[IntervalLatticeElement], rhs: Option[IntervalLatticeElement]): Option[IntervalLatticeElement] = 
        lhs match {
            case None => rhs
            case Some(l) => rhs match {
                case None => Some(l)
                case Some(r) => Some(meet(l,r))
            }
        }

    def binop(op: IntervalBinop, lhs: Option[IntervalLatticeElement], rhs: Option[IntervalLatticeElement]): Option[IntervalLatticeElement] =
        for {
            l <- lhs
            r <- rhs
        } yield op(l, r)

    def lte(lhs: IntervalLatticeElement, rhs: IntervalLatticeElement): Boolean = (lhs, rhs) match {
        case (Bottom, _) => true
        case (_, Bottom) => false
        case (Interval(a,b), Interval(c,d)) => a <= c && b <= d
    }

    // lhs from rhs
    def containedIn(lhs: IntervalLatticeElement, rhs: IntervalLatticeElement): Boolean = (lhs, rhs) match {
        case (Bottom, _) => false
        case (_, Bottom) => false
        case (Interval(a,b), Interval(c,d)) => c <= a && b <= d
    }

    def containedIn(lhs: Option[IntervalLatticeElement], rhs: Option[IntervalLatticeElement]): Boolean = lhs match {
        case None => true
        case Some(l) => rhs match {
            case None => true
            case Some(r) => containedIn(l, r)
        }
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
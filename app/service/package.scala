//import service.Game.Space
import scala.language.implicitConversions

package object service {

  /**
   * Point/cell representation
   * @param x coordinate
   * @param y coordinate
   */
  case class Point(x: Int = 0, y: Int = 0) {
    def inside(rect: Rect): Boolean = rect.contains(this)
  }

  object Point {
    implicit def apply(tuple: (Int, Int)): Point = Point(tuple._1, tuple._2)
  }

  /**
   * Rectangle representation
   * @param x1 upper-left corner, x coordinate
   * @param y1 upper-left corner, y coordinate
   * @param x2 lower-right corner, x coordinate
   * @param y2 lower-right corner, y coordinate
   */
  case class Rect(x1: Int = 0, y1: Int = 0, x2: Int = 0, y2: Int = 0) {
    require(x1 <= x2)
    require(y1 <= y2)

    def extend(p: Point): Rect = Rect(
      if (p.x < x1) p.x else x1,
      if (p.y < y1) p.y else y1,
      if (p.x > x2) p.x else x2,
      if (p.y > y2) p.y else y2
    )

    def zoom(left: Int, up: Int, right: Int, down: Int): Rect =
      Rect(x1 - left, y1 - up, x2 + right, y2 + down)

    def zoom(inc: Int): Rect = zoom(inc, inc, inc, inc)

    def intersect(other: Rect): Option[Rect] =
      if (other.x2 < x1 || other.x1 > x2 || other.y2 < y1 || other.y1 > y2) None
      else Some(Rect(
        if (x1 > other.x1) x1 else other.x1,
        if (y1 > other.y1) y1 else other.y1,
        if (x2 < other.x2) x2 else other.x2,
        if (y2 < other.y2) y2 else other.y2
      ))

    def union(other: Rect): Rect = Rect(
      if (x1 < other.x1) x1 else other.x1,
      if (y1 < other.y1) y1 else other.y1,
      if (x2 > other.x2) x2 else other.x2,
      if (y2 > other.y2) y2 else other.y2
    )

    def contains(p: Point): Boolean = x1 <= p.x && p.x <= x2 && y1 <= p.y && p.y <= y2
  }

  object Rect {
    def apply(p: Point): Rect = Rect(p, p)

    def apply(p1: Point, p2: Point): Rect = {
      val (xs, ys) = (
        if (p1.x < p2.x) (p1.x, p2.x) else (p2.x, p1.x),
        if (p1.y < p2.y) (p1.y, p2.y) else (p2.y, p1.y)
        )
      Rect(xs._1, ys._1, xs._2, ys._2)
    }
  }

//  implicit class IntTuple2(tuple: (Int, Int)) {
//
//    def min: Int = if (tuple._1 < tuple._2) tuple._1 else tuple._2
//
//    def max: Int = if (tuple._1 > tuple._2) tuple._1 else tuple._2
//
//    def sort: (Int, Int) = if (tuple._1 < tuple._2) (tuple._1, tuple._2) else (tuple._2, tuple._1)
//
//    def toPoint: Point = Point(tuple)
//
//  }

//  implicit class IntTuple4(tuple: (Int, Int, Int, Int)) {
//    def toRect: Rect = Rect(tuple._1, tuple._2, tuple._3, tuple._4)
//  }


  implicit class IntList(list: List[Int]) {
    def groupSeq(minGap: Int = 1): List[(Int, Int)] =
      list.sorted.foldLeft[List[(Int, Int)]](Nil)(
        (res, n) => res match {
          case Nil => List((n, 1))
          case head :: tail =>
            if (n - head._1 <= minGap) (n, head._2 + n - head._1) :: tail
            else (n, 1) :: res
        }
      ).map(p => (p._1 + 1 - p._2, p._1))
  }

//  implicit class PointSet(grid: Set[Point]) {
//    def bounds: Option[Rect] = grid.foldLeft[Option[Rect]](None)((rectOpt, point) => rectOpt match {
//      case Some(rect) => Some(rect.extend(point))
//      case None => Some(Rect(point))
//    })
//
//    def toSpace: Space = Space(grid)
//
//    def split(borders: Rect, minGap: Int): List[(Rect, Set[Point])] = {
//      grid.map(_.x).toList.sorted.groupSeq(minGap) flatMap {
//        d =>
//          val points = grid.filter((d._1, borders.y1, d._2, borders.y2).toRect.contains)
//          points.bounds.map(_ -> points)
//      }
//    }
//
//  }

  implicit class RectSet(set: Set[Rect]) {

    // TODO: implement effective rectangle overlaping detection
    def group(maxGap: Int): Set[Rect] = set.foldLeft(set)(
      (res, r1) => res.map(
        r2 => r1 intersect r2 match {
          case Some(r3) => r1 union r2
          case None => r2
        }
      )
    )
  }

}

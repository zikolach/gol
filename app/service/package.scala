//import service.Game.Space
import scala.language.implicitConversions
import service.RuleGame._

package object service {

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

  implicit class PointSet(grid: Set[Point]) {
    def bounds: Option[Rect] = grid.foldLeft[Option[Rect]](None)((rectOpt, point) => rectOpt match {
      case Some(rect) => Some(rect.extend(point))
      case None => Some(Rect(point))
    })

    def split(borders: Rect, minGap: Int): List[(Rect, Set[Point])] = {
      grid.map(_.x).toList.sorted.groupSeq(minGap) flatMap {
        d =>
          val points = grid.filter((d._1, borders.ul.y, d._2, borders.lr.y).toRect.contains)
          points.bounds.map(_ -> points)
      }
    }

  }

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

import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.mutable._


@RunWith(classOf[JUnitRunner])
class GameTest extends Specification {
  import service._
  import service.RuleGame._

  "Tuple" should {

    "work as Point" in {
      (1, 2).toPoint === Point(1,2)
    }

    "calculate max" in {
      (1, 2).max === 2
    }

    "calculate max" in {
      (1, 2).min === 1
    }

  }

  "List of Ints" should {
    "be able to group sequences" in {
      List(1,2,3,5,6,8,9).groupSeq(1).sortBy(_._1) === List((1,3),(5,6),(8,9))
      List(1,2,3,5,6,8,9).reverse.groupSeq(1).sortBy(_._1) === List((1,3),(5,6),(8,9))
      List(1,2,5,6,8,9).reverse.groupSeq(2).sortBy(_._1) === List((1,2),(5,9))
      List(-2,-1,44,45,47,99).reverse.groupSeq(2).sortBy(_._1) === List((-2,-1),(44,47),(99,99))
      List(-2,-1,5,6,10).reverse.groupSeq(4).sortBy(_._1) === List((-2,-1),(5,10))
    }
  }

  "Set of Points" should {
    "have bounds" in {
      Set((1,1).toPoint, (2,2).toPoint).bounds === Some(Rect(1,1,2,2))
    }
    "split into two or more" in {
      val space = Set(
        (1,1).toPoint,
        (4,1).toPoint,
        (5,2).toPoint)
      space.split(space.bounds.get, 2).sortBy(_._1.ul.x) === List(
        (Rect(1,1,1,1), Set((1,1).toPoint)),
        (Rect(4,1,5,2), Set((4,1).toPoint,(5,2).toPoint))
      )
    }
  }

  "Rect" should {
    "intersect other" in {
      val rect = Rect(3,3,5,5)
      rect.intersect(Rect(4,4,6,6)) === Some(Rect(4,4,5,5))
      rect.intersect(Rect(4,2,6,4)) === Some(Rect(4,3,5,4))
      rect.intersect(Rect(2,2,4,4)) === Some(Rect(3,3,4,4))
      rect.intersect(Rect(2,4,4,6)) === Some(Rect(3,4,4,5))
      rect.intersect(Rect(2,2,6,6)) === Some(Rect(3,3,5,5))
      rect.intersect(Rect(1,1,2,2)) === None
      rect.intersect(Rect(1,1,6,2)) === None
    }
    "union with other" in {
      val rect = Rect(3,3,5,5)
      rect.union(Rect(4,4,6,6)) === Rect(3,3,6,6)
      rect.union(Rect(4,2,6,4)) === Rect(3,2,6,5)
    }
    "extend by point" in {
      val rect = Rect(3,3,5,5)
      rect.extend((2,6)) === (2,3,5,6).toRect
    }
  }

//  "Set of Rects" should {
//    "find intersects" in {
//      val set = Set(
//        (1,1,2,2).toRect,
//        (3,3,4,5).toRect,
//        (3,1,4,2).toRect
//      )
//      set.group(1) === set
//
//      (set + (1,1,4,5).toRect).group(1) === Set((1,1,4,5).toRect)
//      (set + (3,1,4,4).toRect).group(1) === Set((1,1,2,2).toRect, (3,1,4,5).toRect)
//
//    }
//  }

}

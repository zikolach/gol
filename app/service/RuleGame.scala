package service

import akka.actor.Actor
import scala.language.postfixOps
import play.api.libs.concurrent.Execution.Implicits._

import Game._

import RuleGame._
import play.api.libs.iteratee.Concurrent
import scala.concurrent.duration._
import Rules._
import play.Logger
import scala.util.Random
import scala.language.implicitConversions

class RuleGame(rulesName: String) extends Actor {

  println(rulesName)

  implicit val rules = RuleGame.rules.get(rulesName) match {
    case Some(r) => r
    case None =>
      Logger.warn(s"Rules [$rulesName] not found, used default rules.")
      RuleGame.defaultRules
  }

  var currentSpace: PersistedSpace = rules.initialSpace
  var paused = true
  val (gameEnumerator, gameChannel) = Concurrent.broadcast[String]

  def sendUpdate() = gameChannel push s"UPDATE ${self.path.toString}\n${currentSpace.cells.map(c => s"${c._1.x}:${c._1.y}:${c._2}").mkString("\n")}"

  def sendJoin(username: String) = gameChannel push s"JOINED $username TO ${self.path.toString}"

  def sendClose() = gameChannel.eofAndEnd()

  def step() {
    if (!paused) {
      context.system.scheduler.scheduleOnce(100 millis) {
        self ! Step
      }
      currentSpace = currentSpace.transform()
      sendUpdate()
    }
  }

  def receive: Actor.Receive = working

  def working: Actor.Receive = {
    case Join(username) =>
      sender ! Joined(gameEnumerator)
      sendJoin(username)
    case Touch(x, y) =>
      currentSpace = rules.touch(currentSpace, (x, y).toPoint)
      sendUpdate()
    case Pause =>
      paused = !paused
      step()
    case Stop =>
      sendClose()
      context.become(stopping)
      context.system.scheduler.scheduleOnce(1 second) {
        context.stop(self)
      }
    case Step => step()
  }

  def stopping: Receive = {
    case _ => /* ignore all messages while stopping */
  }

}


object RuleGame {

  trait State {
    //override def toString = this.getClass.getSimpleName
  }

  implicit class PointTuple(tuple: (Int, Int)) {
    def toPoint: Point = Point(tuple._1, tuple._2)

    def toRect: Rect = Rect(Point(tuple._1, tuple._2), Point(tuple._1, tuple._2))

    def min: Int = if (tuple._1 < tuple._2) tuple._1 else tuple._2

    def max: Int = if (tuple._1 > tuple._2) tuple._1 else tuple._2
  }

  implicit class RectTuple(tuple: (Int, Int, Int, Int)) {
    def toRect: Rect = Rect(Point(tuple._1, tuple._2), Point(tuple._3, tuple._4))
  }

  case class Point(x: Int, y: Int) {
    def toRect: Rect = (x, y).toRect
  }

  object Point {
    implicit def apply(tuple: (Int, Int)): Point = tuple.toPoint
  }

  case class Rect(ul: Point, lr: Point) {
    require(ul.x <= lr.x)
    require(ul.y <= lr.y)

    def extend(point: Point): Rect =
      ((point.x, ul.x).min, (point.y, ul.y).min, (point.x, lr.x).max, (point.y, lr.y).max).toRect

    def zoom(px: Int): Rect = zoom(px, px, px, px)

    def zoom(left: Int, up: Int, right: Int, down: Int): Rect =
      (ul.x - left, ul.y - up, lr.x + right, lr.y + down).toRect

    def intersect(other: Rect): Option[Rect] =
      if (other.lr.x < ul.x || other.ul.x > lr.x || other.lr.y < ul.y || other.ul.y > lr.y) None
      else Some(Rect(
        (ul.x, other.ul.x).max, (ul.y, other.ul.y).max,
        (lr.x, other.lr.x).min, (lr.y, other.lr.y).min
      ))

    def union(other: Rect): Rect = Rect(
      (ul.x, other.ul.x).min, (ul.y, other.ul.y).min,
      (lr.x, other.lr.x).max, (lr.y, other.lr.y).max
    )

    def contains(p: Point): Boolean = ul.x <= p.x && p.x <= lr.x && ul.y <= p.y && p.y <= lr.y
  }

  object Rect {
    def apply(point: Point): Rect = Rect(point, point)
    def apply(x1: Int, y1: Int, x2: Int, y2: Int): Rect = Rect((x1, y1).toPoint, (x2, y2).toPoint)
  }

  /**
   * Abstract rules
   */
  abstract class Rules {

    import Rules._

    // transform between generations
    val transform: TransformFunc
    // default cell state
    val defaultState: State

    // initial space state
    def initialSpace: PersistedSpace

    // interaction with space
    def touch(space: PersistedSpace, point: Point): PersistedSpace

    // list of states which are stored between generations
    val persistStates: Set[State]

    // persist generation of cells inside fixed frame/rectangle
    def persist(space: Space, rectOpt: Option[Rect]): PersistedSpace = {
      val cells: Map[Point, State] = rectOpt.map(_.zoom(1)) match {
        case None => Map.empty
        case Some(rect) => (for {
          x <- rect.ul.x to rect.lr.x
          y <- rect.ul.y to rect.lr.y
          p <- Some((x, y).toPoint)
          s <- Some(space(p)) if persistStates.contains(s)
        } yield p -> space(p)).toMap
      }
      val newSpace: Space = (point: Point) => cells.get(point) match {
        case Some(state) => state
        case None => defaultState
      }
      PersistedSpace(cells, newSpace)
    }
  }

  object Rules {

    // functional representation of space
    type Space = Point => State
    // transformation function type
    type TransformFunc = Space => Space
    // persist function type
    type PersistFunc = (Space, Option[Rect]) => PersistedSpace

    // persisted space representation
    sealed case class PersistedSpace(cells: Map[Point, State], space: Space) {

      lazy val bounds: Option[Rect] = cells.keys.foldLeft[Option[Rect]](None)((rectOpt, point) => rectOpt match {
        case Some(rect) => Some(rect.extend(point))
        case None => Some(Rect(point))
      })

      override def toString: String = bounds match {
        case None => ""
        case Some(rect) =>
          (for {
            y <- rect.ul.y to rect.lr.y
            x <- rect.ul.x to rect.lr.x
          } yield s"${ if (cells.contains((x, y).toPoint)) "O" else "." } ${ if (x == rect.lr.x) "\n" else "" }").mkString
      }

    }

    // utility classes
    implicit class HyperSpace(space: Space)(implicit rules: Rules) {
      def transform(): Space = rules.transform(space)
    }

    implicit class PersistableSpace(space: Space)(implicit rules: Rules) {
      def persist(rectOpt: Option[Rect]): PersistedSpace = rules.persist(space, rectOpt)
    }

    implicit class TransformablePersistedSpace(space: PersistedSpace)(implicit rules: Rules) {
      def transform(): PersistedSpace = rules.persist(rules.transform(space.space), space.bounds)
    }

    implicit class TouchableSpace(space: PersistedSpace)(implicit rules: Rules) {
      def touch(point: Point): PersistedSpace = rules.touch(space, point)
    }

  }

  /**
   * Standard Game of Life Conway rules
   * Cases:
   * 2 neighbours - stay alive
   * 3 neighbours - birth
   * 1 or 4+ neighbours - death
   */
  abstract class S2B3 extends Rules {

    import Rules._

    case object DEAD extends State

    case object ALIVE extends State

    val persistStates: Set[State] = Set(ALIVE)

    val transform: TransformFunc =
      (space: Space) => {
        (p: Point) =>
          (for {
            xx <- p.x - 1 to p.x + 1
            yy <- p.y - 1 to p.y + 1 if xx != p.x || yy != p.y
            p <- Some((xx, yy).toPoint)
          } yield space(p)).count(_ == ALIVE) match {
            case 2 => space((p.x, p.y).toPoint)
            case 3 => ALIVE
            case _ => DEAD
          }
      }

    val defaultState: State = DEAD

    lazy val initialSpace: PersistedSpace = persist({
      case _ => defaultState
    }, None)

    def touch(space: PersistedSpace, point: Point): PersistedSpace = {
      val s: Space = {
        case p if p == point => if (space.space(p) == DEAD) ALIVE else DEAD
        case p => space.space(p)
      }
      val r: Rect = space.bounds match {
        case None => point.toRect
        case Some(rect) => rect.extend(point)
      }
      persist(s, Some(r))
    }
  }

  // default S2B3 rules singleton
  object defaultS2B3 extends S2B3

  object randomS2B3 extends S2B3 {
    val initSize = 100
    override lazy val initialSpace: PersistedSpace = persist({
      case _ => if (Random.nextBoolean()) ALIVE else DEAD
    }, Some(Rect(Point(0, 0)).zoom(initSize / 2)))
  }

  // list of implementations
  lazy val defaultRules = defaultS2B3
  lazy val rules = Map(
    "S2B3" -> defaultRules,
    "Random S2B3 100x100" -> randomS2B3
  )

}

package service

import akka.actor._
import scala.util.Try
import play.api.libs.iteratee.{Iteratee, Enumerator, Concurrent}
import scala.concurrent.duration._
import play.api.libs.concurrent.Execution.Implicits._
import service.Game._
import scala.language.postfixOps
import scala.concurrent.Future
import play.api.libs.concurrent.Akka
import akka.pattern.ask
import akka.util.Timeout
import play.api.Play.current
import play.Logger
import service.Game.Touch
import service.Game.Join
import scala.Some
import service.Game.Joined
import akka.actor.Terminated
import scala.language.implicitConversions

class GameServer extends Actor {

  import service.GameServer._

  val (gameListEnumerator, gameListChannel) = Concurrent.broadcast[String]

  def receive: Actor.Receive = {

    case Create =>
      val gameId = context.children.size + 1
      val game = context.actorOf(Props[Game], s"game$gameId")
      context.watch(game)
      sender ! Created(game)
      gameListChannel push context.children.map(_.path.toString).mkString("\n")
      Logger.info(s"game ${game.path.toString} started")

    case Find(gameUrl) =>
      context.children.find(_.path.toString == gameUrl) match {
        case Some(game) => sender ! Found(game)
        case None => sender ! NotFound
      }

    case Terminated(game) =>
      Logger.info(s"game ${game.path.toString} stopped")
      gameListChannel.push(context.children.map(_.path.toString).mkString("\n"))

    case Subscribe =>
      sender ! Subscribed(gameListEnumerator)
      context.system.scheduler.scheduleOnce(2 second) {
        gameListChannel.push(context.children.map(_.path.toString).mkString("\n"))
      }

  }
}

object GameServer {


  def list(): Future[(Iteratee[String, _], Enumerator[String])] = {
    (gameServer ? Subscribe) map {
      case Subscribed(enumerator) =>
        val iteratee = Iteratee.consume[String]()
        (iteratee, enumerator)
    }
  }

  lazy val gameServer: ActorRef = Akka.system.actorOf(Props(classOf[GameServer]), "gameServer")

  implicit val timeout = Timeout(2 second)

  def connect(user: String, gameUrlOpt: Option[String]): Future[(Iteratee[String, _], Enumerator[String])] = {

    def join(game: ActorRef): Future[(Iteratee[String, _], Enumerator[String])] = {
      (game ? Join(user)) map {
        case Joined(enumerator) =>
          Logger.info("player joined")
          val iteratee = Iteratee.foreach[String] {
            msg =>
              msg.split('\n').toList match {
                case "TOUCH" :: x :: y :: Nil =>
                  game ! Touch(x.toString.toInt, y.toString.toInt)
                case "PAUSE" :: Nil =>
                  game ! Pause
                case "STOP" :: Nil =>
                  game ! Stop
                case cmd => Logger.warn(s"Unrecognized command $cmd")
              }
          }
          (iteratee, enumerator)
      }
    }

    (gameUrlOpt match {
      case Some(gameUrl) => gameServer ? Find(gameUrl)
      case None => gameServer ? Create
    }) flatMap {
      case Created(game) => join(game)
      case Found(game) => join(game)
    }
  }

  // actor messages

  case object Create

  trait Result {
    def game: ActorRef
  }

  case class Created(game: ActorRef) extends Result

  case class Find(gameUrl: String)

  case object NotFound

  case class Found(game: ActorRef) extends Result

  case object Subscribe

  case class Subscribed(enumerator: Enumerator[String])


}


class Game extends Actor {

  import service.Game._

  var space: Space = Space()
  var rectOpt: Option[Rect] = None
  var paused = true
  val (gameEnumerator, gameChannel) = Concurrent.broadcast[String]

  def sendUpdate() {
    gameChannel push s"UPDATE ${self.path.toString}\n${space.toSet(rectOpt).map(c => s"${c.x}:${c.y}").mkString("\n")}"
  }

  def nextStep() {
    if (!paused) context.system.scheduler.scheduleOnce(100 millis) { self ! Step }
    val (newState, newRect) = space.transform().persist(rectOpt)
    space = newState
    rectOpt = newRect
    sendUpdate()
  }

  def receive: Actor.Receive = {
    case Join(username) =>
      sender ! Joined(gameEnumerator)
      gameChannel push s"JOINED $username TO ${self.path.toString}"

    case Touch(x, y) =>
      //Logger.info(s"space touched at [$x:$y]")
      rectOpt match {
        case Some(rect) =>
          if (space(x, y))
            space --= Space((x, y))
          else {
            space ++= Space((x, y))
            rectOpt = Some(rect.extend((x,y).toPoint))
          }
        case None =>
          space = Space((x, y))
          rectOpt = Some(Rect(x, y, x, y))
      }
      //Logger.debug(s"current state \n${space.toString(rectOpt)}")
      sendUpdate()

    case Pause =>
      paused = !paused
      nextStep()

    case Stop =>
      gameChannel.eofAndEnd()
      context.become(stopping)
      context.system.scheduler.scheduleOnce(1 second) {
        context.stop(self)
      }

    case Step => nextStep()

  }

  def stopping: Receive = {
    case _ =>
  }

}

object Game {

  case class Touch(x: Int, y: Int)

  case object Step

  case object Pause

  case object Stop

  case class Join(username: String)

  case class Joined(enumerator: Enumerator[String])

  type Space = (Int, Int) => Boolean

  object Space {

    def apply(p: Point*): Space = (x: Int, y: Int) => p.contains((x, y).toPoint)

    def apply(points: Set[Point]): Space = (x: Int, y: Int) => points.contains((x, y))

    def apply(s: String): Space =
      (x: Int, y: Int) => Try(s.split('\n')(y).charAt(x)).getOrElse(".") == 'O'

    def empty: Space = (x: Int, y: Int) => false

  }

  implicit class PrintableSpace(state: Space) {
    def toString(rectOpt: Option[Rect]): String = rectOpt match {
      case None => ""
      case Some(rect) =>
        (for {
          y <- rect.y1 to rect.y2
          x <- rect.x1 to rect.x2
        } yield (if (state(x, y)) "O" else ".") + " " + (if (x == rect.x2) "\n" else "")).mkString
    }
  }

  implicit class TransformableSpace(state: Space) {
    def transform(): Space =
      (x: Int, y: Int) =>
        (for {
          xx <- x - 1 to x + 1
          yy <- y - 1 to y + 1 if xx != x || yy != y
        } yield state(xx, yy)).count(_ == true) match {
          case n if n < 2 || n > 3 => false
          case n if n == 3 => true
          case _ => state(x, y)
        }

    def persist(rectOpt: Option[Rect]): (Space, Option[Rect]) = {
      rectOpt match {
        case Some(rect) =>
          val table: Set[(Int, Int)] = (for {
            y <- rect.y1 - 1 to rect.y2 + 1
            x <- rect.x1 - 1 to rect.x2 + 1 if state(x, y)
          } yield (x, y)).toSet
          val (xs, ys) = (table.map(_._1), table.map(_._2))
          val newRect: Rect = Rect(xs.reduceOption(_ min _).getOrElse(0), ys.reduceOption(_ min _).getOrElse(0), xs.reduceOption(_ max _).getOrElse(0), ys.reduceOption(_ max _).getOrElse(0))
          ((x: Int, y: Int) => table.contains((x, y)), Some(newRect))
        case None =>
          ((x: Int, y: Int) => false, None)
      }
    }

    def ++(other: Space): Space =
      (x: Int, y: Int) => state(x, y) || other(x, y)

    def --(other: Space): Space =
      (x: Int, y: Int) => state(x, y) && !other(x, y)

    def toSet(rectOpt: Option[Rect]): Set[Point] = rectOpt match {
      case None => Set.empty
      case Some(rect) =>
        (for {
          y <- rect.y1 to rect.y2
          x <- rect.x1 to rect.x2 if state(x, y)
        } yield (x, y).toPoint).toSet
    }

  }

}

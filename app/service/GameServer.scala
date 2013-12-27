package service

import akka.actor._
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
      val game = context.actorOf(Props(classOf[RuleGame], "S2B3"), s"game$gameId")
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

  def list(): Future[(Iteratee[String, _], Enumerator[String])] = (gameServer ? Subscribe) map {
    case Subscribed(enumerator) => (Iteratee.consume[String](), enumerator)
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

object Game {

  case class Touch(x: Int, y: Int)

  case object Step

  case object Pause

  case object Stop

  case class Join(username: String)

  case class Joined(enumerator: Enumerator[String])

}

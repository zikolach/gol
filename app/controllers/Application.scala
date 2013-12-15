package controllers

import play.api.mvc._
import play.api.libs.iteratee.{Concurrent, Iteratee}
import scala.concurrent.{Future, ExecutionContext}
import ExecutionContext.Implicits.global
import scala.util.Try
import scala.annotation.tailrec

object Application extends Controller {

  val (out, channel) = Concurrent.broadcast[String]


  object GoL extends App {

    type GameState = (Int, Int) => Boolean
    var cnt: Long = 0

    def transform(state: GameState): GameState =
      (x: Int, y: Int) =>
        (for {
          xx <- x - 1 to x + 1
          yy <- y - 1 to y + 1 if xx != x || yy != y
        } yield {
          cnt = cnt + 1; state(xx, yy)
        }).count(_ == true) match {
          case n if n < 2 || n > 3 => false
          case n if n == 3 => true
          case _ => state(x, y)
        }

    def persist(state: GameState, upperLeft: (Int, Int), lowerRight: (Int, Int)): GameState = {
      val table: Set[(Int, Int)] = (for {
        y <- upperLeft._2 to lowerRight._2
        x <- upperLeft._1 to lowerRight._1 if state(x, y)
      } yield (x, y)).toSet
      (x: Int, y: Int) => table.contains((x, y))
    }

    def combine(s1: GameState, s2: GameState): GameState =
      (x: Int, y: Int) => s1(x, y) || s2(x, y)

    object GameState {
      def apply(p: (Int, Int)*): GameState = (x: Int, y: Int) => p.contains((x, y))

      def apply(s: String): GameState =
        (x: Int, y: Int) => Try(s.split('\n')(y).charAt(x)).getOrElse(".") == 'O'

      def toString(state: GameState, upperLeft: (Int, Int), lowerRight: (Int, Int)): String =
        (for {
          y <- upperLeft._2 to lowerRight._2
          x <- upperLeft._1 to lowerRight._1
        } yield (if (state(x, y)) "O" else ".") + " " + (if (x == lowerRight._1) "\n" else "")).mkString
    }

    private var gameStarted = false
    private var gamePaused = false
    private var (width, height) = (0, 0)
    private var currentState: Option[GameState] = None

    def startGame(w: Int, h: Int) {
      require(w > 0)
      require(h > 0)
      width = w
      height = h
      if (!gameStarted) {
        currentState = Some(GameState())
        Future(gameLoop())
        gameStarted = true
        println("game started")
      }
    }

    def togglePause() {
      gamePaused = !gamePaused
      println(s"game ${if (gamePaused) "paused" else "resumed"}")
    }

    def touchSpace(x: Int, y: Int) {
      println(s"add life at ($x:$y)")
      println(s"width: $width  height: $height")
      currentState = Some(combine(currentState.getOrElse(GameState()), GameState((x, y))))
//      println(GameState.toString(currentState.get, (0,0), (width - 1, height - 1)))
      channel push "UPDATE\n" + GameState.toString(currentState.get, (0,0), (width - 1, height - 1))
    }

    @tailrec
    def gameLoop() {
      if (!gamePaused) {
        currentState = Some(GoL.persist(GoL.transform(currentState.getOrElse(GameState())), (0,0), (width - 1, height - 1)))
        channel push "UPDATE\n" + GameState.toString(currentState.get, (0,0), (width - 1, height - 1))
      }
      scala.concurrent.blocking {
        Thread.sleep(100)
      }
      if (gameStarted) gameLoop()
    }
  }

  def index = Action {
    Ok(views.html.index("Game of Life!!!"))
  }

  def status = WebSocket.using[String] {
    request =>

      val in = Iteratee.foreach[String] {
        case msg =>
          msg.split('\n').toList match {
            case "START" :: w :: h :: Nil =>
              GoL.startGame(w.toInt, h.toInt)
            case "TOUCH" :: x :: y :: Nil =>
              GoL.touchSpace(x.toInt, y.toInt)
            case "STOP" :: Nil =>
              println("pause pressed")
              GoL.togglePause()
            case _ =>
              println("!!!!!!" + msg)
          }
      }
      (in, out)
  }

}
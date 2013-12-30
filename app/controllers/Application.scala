package controllers

import play.api.mvc._
import play.api.Play
import play.api.Play.current
import service.GameServer


object Application extends Controller {

  def index = Action {
    val hostname = scala.util.Properties.envOrElse("hostname", Play.configuration.getString("server.hostname").getOrElse("localhost"))
    Ok(views.html.index("Game of Life!!!", hostname))
  }

  def connect = WebSocket.async[String] { request  =>
    val user = request.getQueryString("player")
    require(user.isDefined)
    val gameUrl = request.getQueryString("game")
    val gameParams = Map(
      'gameStrategy -> request.getQueryString("gameStrategy"),
      'spaceType -> request.getQueryString("spaceType"),
      'spaceWidth -> request.getQueryString("spaceWidth"),
      'spaceHeight -> request.getQueryString("spaceHeight")
    )
    GameServer.connect(user.getOrElse("default"), gameUrl, gameParams.filter(_._2.getOrElse("").length > 0))

  }

  def list = WebSocket.async[String] { request  =>
    GameServer.list()
  }
}
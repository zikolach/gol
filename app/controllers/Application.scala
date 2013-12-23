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
    val user = request.getQueryString("user")
    require(user.isDefined)
    val gameUrl = request.getQueryString("game")
    GameServer.connect(user.getOrElse("default"), gameUrl)

  }

  def list = WebSocket.async[String] { request  =>
    GameServer.list()
  }
}
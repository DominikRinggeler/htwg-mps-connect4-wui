package controllers

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import mps.Controller.GameController
import mps.Model.{Grid, HumanPlayer}
import mps.WuiActor
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.mvc._
import utils.{OutputField, Wui}

import scala.concurrent._
import scala.concurrent.duration._

object Application extends Controller {

  implicit val timeout = Timeout(10.seconds)

  var controller = new GameController(new Grid(6,7))
  controller.addPlayer(new HumanPlayer(2, "Player 2"))
  controller.addPlayer(new HumanPlayer(1, "Player 1"))

  val system = ActorSystem("Connect4System")
  val wuiActor:ActorRef = system.actorOf(Props[WuiActor], name = "wui")

  def start = Action { implicit request =>
    val line =  request.getQueryString("line")
    System.out.print("LINE: " + line)

    controller.removePlayers()
    controller.reset()
    controller.addPlayer(new HumanPlayer(2, "Player 2"))
    controller.addPlayer(new HumanPlayer(1, "Player 1"))

    Ok(views.html.main(Wui.printField(controller.grid)))
  }

  def printFiled = Action.async {
    val future = wuiActor.ask(OutputField(controller.grid)).mapTo[String]
    val timeoutFuture = play.api.libs.concurrent.Promise.timeout(0, 1.second)
    Future.firstCompletedOf(Seq(future, timeoutFuture)).map {
      case i: String => Ok(views.html.main(i))
      case i: Int => Ok(views.html.main("ERROR"))
    }
  }

  def command = Action { implicit request =>
    val line =  request.getQueryString("line")
    val result = controller.processInputLine(line.get)

    Ok(views.html.main(result))
  }
}
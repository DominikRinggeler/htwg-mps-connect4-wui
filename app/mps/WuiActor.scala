package mps

import akka.actor.Actor
import play.api.mvc.Results
import utils.{OutputField, Wui}

/**
 * Created by dominikringgeler on 25.10.15.
 */
class WuiActor extends Actor with Results {

  def receive = {
    case OutputField(grid) => {
      val results = Wui.printField(grid)
      sender.forward(results)
    }
  }
}

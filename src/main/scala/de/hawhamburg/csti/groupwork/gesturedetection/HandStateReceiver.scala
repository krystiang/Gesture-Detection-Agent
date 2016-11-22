package de.hawhamburg.csti.groupwork.gesturedetection

import de.hawhamburg.csti.framework.{ InternalAgent, AgentInfo }
import akka.io._
import akka.actor._
import java.net._
import de.hawhamburg.csti.config.Config
import de.hawhamburg.csti.groupwork.api._
class HandStateReceiver(ip: String, port: Int) extends Actor with ActorLogging {
  import context.system

  val agentInfo = AgentInfo("HandState-reader-agent")

  IO(Udp) ! Udp.Bind(self, new InetSocketAddress(ip, port))

  def receive = {
    case Udp.Bound(local) =>
      context.become(ready(sender()))
      log.info("UDP connected to {}:{}", ip, port)
  }

  def ready(socket: ActorRef): Receive = {
    case Udp.Received(data, remote) =>
      val processed = data.decodeString("utf-8") // parse data etc., e.g. using PipelineStage
      try {
        val Array(trackingId, lx, ly, lz, leftHandState,rx, ry, rz, rightHandState) = processed.replace(",", ".").split(";")
        context.parent ! HandState(trackingId.toLong, lx.toDouble, ly.toDouble, lz.toDouble, leftHandState.toString, rx.toDouble, ry.toDouble, rz.toDouble, rightHandState.toString)
      } catch {
        case e: Throwable => log.warning("could not parse hand state data {} - {} {}", processed, e.toString, e.getMessage)
      }
    case Udp.Unbind => socket ! Udp.Unbind
    case Udp.Unbound => context.stop(self)
  }
}
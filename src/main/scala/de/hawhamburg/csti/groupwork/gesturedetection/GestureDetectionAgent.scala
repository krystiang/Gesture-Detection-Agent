

package de.hawhamburg.csti.groupwork.gesturedetection

// Import dependencies for reading the config, use framework support for agents
// and the project specific API
import de.hawhamburg.csti.config.Config
import de.hawhamburg.csti.framework.{ Agent, AgentInfo }
import de.hawhamburg.csti.groupwork.api._
import de.hawhamburg.csti.groupwork.internal.api._
import de.hawhamburg.csti.groupwork.internal.api.internal._

import de.hawhamburg.csti.skeleton._
import scala.concurrent.duration._
import akka.actor.Props

// Agent main class, takes agent config as argument
class GestureDetectionAgent(val config: Config) extends Agent with GroupworkGestureDetectionAgentConfig {
  import SkeletonSerialization._
  import GestureSerialization._
  import context.dispatcher
  val agentInfo = AgentInfo("example-agent")

  val screenWidth = Settings.displayWidth;
  val screenHeight = Settings.displayHeight;
  
  // Map with skeleton IDs given by the Kinect and their individual GestureRecognizer
  var skeletonIds: Map[Int, GestureRecognizer] = Map()
  // Map with skeleton IDs given by the Kinect and the last time this skeleton was recognized by a fired frame
  var skeletonIdTimer: scala.collection.mutable.Map[Int, Long] = scala.collection.mutable.Map[Int, Long]()

  var handStateReceiver = context.actorOf(Props(classOf[HandStateReceiver], Settings.handStateIp, Settings.handStatePort))
  var handStates = Map[Long,Array[Array[Any]]]();
  var handPostures = scala.collection.mutable.Map[Int, Array[String]]();
  subscribe(Groups.Kinect)(SkeletonDeserializer)
  subscribe(Groups.Gesture)(GestureDeserializer)

  /**
   * checks periodicly if the last time a skeleton was recognized
   * was more then 5 seconds ago and deletes the skeleton plus his
   * GestureRecognizer if thats the case
   */
  context.system.scheduler.schedule(1000.millisecond, 30.millisecond) {
    val time = System.currentTimeMillis()
    skeletonIdTimer.foreach { x =>
      if (time - x._2 > 2000) {
        Groups.Gesture ! Event("Unregister", x._1)
        skeletonIdTimer -= x._1;
        skeletonIds -= x._1;
		handPostures -= x._1;
      }
    }
	handStates.foreach { y =>
	val time = System.currentTimeMillis();
	if (time - (y._2(2)(0)).asInstanceOf[Long] > 4000){
	handStates -= y._1;
	}
  }
  }
  
  def snchronizeHandStatesAndSkeletons(skeleton:Skeleton){
    var leftHand = skeleton.joints("7");
    var rightHand = skeleton.joints("11");
    var leftHandX = leftHand.position.x;
    var leftHandY = leftHand.position.y;
    var leftHandZ = leftHand.position.z;
    var rightHandX = rightHand.position.x;
    var rightHandY = rightHand.position.y;
    var rightHandZ = rightHand.position.z;    
    var closest:Long = -1;
    var smallestDistance = Double.MaxValue;

	
    for( handState <- handStates){
      var distance = Math.sqrt(Math.pow(handState._2(0)(0).asInstanceOf[Double] - leftHandX,2) +
                Math.pow(handState._2(0)(1).asInstanceOf[Double] - leftHandY,2) +
                Math.pow(handState._2(0)(2).asInstanceOf[Double] - leftHandZ,2));
      if (distance < smallestDistance){
        smallestDistance = distance;
        closest = handState._1
      }}
    if(closest != -1){
    handPostures(skeleton.id) = Array(handStates(closest)(0)(3).toString,handStates(closest)(1)(3).toString);
    }
  }

  /**
   * Receives skeleton data fired by the kinect and checks if there is a
   * new skeleton. If thats the case it receives it's own GestureRecognizer.
   * If that specific GestureRecognizer recognizes a gesture it'll be published
   * to the gesture group.
   */
  def receive = {
    case x: Skeleton =>
      if (!skeletonIds.contains(x.id)) {
        skeletonIds = skeletonIds + (x.id -> new GestureRecognizer(screenWidth,screenHeight))
      } else {
        skeletonIdTimer(x.id) = System.currentTimeMillis()
      }
      if(!handPostures.contains(x.id)){
         handPostures += x.id -> Array("Unknown...","Unknown...");
      }
      snchronizeHandStatesAndSkeletons(x);
      val gesture: Option[Gesture] = skeletonIds.get(x.id).get.recognize(x, handPostures(x.id));
      gesture match {
        case Some(i: SelectionGesture) =>
            Groups.Gesture ! JSelectionGesture(i.skeletonId,i.x,i.y)
        case Some(i: SwipeGesture) =>
            Groups.Gesture ! JSwipeGesture(i.skeletonId,i.direction)
        case Some(i: DragGesture) =>
            Groups.Gesture ! JDragGesture(i.skeletonId,i.x,i.y,i.phase)
        case Some(i: MoveGesture) =>
            Groups.Gesture ! JMoveGesture(i.skeletonId,i.x,i.y)
        case Some(i: TransformGesture) =>
             Groups.Gesture ! JTransformGesture(i.skeletonId,i.scale,i.angle,i.rotation,i.phase)
        case Some(i: RegisterGesture) =>
             Groups.Gesture ! Event("Register", i.skeletonId)
        case None => //System.out.println("No Gesture found.")
      }
    case x: HandState =>
			handStates += x.id -> Array(Array(x.lx,x.ly,x.lz, x.leftHandState),Array(x.rx,x.ry,x.rz,x.rightHandState), Array(System.currentTimeMillis()));
//}
 }
}


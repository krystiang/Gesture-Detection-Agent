package de.hawhamburg.csti.groupwork.gesturedetection

import de.hawhamburg.csti.config.Config
import de.hawhamburg.csti.framework.{ Agent, AgentInfo }
import de.hawhamburg.csti.groupwork.api._
import de.hawhamburg.csti.skeleton._
import de.hawhamburg.csti.groupwork.internal.api._
import de.hawhamburg.csti.groupwork.internal.api.internal._
import de.hawhamburg.csti.groupwork.gesturerecognizer._

class GestureRecognizer(screenWidth:Int, screenHeight:Int) {

  val aidFunctions = new AidFunctions(screenWidth, screenHeight);
  val selectionGestureRecognizer = new SelectionGestureRecognizer(screenWidth,screenHeight,aidFunctions);
  val swipeGestureRecognizer = new SwipeGestureRecognizer();
  val moveGestureRecognizer = new MoveGestureRecognizer(screenWidth,screenHeight,aidFunctions);
  val dragGestureRecognizer = new DragGestureRecognizer(screenWidth,screenHeight,aidFunctions);
  val transformGestureRecognizer = new TransformGestureRecognizer();
  var reach: Double = 0.0;
  var reachEstimations = 0;
  
    import GestureSerialization._

  
  def recognize(x: Skeleton, handStates: Array[String]): Option[Gesture] = {
    if (reach == 0.0) {
      reach = aidFunctions.estimateReach(x);
      if(reach != 0.0){
        return Some(RegisterGesture(x.id))
      }
      None
    } else {

      var gesture: Option[Gesture] = None

      var moveGesture = moveGestureRecognizer.recognize(x, handStates, reach);
      moveGesture match {
        case Some(i: MoveGesture) => gesture = Some(i)
        case None                 => Unit
      }

     
      var dragGesture = dragGestureRecognizer.recognize(x, handStates, reach);
      dragGesture match {
        case Some(i: DragGesture) => gesture = Some(i)
        case None                 => Unit;
      }
      
      var transformGesture = transformGestureRecognizer.recognize(x, handStates);
      transformGesture match {
        case Some(i: TransformGesture) => gesture = Some(i)
        case None                      => Unit
      }
     
      var swipeGesture = swipeGestureRecognizer.recognize(x, handStates);
      swipeGesture match {
        case Some(i: SwipeGesture) => gesture = Some(i)
        case None                  => Unit
      }

      var selectionGesture = selectionGestureRecognizer.recognize(x, handStates, reach);
      selectionGesture match {
        case Some(i: SelectionGesture) => gesture = Some(i)
        case None                      => Unit
      }
      
      
      gesture
    }

  }
}
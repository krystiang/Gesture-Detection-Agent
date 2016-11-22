package de.hawhamburg.csti.groupwork.gesturerecognizer

import de.hawhamburg.csti.skeleton._
import de.hawhamburg.csti.groupwork.api._
import de.hawhamburg.csti.groupwork.internal.api._
import de.hawhamburg.csti.groupwork.internal.api.internal._
import scala.collection.mutable.Queue

class MoveGestureRecognizer(screenWidth: Int, screenHeight: Int, aidFunctions: AidFunctions) {

  var lastHandStates = Array("Unknown...", "Unknown...");

  def recognize(x: Skeleton, handStates: Array[String], reach: Double): Option[MoveGesture] = {

    val headX = x.joints("3").position.x
    val headY = x.joints("3").position.y
    val headZ = x.joints("3").position.z

    val spineZ = x.joints("1").position.z
    val leftHandX = x.joints("6").position.x
    val rightHandX = x.joints("10").position.x
    val leftHandY = x.joints("6").position.y
    val rightHandY = x.joints("10").position.y
    val leftHandZ = x.joints("6").position.z
    val rightHandZ = x.joints("10").position.z

    //move gesture with left hand on imaginary wall right infront of the user. real monitor size is scaled onto arm length
    if (handStates(0).equals("Open") && reach != 0.0 && spineZ - leftHandZ > 0.1 && spineZ - rightHandZ < 0.1) {
      var screenCoord = aidFunctions.leftHandPositionToScreenPixel(x, reach);
      Some(MoveGesture(x.id, screenCoord(0), screenCoord(1)))
    } //move gesture with right hand on imaginary wall right infront of the user. real monitor size is scaled onto arm length
    else if (handStates(1).equals("Open") && reach != 0.0 && spineZ - rightHandZ > 0.1 && spineZ - leftHandZ < 0.1) {
      var screenCoord = aidFunctions.rightHandPositionToScreenPixel(x, reach);
      Some(MoveGesture(x.id, screenCoord(0), screenCoord(1)))
    } else {
      None
    }
  }
}
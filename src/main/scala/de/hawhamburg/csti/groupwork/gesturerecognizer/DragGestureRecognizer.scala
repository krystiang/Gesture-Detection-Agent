package de.hawhamburg.csti.groupwork.gesturerecognizer

import de.hawhamburg.csti.skeleton._
import de.hawhamburg.csti.groupwork.api._
import de.hawhamburg.csti.groupwork.internal.api._
import de.hawhamburg.csti.groupwork.internal.api.internal._

class DragGestureRecognizer(screenWidth: Int, screenHeight: Int, aidFunctions: AidFunctions) {

  var handPosition = Vector(Int.MinValue, Int.MinValue)
  var lastHandStates = Array("Unknown...", "Unknown...");
  var phase = "Unknown";

  def recognize(x: Skeleton, handStates: Array[String], reach: Double): Option[DragGesture] = {

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

    if (reach != 0.0 &&
      (handStates(0).equals("Closed") || lastHandStates(0).equals("Closed")) &&
      spineZ - leftHandZ > 0.1 &&
      spineZ - rightHandZ < 0.1) {
      if ((lastHandStates(0).equals("Open") || lastHandStates(0).equals("Unknown")) &&
        handStates(0).equals("Closed")) {
        phase = "Grab";
      } else if (lastHandStates(0).equals("Closed") &&
        handStates(0).equals("Open")) {
        phase = "Release";
      } else {
        phase = "Drag";
      }
      var screenCoord = aidFunctions.leftHandPositionToScreenPixel(x, reach);
      handPosition = screenCoord;
    } else if (reach != 0.0 &&
      (handStates(1).equals("Closed") || lastHandStates(1).equals("Closed"))
      && spineZ - rightHandZ > 0.1 &&
      spineZ - leftHandZ < 0.1) {
      if ((lastHandStates(1).equals("Open") || lastHandStates(1).equals("Unknown")) &&
        handStates(1).equals("Closed")) {
        phase = "Grab";
      } else if (lastHandStates(1).equals("Closed") &&
        handStates(1).equals("Open")) {
        phase = "Release";
      } else {
        phase = "Drag";
      }
      var screenCoord = aidFunctions.rightHandPositionToScreenPixel(x, reach);
      handPosition = screenCoord
    }
    if (!handPosition.equals(Vector(Int.MinValue, Int.MinValue))) {

      var tempHandPosition = handPosition
      handPosition = Vector(Int.MinValue, Int.MinValue)
      lastHandStates = handStates;
      return Some(DragGesture(x.id, tempHandPosition(0), tempHandPosition(1), phase))
    } else
      lastHandStates = handStates;
    None
  }
}


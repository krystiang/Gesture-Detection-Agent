package de.hawhamburg.csti.groupwork.gesturerecognizer

import de.hawhamburg.csti.skeleton._
import de.hawhamburg.csti.groupwork.api._
import de.hawhamburg.csti.groupwork.internal.api._
import de.hawhamburg.csti.groupwork.internal.api.internal._

class SelectionGestureRecognizer(screenWidth: Int, screenHeight: Int, aidFunctions: AidFunctions) {

  var gestureSegmentSeq = Seq[Tuple2[Int, Int]]();

  var lastHandStates = Array("Unknown...", "Unknown...");

  def recognize(x: Skeleton, handStates: Array[String], reach: Double): Option[SelectionGesture] = {

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
      //selection gesture with left hand on imaginary wall right infront of the user. real monitor size is scaled onto arm length
    if (handStates(0).equals("Open") && reach != 0.0 && spineZ - leftHandZ > 0.1 && spineZ - rightHandZ < 0.1) {
      var screenCoord = aidFunctions.leftHandPositionToScreenPixel(x, reach);
      gestureSegmentSeq = gestureSegmentSeq :+ (screenCoord(0), screenCoord(1))
    } //selection gesture with right hand on imaginary wall right infront of the user. real monitor size is scaled onto arm length
    else if (handStates(1).equals("Open") && reach != 0.0 && spineZ - rightHandZ > 0.1 && spineZ - leftHandZ < 0.1) {
      var screenCoord = aidFunctions.rightHandPositionToScreenPixel(x, reach);
      gestureSegmentSeq = gestureSegmentSeq :+ (screenCoord(0), screenCoord(1))
    } else {
      None
    }
      //checks if the hand traveled less then 5 cm in the last 8 frames.
    if (gestureSegmentSeq.length > 8) {
      if (gestureSegmentSeq.forall(elem => Math.sqrt(
        Math.pow(elem._1 - gestureSegmentSeq(1)._1, 2) +
          Math.pow(elem._2 - gestureSegmentSeq(2)._2, 2)) < 5)) {
        def mean(s: Seq[Int]) = s.foldLeft(0)(_ + _) / s.size
        var tempGestureSegmentSeq = gestureSegmentSeq
        gestureSegmentSeq = Seq[Tuple2[Int, Int]]();
        Some(SelectionGesture(x.id, mean(tempGestureSegmentSeq.map(elem => elem._1)), mean(tempGestureSegmentSeq.map(elem => elem._2))))
      } else {
        gestureSegmentSeq = Seq[Tuple2[Int, Int]]();
        None
      }
    } else {
      None
    }
  }
}
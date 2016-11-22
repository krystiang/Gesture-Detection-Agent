package de.hawhamburg.csti.groupwork.gesturerecognizer

import de.hawhamburg.csti.skeleton._
import de.hawhamburg.csti.groupwork.api._
import de.hawhamburg.csti.groupwork.internal.api._
import de.hawhamburg.csti.groupwork.internal.api.internal._

class SwipeGestureRecognizer {
  var gestureSegmentSeq = Seq[Int]();
  var lastSkeleton: Option[Skeleton] = None
  var firstHandCoordinates = Vector(Double.MinValue, Double.MinValue, Double.MinValue);
  var lastHandStates = Array("Unknown...", "Unknown...");

  def recognize(x: Skeleton, handStates: Array[String]): Option[SwipeGesture] = {
    if (lastSkeleton == None) {
      lastSkeleton = Some(x)
    }
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

    val lastLeftHandX = lastSkeleton.get.joints("6").position.x
    val lastRightHandX = lastSkeleton.get.joints("10").position.x
    val lastLeftHandY = lastSkeleton.get.joints("6").position.y
    val lastRightHandY = lastSkeleton.get.joints("10").position.y
    val lastLeftHandZ = lastSkeleton.get.joints("6").position.z
    val lastRightHandZ = lastSkeleton.get.joints("10").position.z
    //if left hand is closed and infront of the body while right isnt
    if (handStates(0).equals("Closed") && spineZ - leftHandZ > 0.2 && spineZ - rightHandZ < 0.2) {
      if (gestureSegmentSeq.isEmpty) {
        firstHandCoordinates = Vector(leftHandX, leftHandY, leftHandZ)
      }
      // from the last frame to this one in which direction did the hand move the most
      val differenceInDirections = Seq((0, lastLeftHandX - leftHandX),
        (1, leftHandY - lastLeftHandY),
        (2, leftHandX - lastLeftHandX),
        (3, lastLeftHandY - leftHandY))
      def max(d1: Tuple2[Int, Double], d2: Tuple2[Int, Double]): Tuple2[Int, Double] = if (d1._2 > d2._2) d1 else d2
      val direction = differenceInDirections.reduceLeft(max)._1

      // if the swipe direction doesnt change keep track of the direction otherwise reset the swipe gesture
      if (gestureSegmentSeq.isEmpty || gestureSegmentSeq.last.equals(direction)) {
        gestureSegmentSeq = gestureSegmentSeq :+ direction
        None
      } else {
        gestureSegmentSeq = Seq[Int]();
      }

      //saves the skeleton information as last skeleton for the next skeleton to come so 
      //its possible to compare those two
      lastSkeleton = Option(x);

      //swipe gesture is being thrown if 5 contiguous frames the hand moves in the same direction
      //and travels at least 0.2 meters over that period
      if (gestureSegmentSeq.size > 5 &&
        Math.sqrt(
          Math.pow(firstHandCoordinates(0) - leftHandX, 2) +
            Math.pow(firstHandCoordinates(1) - leftHandY, 2)) > 0.1) {
        gestureSegmentSeq = Seq[Int]();
        Some(SwipeGesture(x.id, direction))
      } else {
        None
      }
      //if right hand is closed and infront of the body while left isnt

    } else if (handStates(1).equals("Closed") && spineZ - rightHandZ > 0.2 && spineZ - leftHandZ < 0.2) {
      if (gestureSegmentSeq.isEmpty) firstHandCoordinates = Vector(rightHandX, rightHandY, rightHandZ);
      // from the last frame to this one in which direction did the hand move the most
      val differenceInDirections = Seq((0, lastRightHandX - rightHandX),
        (1, rightHandY - lastRightHandY),
        (2, rightHandX - lastRightHandX),
        (3, lastRightHandY - rightHandY))
      def max(d1: Tuple2[Int, Double], d2: Tuple2[Int, Double]): Tuple2[Int, Double] = if (d1._2 > d2._2) d1 else d2
      val direction = differenceInDirections.reduceLeft(max)._1

      // if the swipe direction doesnt change keep track of the direction otherwise reset the swipe gesture
      if (gestureSegmentSeq.isEmpty || gestureSegmentSeq.last.equals(direction)) {
        gestureSegmentSeq = gestureSegmentSeq :+ direction
      } else {
        gestureSegmentSeq = Seq[Int]();
      }

      //saves the skeleton information as last skeleton for the next skeleton to come so 
      //its possible to compare those two
      lastSkeleton = Option(x);
      //swipe gesture is being thrown if 5 contiguous frames the hand moves in the same direction
      //and travels at least 0.2 meters over that period
      if (gestureSegmentSeq.size > 5 &&
        Math.sqrt(
          Math.pow(firstHandCoordinates(0) - rightHandX, 2) +
            Math.pow(firstHandCoordinates(1) - rightHandY, 2)) > 0.1) {
        gestureSegmentSeq = Seq[Int]();
        Some(SwipeGesture(x.id, direction))
      } else {
        None
      }
    } else {
      None
    }
  }
}
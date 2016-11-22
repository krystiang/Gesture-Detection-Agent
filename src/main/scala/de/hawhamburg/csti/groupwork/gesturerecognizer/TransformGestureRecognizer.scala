package de.hawhamburg.csti.groupwork.gesturerecognizer

import de.hawhamburg.csti.skeleton._
import de.hawhamburg.csti.groupwork.api._
import de.hawhamburg.csti.groupwork.internal.api._
import de.hawhamburg.csti.groupwork.internal.api.internal._

class TransformGestureRecognizer {

  var lastHandStates = Array("Unknown...", "Unknown...");
  var firstHandsDistance = Double.MinValue;
  var phase = "Unknown...";
  def recognize(x: Skeleton, handStates: Array[String]): Option[TransformGesture] = {

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

    var directionVectorBetweenHands = Vector(leftHandX - rightHandX, leftHandY - rightHandY)
    var angle = angleBetweenTwoVectors(directionVectorBetweenHands, Vector(1, 0)).toDegrees;

    //Save hand positions in specific arrays if they fulfill the criterias for it
    //If enough consequent fulfilling gestures are recognized throw event otherwise clear Arrays
    if (((handStates(0).equals("Closed") &&
      handStates(1).equals("Closed")) ||
      (lastHandStates(0).equals("Closed") &&
        lastHandStates(1).equals("Closed"))) &&
        spineZ - leftHandZ > 0.25 &&
        spineZ - rightHandZ > 0.25 &&
        Math.abs(leftHandZ - rightHandZ) < 0.2) {
      if ((lastHandStates(0).equals("Open") || lastHandStates(0).equals("Unknown...") ||
        lastHandStates(1).equals("Open") || lastHandStates(1).equals("Unknown...")) &&
        handStates(0).equals("Closed") && handStates(1).equals("Closed")) {
        phase = "Start";
      } else if (lastHandStates(0).equals("Closed") &&
        lastHandStates(1).equals("Closed") &&
        (handStates(0).equals("Open") ||
          handStates(0).equals("Unknown...") ||
          handStates(1).equals("Open") ||
          handStates(1).equals("Unknown..."))) {
        phase = "End";
      } else {
        phase = "Running";
      }
      var distanceBetweenHands = Math.sqrt(
        Math.pow(leftHandX - rightHandX, 2) +
          Math.pow(leftHandY - rightHandY, 2));
      if (phase == "Start") {
        firstHandsDistance = distanceBetweenHands;
        lastHandStates = handStates;
      } else if (phase == "End") {
        lastHandStates = handStates;
      }
      if (firstHandsDistance != Double.MinValue) {
        var change = distanceBetweenHands - firstHandsDistance;
        var scale = (change / firstHandsDistance);
        var rotation = 0;
        if (leftHandY > rightHandY) {
          rotation = -1;
        } else {
          rotation = 1;
        }
        if (leftHandX > rightHandX) {
          angle = 180.0 - angle;
        }
        lastHandStates = handStates;
        return Some(TransformGesture(x.id, scale, angle, rotation, phase))
      } else {
        None
      }
    } else {
      None
    }
  }
  def angleBetweenTwoVectors(vec1: Vector[Double], vec2: Vector[Double]): Double = {
    var angle = Math.acos(Math.abs(vec1(0) * vec2(0) + vec1(1) * vec2(1)) /
      (Math.sqrt(Math.pow(vec1(0), 2) + Math.pow(vec1(1), 2)) * (Math.sqrt(Math.pow(vec2(0), 2) + Math.pow(vec2(1), 2)))));
    angle
  }
}
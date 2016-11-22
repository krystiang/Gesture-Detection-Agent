package de.hawhamburg.csti.groupwork.gesturerecognizer

import de.hawhamburg.csti.skeleton._
import scala.collection.mutable.Queue

class AidFunctions(screenWidth: Int, screenHeight: Int) {

  var gestureSegmentSeq = Seq[Tuple3[Double, Double, Double]]();
  val lastTenScreenCoords = Queue[Vector[Int]]();
  var reachEstimations = 0;
  var reachSum = 0.0;
  var reach = 0.0

  def estimateReach(x: Skeleton) = {

    val headX = x.joints("3").position.x
    val headY = x.joints("3").position.y
    val headZ = x.joints("3").position.z

    val leftHandX = x.joints("6").position.x
    val rightHandX = x.joints("10").position.x
    val leftHandY = x.joints("6").position.y
    val rightHandY = x.joints("10").position.y
    val leftHandZ = x.joints("6").position.z
    val rightHandZ = x.joints("10").position.z
    
    val leftElbowX = x.joints("5").position.x
    val leftElbowY = x.joints("5").position.y
    val leftElbowZ = x.joints("5").position.z
    val rightElbowX = x.joints("9").position.x
    val rightElbowY = x.joints("9").position.y
    val rightElbowZ = x.joints("9").position.z


    val leftClavicleX = x.joints("4").position.x
    val rightClavicleX = x.joints("8").position.x
    val leftClavicleY = x.joints("4").position.y
    val rightClavicleY = x.joints("8").position.y
    val leftClavicleZ = x.joints("4").position.z
    val rightClavicleZ = x.joints("8").position.z
    /* T gesture to estimate arm length/the potential reach of the user and
     * to ignore all skeletons which dont want to interact with the display
     * or are recognized wrong by the kinect
     */
    if (Math.abs(leftHandX - leftElbowX) < 0.15 &&
        Math.abs(leftHandX - leftClavicleX) < 0.15 &&
        Math.abs(leftHandZ - leftElbowZ) < 0.40 &&
      Math.abs(leftHandZ - leftClavicleZ) < 0.40 &&
      Math.abs(rightHandX - rightElbowX) < 0.15 &&
      Math.abs(rightHandX - rightClavicleX) < 0.15 &&
      Math.abs(rightHandZ - rightElbowZ) < 0.40 &&
      Math.abs(rightHandZ - rightClavicleZ) < 0.40 &&
      Math.abs(leftHandY - rightHandY) < 0.15 &&
      (leftHandY > leftElbowY) &&
       (leftElbowY > leftClavicleY) &&
       (rightHandY > rightElbowY) &&
       (rightElbowY > rightClavicleY)){
      var theoreticalReach = Math.sqrt(
        Math.pow(leftHandX - leftClavicleX, 2) +
          Math.pow(leftHandY - leftClavicleY, 2) +
          Math.pow(leftHandZ - leftClavicleZ, 2));
      reachSum += theoreticalReach;
      reachEstimations += 1;
    }
    if (reachEstimations >= 60) {
      reach = reachSum / reachEstimations;
      reach = reach -0.2;
    }
    reach;
  }

  def leftHandPositionToScreenPixel(x: Skeleton, reach: Double): Vector[Int] = {

    val leftHandX = x.joints("6").position.x
    val rightHandX = x.joints("10").position.x
    val leftHandY = x.joints("6").position.y
    val rightHandY = x.joints("10").position.y
    val leftHandZ = x.joints("6").position.z
    val rightHandZ = x.joints("10").position.z

    val leftClavicleX = x.joints("4").position.x
    val rightClavicleX = x.joints("8").position.x
    val leftClavicleY = x.joints("4").position.y
    val rightClavicleY = x.joints("8").position.y
    val leftClavicleZ = x.joints("4").position.z
    val rightClavicleZ = x.joints("8").position.z

    var relativeHandX = leftHandX - leftClavicleX + 0.1;
    var relativeHandY = leftHandY - leftClavicleY;
    var center = Array(screenWidth / 2, screenHeight / 2);
    var handPositionX = relativeHandX / reach;
    if (handPositionX > 0.0) handPositionX = 0.0;
    if (handPositionX < -1.0) handPositionX = -1.0;
    handPositionX = Math.abs(handPositionX);
    var handPositionY = Math.abs(relativeHandY / reach);
    var screenX: Int = (screenWidth - (screenWidth * handPositionX)).toInt;
    var screenY: Int = if (relativeHandY < 0.0) (center(1) + ((screenHeight / 2) * handPositionY)).toInt else (center(1) - ((screenHeight / 2) * handPositionY)).toInt;
    
     lastTenScreenCoords.enqueue(Vector(screenX, screenY));
      if(lastTenScreenCoords.length > 10) lastTenScreenCoords.dequeue();
      var averageCoord = Vector(0,0);
      for (coord <- lastTenScreenCoords){
        averageCoord = averageCoord.updated(0, averageCoord(0) + coord(0));
        averageCoord = averageCoord.updated(1, averageCoord(1) + coord(1));
      }
        averageCoord = averageCoord.updated(0, averageCoord(0) /lastTenScreenCoords.length);
        averageCoord = averageCoord.updated(1, averageCoord(1) /lastTenScreenCoords.length);
        averageCoord;
  }
  def rightHandPositionToScreenPixel(x: Skeleton, reach: Double): Vector[Int] = {

    val leftHandX = x.joints("6").position.x
    val rightHandX = x.joints("10").position.x
    val leftHandY = x.joints("6").position.y
    val rightHandY = x.joints("10").position.y
    val leftHandZ = x.joints("6").position.z
    val rightHandZ = x.joints("10").position.z

    val leftClavicleX = x.joints("4").position.x
    val rightClavicleX = x.joints("8").position.x
    val leftClavicleY = x.joints("4").position.y
    val rightClavicleY = x.joints("8").position.y
    val leftClavicleZ = x.joints("4").position.z
    val rightClavicleZ = x.joints("8").position.z

    var relativeHandX = rightHandX - rightClavicleX -0.1;
    var relativeHandY = rightHandY - rightClavicleY;
    var center = Array(screenWidth / 2, screenHeight / 2);
    var handPositionX = relativeHandX / reach;
    if (handPositionX > 1.0) handPositionX = 1.0;
    if (handPositionX < 0.0) handPositionX = 0.0;
    handPositionX = Math.abs(handPositionX);
    var handPositionY = Math.abs(relativeHandY / reach);
    var screenX: Int = (screenWidth * handPositionX).toInt;
    var screenY: Int = if (relativeHandY < 0.0) (center(1) + ((screenHeight / 2) * handPositionY)).toInt else (center(1) - ((screenHeight / 2) * handPositionY)).toInt;
        
    lastTenScreenCoords.enqueue(Vector(screenX, screenY));
      if(lastTenScreenCoords.length > 20) lastTenScreenCoords.dequeue();
      var averageCoord = Vector(0,0);
      for (coord <- lastTenScreenCoords){
        averageCoord = averageCoord.updated(0, averageCoord(0) + coord(0));
        averageCoord = averageCoord.updated(1, averageCoord(1) + coord(1));
      }
        averageCoord = averageCoord.updated(0, averageCoord(0) /lastTenScreenCoords.length);
        averageCoord = averageCoord.updated(1, averageCoord(1) /lastTenScreenCoords.length);
        averageCoord;
  }
}
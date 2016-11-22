// Project internal package name
package de.hawhamburg.csti.groupwork.internal.api

package internal {
  sealed trait Gesture
}
import internal._

// x and y coordinates of selected area
case class SelectionGesture(skeletonId:Int,x:Int,y:Int) extends Gesture
// flag to swipe direction left (0), up (1), right (2), down (3)
case class SwipeGesture(skeletonId:Int,direction:Int) extends Gesture
// x and y coordinates of drag plus notifier for the phase of the drag move (grab, drag, release)
case class DragGesture(skeletonId:Int,x:Int,y:Int,phase:String) extends Gesture
// x and y coordinates representing the whereabouts of the interacting hand
case class MoveGesture(skeletonId:Int,x:Int,y:Int) extends Gesture
// change in scale and rotation plus rotation direction indicator -1 = counterlockwise and 1 = clockwise
case class TransformGesture(skeletonId:Int,scale:Double,angle:Double,rotation:Int,phase:String) extends Gesture

case class RegisterGesture(skeletonId:Int)extends Gesture

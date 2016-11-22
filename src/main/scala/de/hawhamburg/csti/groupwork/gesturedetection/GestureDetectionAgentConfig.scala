// Project internal package name
package de.hawhamburg.csti.groupwork.gesturedetection

// Standard imports for group communication and agent configuration
import de.hawhamburg.csti.framework.Group
import de.hawhamburg.csti.framework.AgentConfig

// Define names of groups for group communication
// The concrete values of the groups names are stored in
// src/main/resources/application.conf
// So "Request" is a group object witch is called by a name stored in a value named "groups.request"
trait GroupworkGestureDetectionAgentConfig extends AgentConfig {
  object Groups {
    val Kinect = config.get[Group]("groups.kinect")
    val Gesture = config.get[Group]("groups.gesture")
  }
  
  // not used anymore. still here in case i'll need the syntax.
    object Settings {
    val handStateIp = config.get[String]("settings.handStateIp")
    val handStatePort = config.get[Int]("settings.handStatePort")
    val displayHeight = config.get[Int]("settings.displayHeight")
    val displayWidth = config.get[Int]("settings.displayWidth")
  }
  
}

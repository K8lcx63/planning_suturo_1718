# Planning-Interaction

## Purpose 

General purpose of this package is it to establish certain functions, which enable the Pr2 Robot
to interact with humans in case of some unsolvable Problems, like unreachable objects, unretrievable objects or unplaceable objects. Also the robot can use this interactions to solve some deadlocks, like the storageplaces beeing to filled up.

## Usage of interfaces

### first to start

#### init-interaction()
Initializes this package, and gets all needed publishers and subscribers ready.
Also this starts up the handshake detection in the background. 

Loops till all needed 3rd-party packages are running.

### Core features

#### ask-human-to-move-object()

!! CORE-FUNCTION !! 

+++ UNDER TESTING +++

Points at certain object which robot is not able to grasp for some reason.
Opens up gripper and asks human to put the named object in his gripper. 
Closes his gripper 5 seconds after handshake with human, and is able to continue normally.

### Safely usable Methods

#### say (string)
Uses the sound_play package to let the Robot say the given string. 
A phonetical text-to-speech interpreter will convert the given String into spoken word.

#### get-pointing-pose (pose)
First Transforms given Point into base-link frame and then gives x-value 
of given Pose a static x value to ensure that the PR2 is able to Point in this direction.

#### drive-to-human ()
Drives to a position near the wooden table at the right side of the IAI-Kitchen

#### calculate-wrench-magnitude (msg)
Calculates the magnitude of the given geometry_msgs/WrenchStamped message, containing 
the force-vector of the force/torque sensor at the wrist of the Pr2-Robot.
This function publishes a float32 '15' on the topic '/planning\_interaction/handshake\_detection'
and fills the \*handshake-detection\*-fluent with nil - when a handshake motion is detected.
*will probably get renamed later on*



#### decide-gripper(moving-command)
Because Motion decided to uses terrible magic numbers in theyre services, thinks are like this.
All actions for moving the arms do have magic numbers for which gripper to use. All actions for the right arm are noted with an even number, while all actions for the left arm are noted with an uneven number.

On the contrary, the magic numbers for the gripper-opening and gripper-closing are vise-versa. So this need to be calculated for some reason.

### Special dependencies

1. Planning-Move
2. Planning-Motion
3. Cram
4. sound_play
5. Planning-Logic
6. Robot\_wrist\_ft\_tools

### To be done

1. Generalized function for talking to humans unsing sound_play-package ```done```
2. Method to be called, when Object is unreachable ```in development```
3. Method to be called, when Object is unplaceable
4. Method to be called, when Object fell down
5. Method to be called, when unknown deadlock happened
6. Function to drive to a save position ```done```
7. Function calculate point in direction ```done```
8. Function that identifies handshake as a gesture ```done```

### Contact

Regarding any questions please contact
**stoermer@uni-bremen.de**

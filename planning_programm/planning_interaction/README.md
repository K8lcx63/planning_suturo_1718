# Planning-Interaction

## Purpose 

General purpose of this package is it to establish certain functions, which enable the Pr2 Robot
to interact with humans in case of some unsolvable Problems, like unreachable objects, unretrievable objects or unplaceable objects. Also the robot can run can use this interactions to solve some deadlocks, like the storageplaces beeing to filled up.

## Usage of interfaces

#### say (string)
Uses the sound_play package to let the Robot say the given string. 
A phonetical text-to-speech interpreter will convert the given String into spoken word.

#### get-pointing-pose (pose)
First Transforms given Point into base-link frame and then gives x-value 
of given Pose a static x value to ensure that the PR2 is able to Point in this direction.

### Special dependencies

1. Planning-Move
2. Planning-Motion
3. Cram
4. sound_play
5. Planning-Logic

### To be done

1. Generalized function for talking to humans unsing sound_play-package ```done```
2. Method to be called, when Object is unreachable ```in development```
3. Method to be called, when Object is unplaceable
4. Method to be called, when Object fell down
5. Method to be called, when unknown deadlock happened
6. Function to drive to a save position
7. Function calculate point in direction ```done```

### Contact

Regarding any questions please contact
**stoermer@uni-bremen.de**

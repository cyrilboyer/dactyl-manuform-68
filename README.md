# The Dactyl-ManuForm Keyboard - Topre version
This is a fork of the [Dactyl](https://github.com/adereth/dactyl-keyboard), a parameterized, split-hand, concave, columnar, ergonomic keyboard.

I wanted to have a Dactyl with Topre switches, I built a couple of working prototypes before that, one was in the form of a PCB with an arduino controller.
I then created a small breakout board / controller based off the awesome nice!nano instead so that I could hopefully handwire a Dactyl.

![Prototype](/resources/prototype-dactyl.png)

Changes:
* Modified switch holes to accept single unit custom Topre PCB as well as hold Realforces switch mechanisms (bought a couple off ebay, they are not that uncommon)
* Modified the layout to be close to a Kinesis advantage with a more ergonomic thumb island (after 3d printing the case, it just didn't work, needs to be redesigned, too large for me)
* Added bottom plate


* [Single Unit Topre PCB holder](cyrilboyer/dactyl-manuform-68/tree/master/things/test-single-plate.stl)

For reference see: [Dactyl-manuform](https://github.com/abstracthat/dactyl-manuform)

## License

Copyright © 2015-2017 Matthew Adereth and Tom Short

The source code for generating the models (everything excluding the [things/](things/) and [resources/](resources/) directories is distributed under the [GNU AFFERO GENERAL PUBLIC LICENSE Version 3](LICENSE).  The generated models and PCB designs are distributed under the [Creative Commons Attribution-NonCommercial-ShareAlike License Version 3.0](LICENSE-models).

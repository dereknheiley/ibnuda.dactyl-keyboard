# Forked version of Generator for Dactyl & Dactyl Manuform Keyboards

This is a fork of [the ibnuba fork](https://github.com/ibnuda/dactyl-keyboard) of the [tshort fork](https://github.com/tshort/dactyl-keyboard) of the [Dactyl](https://github.com/adereth/dactyl-keyboard), a parameterized, split-hand, concave, columnar, ergonomic keyboard.

If you want to read the old ibnuba `README.md`, go [here](README.ibnuba.md).

If you want to read the old old tshort `README.md`, go [here](README.keyboard.md).

## Goals
1. Improve strength of kailh hotswap holder
2. Add Support for glueing LED strips to provide per-key LED RGB backlighting
3. Tweak row, and column curvatures for smaller hands.

![Column Curvature](/resources/hobbit_column_curve.jpg)

![Glamour Shot](/resources/hobbit_glamour_shot.jpg)

![Hand Position](/resources/hobbit_hand_position.jpg)

![Pinky Reach](/resources/hobbit_pinky_reach.jpg)

![Clear Bottom](/resources/hobbit_clear_bottom.jpg)

# Generate OpenSCAD and STL models

## OLD
* Run `lein repl`
* In the repl run `(load-file "src/dactyl_keyboard/common.clj") (load-file "src/dactyl_keyboard/manuform.clj")`
* This will regenerate the `things/*.scad` files
* Use OpenSCAD to open a `.scad` file.
* Make changes to design, repeat `load-file`, OpenSCAD will watch for changes and rerender.
* When done, use OpenSCAD to render, then export model STL files which can be printed by 3d printer slicing software.

## NEW (faster)
* Run `lein auto generate`
* This will regenerate the `things/*.scad` files whenever the .clr file is saved
* Use OpenSCAD to open a `.scad` file.
* Make changes to design in `src/dactyl_keyboard/dactyl.clj`, open scad files will auto regenerate, OpenSCAD will rerender.
* When done, use OpenSCAD to render, then export model STL files which can be printed by 3d printer slicing software.

## Batch (parallel) Processing
* Edit the path for OpenSCAD in `create-models.sh` if needed
* Change any other settings for which files you want to render
* Wait for STL files to appear (this may take a minute or two) 

## License
Copyright © 2015-2020 Matthew Adereth, Tom Short, Ibnu D. Aji, Derek Nheiley et. al.

The source code for generating the models (everything excluding the [things/](things/) and [resources/](resources/) directories is distributed under the [GNU AFFERO GENERAL PUBLIC LICENSE Version 3](LICENSE).  The generated models and PCB designs are distributed under the [Creative Commons Attribution-NonCommercial-ShareAlike License Version 3.0](LICENSE-models).

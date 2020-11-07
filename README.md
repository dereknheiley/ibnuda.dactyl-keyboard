# Fokred version of Generator for Dactyl & Dactyl Manuform Keyboards

This is a fork of [the ibnuba fork](https://github.com/ibnuda/dactyl-keyboard) which is a fork of [the tshort fork](https://github.com/tshort/dactyl-keyboard) which is a form of the [Dactyl](https://github.com/adereth/dactyl-keyboard), a parameterized, split-hand, concave, columnar, ergonomic keyboard.

If you want to read the old ibnuba `README.md`, go [here](README.ibnuba.md).
If you want to read the old old tshort `README.md`, go [here](README.keyboard.md).

## Development

![Glamour Shot](/resources/hobbit_column_curve.jpg)

To tinker around follow these "old" steps:

1. Install JDK and [leiningen](https://leiningen.org/#install).
2. Change directory to this repository.
3. Edit and save something in `src/dactyl_keyboard/common.clj` etc..
4. Run `lein repl`.
5. Run `(load-file "src/dactyl_keyboard/common.clj") (load-file "src/dactyl_keyboard/manuform.clj")`
6. Open / wait for SCAD files to update.
7. Render in SCAD
8. Export to STL
9. Slice STL file using 3d printer software.
10. print and assemble your keyboard!

Here is a preview of the keyboard in a 3d printer slicing software (IdeaMaker).
![3D Printing](/resources/3dprintingsupports.png)

## Updated Wiring Diagram
![Fancy Wire Diagram](/resources/fancy-wiring-diagram.png)

## Timelapse Video
[![Dactyl Timelapse Video](/resources/timelapse-screenshot.png)](https://youtu.be/jucJIm_TujM)

## License
Copyright © 2015-2020 Matthew Adereth, Tom Short, Ibnu D. Aji, Derek Nheiley et. al.

The source code for generating the models (everything excluding the [things/](things/) and [resources/](resources/) directories is distributed under the [GNU AFFERO GENERAL PUBLIC LICENSE Version 3](LICENSE).  The generated models and PCB designs are distributed under the [Creative Commons Attribution-NonCommercial-ShareAlike License Version 3.0](LICENSE-models).

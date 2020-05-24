# Web Generator for Dactyl Keyboard

If you want to read the old `README.md`, go [here](README.keyboard.md).

## Development

To tinker around this thing, follow these steps:

1. Install JDK and [leiningen](https://leiningen.org/#install).
2. Change directory to this repository.
3. Run `lein ring server-headless`.
4. Change something in `src/dactyl_keyboard/handler.clj`.
5. Open [localhost:3030](http://localhost:3030).

## Deployment

To deploy it in a computer, follow these steps:

1. Install JDK and [leiningen](https://leiningen.org/#install).
2. Change directory to this repository.
3. Run `lein ring uberjar`.
4. Copy `target/dactyl-keyboard-version-SNAPSHOT-standalone.jar` to your server.
5. In the webserver, run `java -jar dactyl-keyboard-version-SNAPSHOT-standalone.jar`.

## Old Workflow

To use old workflow where `change code -> save -> openscad reloads model`,
uncomment `(spit "things/right.scad" (write-scad (model-right c)))` in
`src/dactyl_keyboard/dactyl.clj` or `src/dactyl_keyboard/lightcycle.clj`.

## Single Key PCB

If you want to use single key PCB, please use [single pcb](https://github.com/ibnuda/single).

## License

Copyright © 2015-2020 Matthew Adereth, Tom Short, and Ibnu D. Aji.

The source code for generating the models (everything excluding the [things/](things/) and [resources/](resources/) directories is distributed under the [GNU AFFERO GENERAL PUBLIC LICENSE Version 3](LICENSE).  The generated models and PCB designs are distributed under the [Creative Commons Attribution-NonCommercial-ShareAlike License Version 3.0](LICENSE-models).

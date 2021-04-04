# DevEnv

## How to use

To build and run a project, use

    stack run

from the project directory. This will download all the necessary dependencies, compile the project and start CodeWorld canvas server on address `http://localhost:3000`.

## Adding new code

Add new files to the `src` directory. Pay attention to the naming — start your files with upper case letter and name the modules with the same names as the files.

To use replace the main entrypoint, alter the `app/Main.hs` file to import the required module and run the required `IO ()` function.
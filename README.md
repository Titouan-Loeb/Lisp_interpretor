# HAL
## What is it?
Hal is a school project made in 3rd year at Epitech. The goal of this project is to make a Lisp interpretor in Haskell. Lisp is a functionnal programming language, it has several iterations but to see what this program can interpret go check the ToolKit.txt that describe every function and functionality that is handle by this interpretor.

## Compilation
This project compile using stack, make sure you have it installed on your machine to compile the program.
To compile the proram you can use the following command:
```bash
make
```
but you can also use the rules `make clean`, `make fclean` and `make re`

## Run
To run the program make sure you first compiled the program. Then run the program via bash command and give the Lisp file as argument like so:
```bash
./hal lispFiles/example.scm
```

You can give your own lisp file (make sure it respects the ToolKit.txt) but you can also try the example files in the `lispFiles` folder

# Fortran Numerical Methods

[![Language: English](https://img.shields.io/badge/Language-English-blue)](./README.md) [![Language: Español](https://img.shields.io/badge/Language-Espa%C3%B1ol-green)](./README.es.md)

This repository contains implementations of various numerical methods in the Fortran programming language. The methods include solving ordinary differential equations, numerical interpolation, and finding roots of equations, among others.

## Contents

-   `class-source`: Contains the source code files for classes to be implemented in future developments.
-   `data`: Contains data files used as input for the example programs.
-   `modules`: Contains Fortran modules for specific numerical methods.
-   `outputs`: Contains output files generated by the example programs.
-   `source`: Contains the main source code for the numerical methods from which the modules are created.
-   `trash`: Directory used to store temporary files and files in the process of being organized.

## Example Main Programs

-   `main_edo.f90`: Main program for solving ordinary differential equations.
-   `main_interpolacion.f90`: Main program for performing numerical interpolation.
-   `main_raices.f90`: Main program for finding roots of equations.

## How to Compile and Run

To compile and run the example main programs, follow these steps:

1.  Navigate to the repository root directory.
    
2.  Compile the main program of your choice using `gfortran`. For example, to compile `main_edo.f90`, run:
    
```bash
    gfortran modules/solucion_edo.f90 main_edo.f90 -o main_edo.out 
```
3.  Execute the compiled program. For example, to run `main_edo.out`, run:
```bash
    `./main_edo.out` 
```
## Contributions

Contributions are welcome. Please create a branch from your fork and submit a pull request with your changes.

## License

This project is licensed under the MIT License. See the LICENSE file for more details.

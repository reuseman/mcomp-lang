# µcomp-lang

![mucomp-lang](docs/mucomp-logo.jpg)

µcomp-lang is a simple component-based imperative language developed as part of a university project. It offers features such as:

* Programs are built out of components, which are linked together to form whole programs, promoting separation of construction and composition;

* A component is stateful and is a singleton, i.e., there is only an instance of each component, available at the beginning of the execution;

* The specification of component behavior is given through interfaces. Interfaces may be provided or used by the component. The provided interfaces represent the functionality that the component provides to its clients, while the used interfaces represent the functionality the component needs to perform its job;

* Interfaces specify a set of functions and global variables to be provided by the interface's provider;

* Components are statically linked to each other via their interfaces.
* To understand better all the remaining features and how to compile and run a µcomp-lang program, please refer to the [documentation](docs/report.pdf).

## Overview
```c
  component MyComponent provides App uses Algorithm {
    def print(a : int[], length : int) : void {
        var i : int = 0;
        put('[');
        for (; i < length; i++) {
            put(a[i]);
            if (i < length - 1) {
                put(',');
            }
        }
        print(']');
    }

    def main() : int {
        var my_arr : int[5];
        
        // Unsorted array
        my_arr[0] = 5;
        my_arr[1] = 4;
        my_arr[2] = 3;
        my_arr[3] = 2;
        my_arr[4] = 1;

        print(my_arr, 5);

        // Search for 2
        var index : int = search(my_arr, 10, 2);
        print(index);
        
        return 0;
    }
}

connect {
  MyComponent.Algorithm <- AlgorithmLib.Algorithm;
}

interface Algorithm {
  def search(a : int[], length: int, key: int) : int;
}

component AlgorithmLib provides Algorithm {
  def search(a : int[], length: int, key: int) : int {
    var i : int = 0;
    for (; i < length; i++) {
      if (a[i] == key) {
        return i;
      }
    }
    return -1;
  }
}

```

## Setup 
A development environment is provided with a detailed description in [here](docs/assignment/SETUP.md)

## Assignments
The project is split in the following assignments:

* **Parsing**: Implement a parser for µcomp-lang using the specifications provided [here](docs/assignment/README-PARSER.md); 

* **Semantic analysis**: Implement a static analysis for checking that a given program obeys the scoping rules and the type system of the language, using the description of semantic rules provided [here](docs/assignment/README-SEMANTIC.md);

* **Component linking and code generation**: Link components together according to what is specified by the programmer and use the LLVM toolchain to translate a µcomp-lang program to low-level code (LLVM bitcode). The description of linking rules can be found [here](docs/assignment/README-CODEGEN.md); 

* **Language extensions**: Extend the µcomp-lang language by implementing at least two of the following constructs: 
    * `do-while` loops;
    * pre/post increment/decrement operators, i.e., `++` and `--`;
    * abbreviation for assignment operators, i.e., `+=`, `-=`, `*=`, `/=` and `%=`;
    * variable declaration with initialization, e.g., `var i : int = 0`;
    * multi-dimensional arrays;
    * floating point arithmetic;
    * strings of characters;
    * inheritance among interfaces;
    * interfaces that can use other interfaces;
    * overloading of functions. 

As part of the project submission, students are required to submit their code, documentation, and a report describing the design and implementation choices.
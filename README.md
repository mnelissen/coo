# coo

Object Oriented C to plain C compiler

## Description

The language COO stands for 'C Object Oriented'. It adds object oriented
features to C but keeps the emphasis on lightweight and simple. So why not use
C++ then? Reasons to dislike C++:
* C++ causes programmers to write (over?)complicated template-heavy code
* C++ templates cause very slow compile times (just including the std lib!)
* template-heavy code tends to get bloated (although this also depends on how
  they are defined)
* need to define all methods in the class header file, no 'static' as in C
  * this in turn tends to cause (unnecessary) recompilations of source files
* exceptions easily turn into a way of use: just throw them, never handled

On the other hand, C++ also has many useful features. Some of those can be
emulated using macros but that tends to get ugly, therefore COO was started.

COO compiles to plain C so you can still use your favorite C compiler. This
makes it useful to use for embedded platforms where a C++ compiler/runtime is
not available.

## Features

* C compatible (any valid C program is a valid COO program)
* class methods
* virtual methods
* no runtime library required

Planned:
* inheritance (TODO)
* structured error handling (TODO)

## Design

A single pass find and replace text design. It does not parse the full
expression grammar of C, it does not build a syntax tree. It only remembers
struct types (calls them classes), and declared variables of these class types.
Tokens are always single characters. It finds the ';', '{', etc delimiters and
acts from there.

### Functionality

* recognize function declarations in struct definitions
  * declare as global functions with classname prepended with underscore
* recognize "virtual" function declarations in struct definitions
  * add a field called "vmt" pointing to the virtual method table
  * define an inline function wrapper that will call via the vmt
* recognize "class::function()" syntax to declare functions in classes
  * adds a "this" parameter automatically
  * recognize member variables, replace with this-\>variable
  * recognize member functions, replace with class function call with this
* recognize declarations of variables of struct (classes)
  * recognize member functions, replace with class function call with that var

## TODO

* generate the VMT
* initialize the "vmt" field to the VMT (add a constructor?)
* support inheritance
* recognize "#include" and parse header files
  * write processed header files to a different extension?

## License

This program is licensed under GPLv3 or later.

## Disclaimer

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

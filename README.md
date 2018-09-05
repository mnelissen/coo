# coo

Object Oriented C to plain C compiler

## Description

The language COO stands for 'C Object Oriented'. It adds object oriented
features to C but keeps the emphasis on lightweight and simple. So why not use
C++ then? Reasons to dislike C++:

* has many traps, e.g. around default-defined and explicit constructors
* constructors cannot fail gracefully, have to throw exception
* causes programmers to write (over?)complicated template-heavy code
* templates cause very slow compile times (just including the std lib!)
* template-heavy code tends to get bloated (although this also depends on how
  they are defined)
* need to define all methods in the class header file, no 'static' as in C
  * tends to cause more (unnecessary) recompilations of source files
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
* inheritance (add more tests)

Planned:

* structured error handling (in some form)
* protected classes, private/public members, encapsulation
* dynamic casting (auto *)
* method instance pointers
* special case for interfaces? (only virtual methods, no variables)
* typesafe containers
* function overloading? (in some form)

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
* recognize #include statements and recursively parse those header files
* recognize inheritance, add ':' after 'struct name' with names of classes
  to be inherited.
  * multiple inheritance is allowed, but not duplicated
  * recognize 'virtual parentname' to declare virtual inheritance (like C++)

## Include files

Files in #include lines are looked up same as C. With '<...>' searches provided
paths on the commandline only, '"..."' also searches the current directory of
the file that has the #include line.

Note that if included files are not found, no error is printed. Typically
there is no need for coo to parse for example system headers anyway. For
diagnostic purpose though, a commandline option '-xi' is available to filter
what files (the ones with given extension) should be processed. If in this case
an include file is not found, then an error _is_ printed.

## Constructors

A function defined in a struct that has the same name as the struct is the
constructor. Let it return void if it cannot fail. This will allow usage as
stack variable. If it can fail, then let it return a pointer to the struct
type (return this; or return NULL on fail).

Constructors must call all their parent constructors somewhere in their
function body. If a class has a VMT or virtual bases, then it also has a
"root constructor". The root constructor initializes all VMTs and virtual
bases and then calls the constructor. It is never called by other class
constructors, it is the origin of class creation (therefore the name
"root"). In simple cases the root constructor is generated automatically,
but if virtual base class constructors do not return void or take different
arguments, then the user must define the root constructor themselves.

Be careful calling virtual methods from a constructor as they will call
up to the most descendent class implementing them, immediately. Even though
that class' constructor might not have finished yet. On the other hand,
this may allow some different design patterns. For this reason, COO allows
you to call parent constructors anywhere in the constructor body, so you
can initialize necessary member variables first (expecting a virtual call).

When declaring class variables on the stack, their (root) constructor is
called by adding the arguments between parentheses after the variable name,
just like in C++. E.g. suppose class C with constructor C::C(int x) then
"C c(5);" declares a variable called "c" and calls its constructor with the
value 5.

## TODO

* fixup class pointer in multiple inheritance virtual call
* recognize global variables (searched, but never any added)
* add syntax "new C" to dynamically allocate class C on heap
* add syntax to zero-initialize a class automatically
* add destructors, plus call them for stack variables
* add virtual abstract = 0 syntax
* add call inherited to call parent's overridden function

## License

This program is licensed under GPLv3 or later. Processed output of this program,
in particular macro defines that are part of the program source code are licensed
under BSD-3-clause.

Copyright 2018 Micha Nelissen

## Disclaimer

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

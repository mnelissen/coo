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

* C compatible, but see below (any valid C program is a valid COO program)
* class methods
* virtual methods
* no runtime library required
* inheritance (add more tests)

Planned:

* structured error handling (in some form)
* protected classes, private/public members, encapsulation
* special case for interfaces? (only virtual methods, no variables)
* dynamic casting (auto *)
* method instance pointers
* typesafe containers
* function overloading? (in some form)

## C compatibility

C compatible but following notes.

### Stack variables with constructors

You cannot declare function prototypes in functions that return a struct on stack
and have a constructor. This means to construct a variable, not declare a function.
As existing C programs never have a constructor those continue to work, but adding
a constructor to that struct will change the meaning to construct a variable.
Example, notice how similar the func_proto_A and var_B lines look:

```
struct A {
  int x, y;
  /* plain struct, no constructor here */
};

struct B {
  int x, y;
  void B(int a, int b);
}

int some_function(int a, int b)
{
  /* declare a function prototype, this still works */
  A func_proto_A(int, int);
  /* B has a constructor so following declares a variable to construct */
  B var_B(a, b);
}
```

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
up to the most descendent class implementing them, immediately. (So, even
though that class' constructor might not have finished yet.) On the other
hand, this may allow some different design patterns than what C++ allows.
For this reason, COO allows you to call parent constructors anywhere in
the constructor body, so you can initialize necessary member variables
first (expecting a virtual call).

When declaring class variables on the stack, their (root) constructor is
called by adding the arguments between parentheses after the variable name,
just like in C++. E.g. suppose class C with constructor C::C(int x) then
"C c(5);" declares a variable called "c" and calls its constructor with the
value 5.

## Call inherited functions

In general member names and therefore function calls are unique. But when
overriding an inherited method, it often makes sense to call the inherited
function. Plain calling it would translate to a virtual method call, most
likely causing a loop. Therefore, similar syntax to C++ is supported to
call inherited functions with "class::function()" syntax. This syntax
causes a hard call to be emitted, never via the VMT. Also other member
functions may be called this way as an optimization if one is certain
that the method is not overridden anyway.

## TODO

* recognize global variables (searched, but never any added)
* add syntax "new C" to dynamically allocate class C on heap
* add syntax to zero-initialize a class automatically
* add destructors, plus call them for stack variables
* check whether all virtual base classes constructed in custom root constructor
* print line pragmas of output file if purely generated (e.g. vmt wrappers)

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

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
* inheritance
* no runtime library required

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

### Keywords

The keywords 'virtual', 'override', and 'new' are introduced.

### Stack variables with constructors

You cannot declare function prototypes in functions that return a struct on stack
and have a constructor. This means to construct a variable, not declare a function.
As existing C programs never have a constructor those continue to work, but adding
a constructor to that struct will change the meaning to construct a variable.
Example, notice how similar the func_proto_A and var_B lines look:

``` c
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

Class functions are translated to C functions with class name and function
name separated with an underscore.

Virtual functions in classes cause a field called 'vmt' to be declared. The
virtual function themselves generate a VMT struct (table) together with wrapper
functions to call the functions in the table.

## Include files

Files in #include lines are looked up same as C. With '<...>' searches provided
paths on the commandline only, '"..."' also searches the current directory of
the file that has the #include line.

Note that if included files are not found, no error is printed. Typically
there is no need for coo to parse for example system headers anyway. For
diagnostic purpose though, a commandline option '-xi' is available to filter
what files (the ones with given extension) should be processed. If in this case
an include file is not found, then an error _is_ printed.

## Classes

Classes are an extension of C struct types. They can have functions defined
in them, and these functions can be virtual. It is possible to declare
variables of class types on the stack and also to create them dynamically
using keyword 'new'. See also [Constructors](#Constructors).

Implementing functions in classes also uses same syntax as in C++. It is
allowed to define (implement) functions that were not declared for that
class. These are automatically made 'static' in the traditional C sense
of being scoped to this file. Inside the class function implementation
you can access member variables. E.g.:

``` cpp
struct A {
  int x;
  void somefunc(void);
};

void A::helperfunc(void)
{
  /* not declared in struct A, but is okay, now visible to this file */
  printf("helperfunc\n");
}

void A::somefunc(void)
{
  helperfunc();
  printf("%d", a);
}
```

### Inheritance

Classes can inherit other classes by listing them after a colon. All
member variable and function names have to be unique. It is not necessary
to specify 'public' (or private etc.) when inheriting a class. The
inheritance is always public.

#### Virtual functions

Function declarations can start with 'virtual' to make them virtual
methods. These methods can be dynamically overridden by descendant classes.
See next example in [Multiple inheritance](#multiple-inheritance).

Although declaring a virtual function uses same syntax as C++, overriding
a virtual function is different. Override a virtual function using just
the keyword 'override' and the function name you want to override. Do
not specify a parameter list, there is no function overloading anyway.

Also abstract functions are recognized with the '= 0' syntax like in C++.
This means the function is not implemented at all (different from C++),
and no prototype for it is emitted. It is also not allowed to create
classes that have abstract methods or do not implement all of their
parents' abstract methods (same as in C++). Variables of these class types
cannot be declared on the stack or created with operator new.

#### Multiple inheritance

Multiple inheritance is allowed, but classes that have member variables
can only be inherited once (directly or indirectly). (But see [Virtual inheritance](#Virtual-inheritance) below.) This rule is to prevent ambiguity when
referencing their (base) member variables in the class implementation.

Classes that define only (virtual) functions can be used as an interface.
This means they do not have any member variables defined, and therefore they
pose no ambiguity.

``` cpp
struct Intf1 {
  virtual void vfunc1(void);
};

struct Intf2 : Intf1 {
  virtual void vfunc2(void);
}

struct Base {
  int a1;
};

struct Left : Base, Intf1 {
  int b1;
  override vfunc1;
};

struct Right : Base {
  int c1;
};

/* this an error */
struct Bottom : Left, Right {
  int bottom;
};

/* this is okay, vfunc1 implemented by Left */
struct Left2 : Left, Intf2 {
  int i2;
  override vfunc2;
}
```

#### Virtual inheritance

Also virtual inheritance is supported, same as in C++. Put the keyword
'virtual' in front of the class name to inherit from, to inherit
virtually from it. This means that the implementation does not know the
exact offset to this base class (it is an extra pointer indirection).
The final class has exactly one occurance of this (virtual) base class
in its memory space.

Virtual inheritance allows the designer to inherit from a base class
multiple times even if that base class has member variables. COO allows
inheriting (literally, that is, non-virtually) from a base class once
plus any number of times virtually. In this case the virtual base
references are resolved to the one literal case. In C++ this would
lead to two copies of that base class in the final class: one for all
of the virtual references, and one for the literal.

### Constructors

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

### Destructors

For stack variables, destructors run automatically at end of function.
Return statements are translated into 'goto' statements that jump to a
common point at the end of the function.

When using goto, do not jump over declaring new variables. This will also
skip their constructor calls. But at exit of function, the destructor
will be called anyway likely leading to wrong behavior.

### Call inherited functions

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
* add syntax to zero-initialize a class automatically
* add "final" classes to not need override, and suppress warning?

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

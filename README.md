# Megastar

An interpreter for C* written in Haskell using Megaparsec.

This is the third iteration of this project, which provides a compiled 
interpreter to provide a faster runtime with some extra new language features.

Currently, there is no REPL for this interpreter *yet*. One will be added soon.

## Documentation

### The Tape

Unless specified, the program begins with a 'tape' of 1 byte that can have a
value ranging from 0-255 initialized to 0, starting at the first byte of 
the tape. If the default tape is not sufficent for a program, it can be reset 
in the following ways:

Initializing tape to a size of three all initialized to 0:

```
# 0 0 0
```

Initializing tape to ASCII values of "Hello, World!":

```
# 72 101 108 108 111 44 32 87 111 114 108 100 33
```

Initializing tape to ASCII values of "Hello, World!" with a newline via string 
notation:

``` 
#string "Hello, World!"
```

Initializing the tape to the contents of a file (Not finished):

```
#file <hello_world.txt>
```

### Modifying the Current Position

By default, the tape position is set to 0, which is the left-most or first 
value of the tape. To change the position of the tape, the following symbols 
can be used:

| Symbol | What it does |
| --- | --- |
| `->` | Increase the tape's position by 1 (move right 1). |
| `<-` | Decrease the tape's position by 1 (move left 1). |
| `>>` | Set the tape's position to the end of the tape (right-most value). |
| `<<` | Set the tape's position to the beginning of the tape (left-most value). |
| `*>` | Circularly increase by 1 (move to beginning if at end). |
| `<*` | Circularly decrease by 1 (move to end if at beginning). |

### Modifying the Current Cell's Value 

There are three basic operations that can modify the value of the selected cell 
on the tape:

#### [`+`] Increase Value

Using + followed by a number will increase the cell's value by that amount. 
If the result exceeds 255 or 0xff, an overflow will occur.

Increase cell value by 1:

```
+1
```

#### [`-`] Decrease Value

Using - followed by a number will decrease the cell's value by that amount. 
If the result exceeds 0 or an underflow will occur.

Decrease cell value by 1:

```
-1
```

#### [`~`] Override Value

Using `~` followed by a number will override the cell's current value to the 
specified amount. If the value is not in the range 0-255 then an overflow will
occurr.

Set the cell's value to 8:

``` 
~8
```

### IO Operations

Input and Output Operations

Input and Output in C* is done char-by-char unless the program specifies to 
read a number. Below are the IO operations:

| Symbol | What it does |
| --- | --- |
| `=>` | Read a char from either the user or the char buffer |
| `<=` | Convert the current cell's value into an ASCII character and display it to the screen. |
| `%>` | Read an integer from the user (does not read from the char buffer) |
| `<%` | Display the current cell's value as an integer. |
| `<>` | Shorthand for `<= ->`. |
| `<%>` | Shorthand for `<% ->`. |

### Control Flow 

#### Basic Loops 

It is sometimes difficult to write the same sequence of symbols over and over 
again, and greatly increases the risk of bugs and errors. To combat this, a
loop using `[]` can be used.

The syntax for a loop is as follows:

```
[iterations](... code ...)
```

If the code of the loop consists of only one operator, then parentheses are not 
needed. For example:

```
[3]+1
```

Because of this, loops can be chained together:

```
[5][4][2](+1 ->)
```

To make a loop run for the number of cells there are in the tape (i.e. there 
are k cells so the loop runs k times), the number of iterations can be left 
blank:

```
[]+1
```

Sometimes, it is helpful to run a loop (k - 1) times, where k is the length of
the tape. This can be used by specifying a negative number of iterations:

```
[-1](+1 ->)
```

#### Rolling Loops 

In order to apply an operation to every item on the tape using standard loops, 
the following pattern emerges:

```
[-1](OPERATION ->) OPERATION
```

Note that in the above pattern, `OPERATION` is written twice, because there is 
no way for a standard element to traverse every element without skipping one.
To solve this pattern, a rolling loop can be used.

Syntax:

```
{iterations}(... code ...)
```

`{i}(OPERATION)` is the same as `[k](OPERATION)`, where k = i - 1.

The positive, negative, and empty iteration syntax that applies to the standard 
loop applies to this as well. Parentheses are also optional for single operator 
statements. So to rewrite the pattern from above:

``` 
{}(OPERATION)
```

For other iterations, the same behavior still holds. Consider a tape of the 
values `[0, 0, 0, 0]`. To add one to the first three elements, with a final 
position on the third value, the following code can be used:

```
{3}+1
```

#### Conditional Operators

There are four additional operators that can be used to control the flow of 
code. They follow the same parentheses properties as the loops:

| Symbol | What it does |
| --- | --- |
| `?` | Runs as long as the current cell does not have a value of 0. |
| `!?` | Runs as long as the current cell has a value of 0. |
| `??` | Runs **once** if the current cell does not have a value of 0. |
| `!??` | Runs **once** of the current cell has a value of 0. |

The following code will decrement the current cell's value until it is equal to 
0:

```
?-1
```

The following code decrements the current cell's value and increments it's 
right neighbor until it is equal to 0:

```
?(-1 -> +1 <-)
```

The following code will set the cell's current value to 1 if it has a nonzero 
value:

```
??~1
```

#### Bookmarks

Cell's can be 'bookmarked' to travel to them easier. The syntax is as follows:

```
@name
^name
```

The first line will set the bookmark, and the second line will set the tape 
position to the bookmarked cell.

The following code will bookmark the current cell as `begin`, travel right until 
it reaches a zero value, bookmark that cell as `end`,
then jump back to `begin`:

```
@begin ?(->) @end ^begin
```

### Variables 

Values can be stored outside the tape for convenience and referenced in other 
calculations. Variables can be created with the following syntax:

```
x := 1
```

To use x as a value, the `$` operator can be used. For example, here's how to 
increment the current cell by `x`:

```
+$x
```

The values of variables can be mutated as well, either by re-assigning again 
or by incrementing/decrementing the value:

```
x += 1 
x -= 1
```

### Functions

If a piece of code is used over and over again but not in the same spot, or you need to nest parenthesis, then a function should be used.
Functions are defined with the syntax:

```
&func_name(... code ...)
```

To call a function, simply add an `*` before the function name, like so:

```
*func_name
```

Sometimes, It is helpful to pass parameters to functions to further reduce the 
amount of code that needs to be written. Functions can have any number of 
parameters, and functions with the sme name and different number of parameters 
can be written. For example, here are two functions named `add`, that perform 
different operations depending on the number of parametesr they are given:

```
&add<a, b>(+$a +$b)
&add<a>(+$a)
```

These functions can still be called with the `*` operator:

```
*add<1, 2>
*add<3>
```

Note that in the function definition, the parameters were access with the `$`
operator, as if they were variables. This is because that once a function is 
called, a new stack of variables is created with the function parameters 
reated as variables. This temporarily overrides any variables with the same 
name until the function has finished executing.

### Numerical Operations

To assist with the manipulation of numerical values, there are a fiew operators 
specifically designed for this purpose.

#### Bookmark Lookup

It is possible to load the value of a cell that has been bookmarked using the 
`^^` operator. For example, this can be used to clone the value of a cell to
the next cell over.

```
@location -> ~^^location
```

#### Char Representation 

It is also possible to convert a char to it's numerical representation by 
surrounding it in single quotes. For example, 

```
myChar := 'M'
```

#### Normalization

Numbers can be normalized by converting them to their boolean representation.
A 0 represents a false value, and a 1 represents a true value. The syntax is
simple:

```
|4|
```

Where `4` can be replaced with any numerical expression.

#### Folding Operators

There are a few folding operations that can be performed to fold a list of 
given numbers. These are the following:

| Symbol | What it does |
| --- | --- |
| `{+\|...}` | Add all elements together. |
| `{-\|...}` | Subtract all elements, left associative. |
| `{&\|...}` | Normalize all elements and perform a logical AND operation on them. |
| `{o\|...}` | Normalize all elements and perform a logical OR operation on them. |
| `{x\|...}` | Normalize all elements and perform a logical XOR operation on them. |

The `...` for each symbol represents a list of numerical expression separated 
by a comma. For example, to add three numbers together, the syntax would be as
follows:

```
result := {+|1,2,3}
```


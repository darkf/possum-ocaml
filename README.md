possum-ocaml v-1
copyright (c) 2013 darkf
licensed under the terms of the MIT license
see LICENSE for details

Possum is a scheme-inspired minimalistic language that is designed to be concise and simple.
It features fixed-arity functions, enabling us to lose the mess of parentheses normally associated with Lisps.

Here's a canonical factorial function (see `examples/` for more):

    defun factorial n is
      if = n 0
        1
      else
        * n factorial - n 1
    end
    
    print factorial 10

You can write this on one line, two lines, or however many you want (although we recommend you indent your code nicely!)

Unfortunately, possum is at a very early stage in development, so expect bugs and things downright not working! (In other words, don't trust your life with it.)

**Dependencies**:

Just an OCaml compiler


**Usage**:

- Compile with either `./build.sh` or `build.bat` depending on your platform. It will also run the resulting executable, so you can pass arguments to it as well, e.g. `./build.sh examples/map.psm`
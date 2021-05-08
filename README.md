Waxeye Parser Generator
=======================

Waxeye is a parser generator based on parsing expression grammars (PEGs).

Currently supported programming languages:
* Racket


Features
--------

* Language-agnostic, modular, composable grammars

* Automatic AST generation

* Command-line grammar interpreter

* Grammar testing DSL


User Manual
-----------

Waxeye's user manual is in `docs/manual.html`. The latest version is also
online at http://waxeye.org/manual.html.


Installation
------------

### Unix and OSX

1. Extract the files of the distribution.

2. Copy the `waxeye` directory to where you wish to install it.

3. Add the `bin/waxeye` binary to your search path. e.g. If you have `~/bin` in
   your `PATH` and installed waxeye to `/usr/local/waxeye` then you might do
   the following.

   `ln -s /usr/local/waxeye/bin/waxeye ~/bin/`


### Windows

1. Extract the files of the distribution.

2. Copy the `waxeye` directory to where you wish to install it.


Running
-------

### Unix and OSX

Use the `waxeye` command.

### Windows

Use a command prompt to run `waxeye.exe`. Note: If using the interpreter under
Windows, you will need to press `Ctrl-z` and then 'Enter' after the input you
want to interpret.


Building from Source
--------------------

1. Install [Racket](http://racket-lang.org)

2. Install Waxeye's backend for Racket.
   * Unix and OSX

     `sudo ln -s /usr/local/waxeye/src/racket/waxeye /usr/local/racket/lib/racket/collects/`

   * Windows

     Copy the directory `src/racket/waxeye` into your Racket `collects`
     directory. For example, `C:\Program Files\Racket\collects`.

3. Build Waxeye
   * Unix and OSX

     `./build/unix`

   * Windows

     - If your Racket installation isn't `C:\Program Files\Racket`, then you
       will need to modify `build\exe.bat` to use the correct path.

     - From your Waxeye installation directory, run the `build\exe.bat` script
       in a command prompt.


License
-------

[PolyForm Noncommercial License 1.0.0](https://polyformproject.org/licenses/noncommercial/1.0.0)

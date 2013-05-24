# AC6

This is a school project for the compilation class. The goal is to write an
interpreter for Clap, a toy language designed by [YRG][yrg].

[yrg]: http://www.pps.univ-paris-diderot.fr/~yrg/

Build with `make`, run with `./clap <your file>`.

## Additional Features

### Static Type-Checking

With `--type`, the interpreter statically check if the types in the program are
correct, using the Hindley-Milner algorithme.

### Memoization & Maximal Sharing

With `--memo`, the interpreter use memoization and maximal sharing (hash
consing). This ensure that a function application is only computed once (the
result is stored in the memory), and that a value is only stored once in the
memory.

### REPL

With `-i` or `--repl`, an interactive interpreter is launched. It works like any
other REPL. When you give it a definition, it evaluate it in the current
environment, and prints its value. It supports multi-lines code, and some
special commands:

- `/exit`, `/quit`, or `^D`: exit the REPL
- `/help`: print this list of commands
- `/clear`: clear the current environment

Note that you have to press `<enter>` on a newline to end your definition.

Additionally, the REPL can be used for debugging purposes. Run `./clap -i` with
some files, and it will execute each file one by one, then merge their
definitions and launch the REPL.

```
% echo "val x = 42" > a.clap
% ./clap -i a.clap
** Clap REPL v0.2.0
**
** Press ^D to exit.
**
clap> val xx = x * 2
   ?>
:- xx = 84
clap>^D
```

### Debug Mode

A debug mode can be activated using the `--debug` flag on the command-line. The
interpreter will then print debugging informations.

### Primitives

One additional primitive has been added, `print`, which prints its argument. No
newline is printed after it.

### Bash completion

Source the `utils/bash_completion.sh` file somewhere in your `~/.bashrc`, e.g.:

```sh
. /…/AC6/utils/bash_completion.sh
```

Or, simpler, copy it as `/etc/bash_completion.d/clap`. It supports options and
filenames completion for the `clap` executable.

### Vim syntax highlighting

Run `make vim-clap-syntax` and follow the instructions. It will add syntax
support for Clap in Vim.

## Tests

To run the tests, use `make check`. It’ll run the whole tests suit. When the
total number of successful tests is greater than the previous one (or if it’s
the first time you’re running the tests), fireworks in ASCII art are displayed,
and the new highscore is set.

A script, written in Ruby, can be used to generate a few tests from the email
answer of the correcting server. It can be found in `utils/get_server_tests.rb`.
Additionally, some dozens of tests has been written by hand, to match some cases
which are not not matched by the “official” tests suit.

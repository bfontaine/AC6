# AC6

This is a school project for the compilation class. The goal is to write an
interpreter for Clap, a toy language designed by [YRG][yrg].

## Build

```
make
```

## Run

```
./clap
```

## Bonuses

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

[yrg]: http://www.pps.univ-paris-diderot.fr/~yrg/

# Papyrus: A Cairo Symbolic Virtual Machine

<div>Papyrus is an open-sourced general framework for verification and synthesis of <img src="./docs/cairo-icon.png" width=24px>Cairo dApps. Papyrus performs reasoning using a builtin symbolic virtual machine based on Cairo bytecode.</div>

***Note: Papyrus is still under active development.***

## Dependencies (Building from Source)

- Cairo (0.8.2 Tested): [https://www.cairo-lang.org/](https://www.cairo-lang.org/)
- Racket (8.0+): [https://racket-lang.org/](https://racket-lang.org/)
  - Rosette (4.0+): [https://github.com/emina/rosette](https://github.com/emina/rosette)
    - `raco pkg install rosette`

## Getting Started: Interpretation

The repo comes with the simple example that you can also find on the official Cairo documentation:

```cairo
# examples/test0.cairo: interpretation
func main():
    [ap] = 1000; ap++
    [ap] = 2000; ap++
    [ap] = [ap - 2] + [ap - 1]; ap++
    ret
end
```

where the user assigns two values `1000` and `2000` to corresponding memory addresses and assigns the result of adding them up to another memory address. If you call Papyrus' builtin virtual machine by:

```bash
racket ./cairo-run.rkt --cname ./examples/test0_compiled.json
```

it can then execute the program and outputs the desired final memory states.

## Getting Started: Symbolic Reasoning (DEV)

***Note: This section is under development.***

Papyrus' virtual machine supports symbolic reasoning and you can utilize it to perform verification and synthesis on Cairo programs. For example:

```cairo
# examples/test1.cairo: symbolic reasoning
func main():
    [ap] = 1000; ap++
    [ap] = symbolic(type='integer', name='var0'); ap++
    [ap] = [ap - 2] / [ap - 1]; ap++
    ret
end
```

The above code snippet creates a symbolic integer named `var0` and assigns it to a certain memory address. We would like to find out whether this piece of code is correct or not by asking Papyrus to find a counterexample of valuation of `var0` to fail the execution of the program. If you call Papyrus' builtin virtual machine by:

```bash
racket ./cairo-run.rkt --cname ./examples/test1_compiled.json
```

It will reason about all possible values of `var0` and return one of them that can compromise the program execution, which is `0` that would cause a "division by 0" error. 

As in the line of code after we create the symbolic variable, `var0` is immediately used as denominator of a division arithmetic operation `[ap] = [ap - 2] / [ap - 1]; ap++`, which will cause an "unknown value for memory cell"  exception if `var0` is set to `0`. This verifies the counterexample found.

## Commands & Usages

### Using `cairo-run.rkt`

```bash
usage: cairo-run.rkt [ <option> ... ]

<option> is one of

  --cname <p-cname>
     path to a compiled Cairo program (.json)
  --help, -h
     Show this help
  --
     Do not treat any remaining argument as a switch (at this level)

 /|\ Brackets indicate mutually exclusive options.

 Multiple single-letter switches can be combined after
 one `-`. For example, `-h-` is the same as `-h --`.
```

### Example Commands

```bash
# normal call to papyrus
racket ./cairo-run.rkt --cname ./examples/test0_compiled.json

# turn on error trace
racket -l errortrace -t ./cairo-run.rkt

# original cairo compile command
cairo-compile ./examples/test1.cairo --output ./examples/test1_compiled.json

# original cairo-run command with trace
cairo-run --program=./examples/test1_compiled.json --print_output --print_info --relocate_prints --tracer
```


# Papyrus: A Symbolic Execution Tool for Cairo

<div>Papyrus is an open-sourced general framework for reasoning about <img src="./docs/cairo-icon.png" width=24px> Cairo programs based on symbolic execution.</div>

***Note: Papyrus is still under active development. For development notes, please see [here](./DEV.md).***

## Features

- [x] [Dev] **Program Exploration**: Papyrus can execute Cairo program with symbolic inputs and explore all its possible program states.
- [x] [Dev] **Property Verification**: Papyrus can check whether certain properties hold on Cairo program.
- [ ] [Dev] **Attack Synthesis**: Papyrus can automatically solve for concrete inputs that crash given Cairo program.
- [ ] [Dev] **Integrations with <img src="./docs/veridise-icon.png" width=24px> Veridise Product Lines**: Papyrus integrates with [[V] specication language](https://github.com/Veridise/V) that allows developers to express correctness properties.

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

where the user assigns two values `1000` and `2000` to corresponding memory addresses and assigns the result of adding them up to another memory address. By calling:

```bash
./run-papyrus.sh ./examples/test0.cairo
```

Papyrus can then execute the program and outputs the desired final memory states.

## Getting Started: Symbolic Reasoning (Dev)

Papyrus also supports reasoning and you can utilize it to verify several properties of Cairo programs. For example:

```cairo
# examples/test1.cairo: symbolic reasoning
func main():
    [ap] = 1000; ap++
    [ap] = symbolic(type='integer', name='var0'); ap++
    [ap] = [ap - 2] / [ap - 1]; ap++
    ret
end
```

The above code snippet creates a symbolic integer `var0` and assigns it to a certain memory address. We would like to find out whether this piece of code is correct or not by asking Papyrus to find a counterexample of `var0` that would potentially crash the execution of the program. By calling:

```bash
./run-papyrus.sh ./examples/test1.cairo
```

Papyrus will reason about all possible values of `var0` together with all program states, and return one of them that can compromise the program execution, which is `0` that would cause a "division by 0" error. 

As in the line of code after we create the symbolic variable, `var0` is immediately used as denominator of a division arithmetic operation `[ap] = [ap - 2] / [ap - 1]; ap++`, which will cause an "unknown value for memory cell"  exception if `var0` is set to `0`. This verifies the counterexample it found.

## Commands & Usages

### Using `run-papyrus.sh`

```bash
usage: run-papyrus.sh <path-to-cairo-program>
```

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
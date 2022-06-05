# Papyrus: A Cairo Symbolic Virtual Machine

This is a development README.

## Dependencies (Building from Source)

- Cairo (0.8.2 Tested): [https://www.cairo-lang.org/](https://www.cairo-lang.org/)
- Racket (8.0+): [https://racket-lang.org/](https://racket-lang.org/)
  - Rosette (4.0+): [https://github.com/emina/rosette](https://github.com/emina/rosette)
    - `raco pkg install rosette`

## Useful Commands

### Cairo Quick Commands

```bash
cairo-compile ./examples/test.cairo --output ./examples/test_compiled.json

cairo-run --program=./examples/test_compiled.json --print_output --print_info --relocate_prints --tracer
```


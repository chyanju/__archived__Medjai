#lang rosette
(require json)

(define json0 (string->jsexpr (file->string "./examples/test_compiled.json")))

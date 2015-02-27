# cl-snn
An implementation of Spiking Neural Networks on Common Lisp


## Requirements

1. [Lispbox](https://common-lisp.net/project/lispbox/)

1. Octave  (on system path)

## Using

Open the REPL and evaluate:

    (push #p"/path/to/repo/" asdf:*central-registry*)
    (ql:quickload "cl-snn")
    (in-package :snn)

Some examples are provided at *snn-scratchpad.lisp*. 
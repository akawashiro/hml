#! /bin/sh

stack build
stack exec hml-exe -- -d adder.ml
stack exec hml-exe -- -d arith.ml
stack exec hml-exe -- -d closure.ml
stack exec hml-exe -- -d fun.ml
stack exec hml-exe -- -d if.ml


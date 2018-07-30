#! /bin/sh

stack build
stack exec hml-exe -- -d adder.ml
echo "-------------------------------"
stack exec hml-exe -- -d arith.ml
echo "-------------------------------"
stack exec hml-exe -- -d closure.ml
echo "-------------------------------"
stack exec hml-exe -- -d fun.ml
echo "-------------------------------"
stack exec hml-exe -- -d if.ml
echo "-------------------------------"


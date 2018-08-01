#! /bin/sh

stack build || exit
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
stack exec hml-exe -- -d nestlet.ml
echo "-------------------------------"
stack exec hml-exe -- -d tuple.ml
echo "-------------------------------"

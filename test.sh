#! /bin/sh

stack build && cat test.ml | stack exec hml-exe

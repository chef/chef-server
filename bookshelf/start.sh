#!/bin/sh

erl -pz $PWD/deps/*/ebin -pz $PWD/ebin -boot start_sasl -s bookshelf_app $@

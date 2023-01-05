#!/bin/sh

./bin/campseudo-to-py $1
python3 $1.py

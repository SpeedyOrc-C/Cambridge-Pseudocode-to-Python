#!/bin/sh

if [ -d ./bin ]
then
    echo \"bin\" folder exists.
else
    echo \"bin\" folder does not exist, create it.
    mkdir ./bin
fi

cd src
ghc Main.hs -o ../bin/campseudo-to-py
cd ../

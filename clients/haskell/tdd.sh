#!/bin/bash

DIR=/tmp/carpaccio
mkdir $DIR

while true; do

    ghc src/Model.hs test/* -odir $DIR -hidir $DIR -o $DIR/carpaccio_test
    $DIR/carpaccio_test
    echo ""

    inotifywait -qr -e modify -e create -e move -e delete src test --exclude "\.\#.*"
done

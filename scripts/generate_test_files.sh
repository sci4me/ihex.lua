#!/bin/bash

N_FILES=5
MIN_BYTES=10
MAX_BYTES=500
FILE_DIR="$(pwd)/spec/test_files"

mkdir -p $FILE_DIR

for i in {1..5}
do
    n=$(($MIN_BYTES + $RANDOM % $MAX_BYTES))
    in="$FILE_DIR/test_$i.bin"
    out="$FILE_DIR/test_$i.hex"

    echo "test_$i: $n bytes"

    dd if=/dev/random of=$in count=$n bs=1 status=none
    bin2ihex -i $in -o $out # bin2ihex => https://github.com/arkku/ihex
done
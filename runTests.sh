#!/bin/bash

cd mini-parser
make
java Mini ../tests/1.mini > ../tests/1.json
java Mini ../tests/2.mini > ../tests/2.json
java Mini ../tests/ret.mini > ../tests/ret.json
java Mini ../tests/ret_bad.mini > ../tests/ret_bad.json
java Mini ../tests/ret_bad2.mini > ../tests/ret_bad2.json
cd ..

mlton mc.mlb
./mc tests/1.json
./mc tests/2.json
./mc tests/ret.json
./mc tests/ret_bad.json
./mc tests/ret_bad2.json


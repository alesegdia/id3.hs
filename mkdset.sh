#!/usr/bin/env bash

rm hashes.dat
rm *.o
rm *.hi
rm DataTrain.*
rm DataTest.*

php csv2hs.php dstrain.csv DataTrain
php csv2hs.php dstest.csv DataTest

ghc Main.hs

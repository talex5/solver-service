#!/bin/sh
for n in 1 2 4 8 16 32 64; do dune exec -- ./stress/stress.exe --count=64 --internal-workers=$n; done
for n in 32 64 127; do dune exec -- ./stress/stress.exe --count=256 --internal-workers=$n; done

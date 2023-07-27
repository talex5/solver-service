.PHONY: all test

all:
	dune build

test:
	dune exec -- ./stress/stress.exe --count=40 --internal-workers=7

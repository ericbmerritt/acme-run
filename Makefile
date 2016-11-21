PHONY: all build format

all: build format

build: 
	stack test
	
format:
	find ./src -name "*.hs" -exec sh -c " stack exec hindent -- --style gibiansky --line-length 80 {} " \;
	find ./abuild -name "*.hs" -exec sh -c " stack exec hindent -- --style gibiansky --line-length 80 {} " \;
	find ./test -name "*.hs" -exec sh -c " stack exec hindent -- --style gibiansky --line-length 80 {} " \;

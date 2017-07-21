Acme-Run
========

Status
------
![Circle Ci Build Status](https://circleci.com/gh/metadrift/acme-run.svg?style=shield&circle-token=:circle-token

Intro
-----

This package provides a set of commands that are useful for the [Acme 
Editor](http://acme.cat-v.org/). 

abuild
------

This command takes the file name of the current window, finds the root build 
directory and run the build. The current build files and related commands in 
priority order. 

1. `build.sbt`: sbt compile
2. `stack.yaml`: stack build
3. `makefile`: make
4. `Makefile`: make

At the moment this isn't configurable, but it would be fairly simple to make it 
configurable. 

aindent
-------

Indent the current source file according to a reasonable set of rules. At the 
moment it knows how to ident haskell files and scala files. This isn't 
configurable, but could be easily made configurable.

afold
-----

Wraps the selected text to 80 characters. Also could be configurable, but 
currently is not.

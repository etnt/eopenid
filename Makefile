include dep.inc

compile: 
	(export YAWS_DIR=$(YAWS_DIR); erl -make)

init:
	git submodule init
	git submodule update
	(cd dep/trane; make)

clean:
	rm -rf ./ebin/*.beam

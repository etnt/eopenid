include dep.inc

compile: 
	(export YAWS_DIR=$(YAWS_DIR); erl -make)
	
clean:
	rm -rf ./ebin/*.beam

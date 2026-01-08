all: happening

happening: src/*.lisp *.asd
	sbcl --eval "(asdf:make :happening)" --quit

clean:
	rm -rf *~ happening

web1: src/*.lisp *.asd
	/usr/bin/sbcl --eval "(asdf:make :happening)" --quit

clean:
	rm -rf *~ happening

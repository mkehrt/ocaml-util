all:
	-mkdir build bin >> /dev/null
	ocamlc str.cma src/markov.ml -o bin/markov

clean:
	rm -rf build bin

#   make         to rebuild the executable file f
#   make test    to rebuild the executable and run the unit tests
#   make sample  to rebuild the executable and run it on input file test.f
#                (this is "make test" in the ocaml implementation)
#   make clean   to remove all intermediate and temporary files

FILES = Typing.hs Syntax.hs Printing.hs Parser.hs Evaluator.hs Main.hs Tests.hs TaplError.hs Util.hs TestUtils.hs ArithTests.hs FullSimpleTests.hs SimpleContext.hs

f: $(FILES) 
	ghc $(INCLUDE) -fglasgow-exts --make Main.hs -o f -dynamic

test-runner: $(FILES) 
	ghc $(INCLUDE) -fglasgow-exts --make Tests.hs -o test-runner -dynamic

test: test-runner
	./test-runner

sample: f
	./f test.f

clean:
	-rm *.hi *.o f test-runner 


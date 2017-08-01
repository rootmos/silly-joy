.PHONY: repl
repl:
	stack build && rlwrap stack exec silly-joy-exe

.PHONY: test
test:
	stack test

.PHONY: repl
repl: build
	stack exec silly-joy-exe

.PHONY: tui
tui: build
	stack exec silly-joy-exe -- --tui

.PHONY: build
build:
	stack build

.PHONY: test
test:
	stack test

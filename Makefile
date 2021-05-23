.PHONY: build
build:
	idris2 --build lsp.ipkg

clean:
	idris2 --clean lsp.ipkg
	rm -r build

repl:
	rlwrap idris2 --repl lsp.ipkg

testbin:
	@${MAKE} -C tests testbin

# usage: `make test only=messages001`
test-only:
	${MAKE} -C tests only=$(only)

test: build testbin test-only


deploy:
	rm ~/.idris2/bin/idris2-lsp
	rm -r ~/.idris2/bin/idris2-lsp_app
	cp ~/Sources/idris2-lsp/build/exec/idris2-lsp ~/.idris2/bin/
	cp -r ~/Sources/idris2-lsp/build/exec/idris2-lsp_app ~/.idris2/bin/

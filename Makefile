sourceFiles != find . -name '*.hs' -type f

build: 
	cabal build

run:
	cabal run

repl:
	cabal repl

hindent:
	cabal clean
	hindent --indent-size 2 --line-length 100 --sort-imports $(sourceFiles)

edit:
	cabal clean
	vim -p $(sourceFiles)

hlint:
	hlint -v --no-exit-code --report --with-group=generalise-for-conciseness --with-group=extra --with-group=teaching .
	open report.html

clean:
	cabal clean
	rm -fv report.html

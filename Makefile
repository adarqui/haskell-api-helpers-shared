build:
	stack build --fast

clean:
	stack clean

make tests:
	stack test --fast

ghci:
	stack ghci haskell-api-helpers-shared

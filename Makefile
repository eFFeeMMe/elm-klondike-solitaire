make:
	elm make src/Main.elm --output dist/index.html

develop:
	elm-live src/Main.elm -- --debug

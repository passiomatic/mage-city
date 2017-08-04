all: build

levels:
	python elmify.py ./assets/Forest1.json .

build:
	elm-make src/Main.elm --output dist/elm.js

dist: levels build
	cp -R images dist/
	cp index.html dist/index.html

run:
	elm-reactor

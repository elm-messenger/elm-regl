release: format
	mkdir -p build
	elm make src/$(NAME).elm --output=build/main.js
	uglifyjs build/main.js -c -m --in-situ
	cp build/main.js ../docs/$(NAME).js

format:
	elm-format src/ --yes

clean:
	rm -rf build/

.PHONY: debug release format clean

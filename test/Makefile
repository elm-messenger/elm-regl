debug: format
	mkdir -p build
	elm make src/$(NAME).elm --output=build/main.js
	cp -f public/$(NAME).html ./index.html
	cp -f public/style.css build/style.css

release: format
	mkdir -p build
	elm make src/$(NAME).elm --output=build/main.js
	uglifyjs build/main.js -c -m --in-situ
	cp -f public/$(NAME).html ./index.html
	cp -f public/style.css build/style.css

host:
	python3 -m webbrowser http://localhost:8123
	python3 -m http.server 8123

format:
	elm-format src/ --yes

clean:
	rm -rf build/

.PHONY: debug release format clean

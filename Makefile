debug: format
	mkdir -p build
	elm make src/Main.elm --output=build/main.js
	cp -f public/index.html .
	cp -f public/style.css build/style.css
	# cp -f public/*.js build/

release: format
	mkdir -p build
	elm make src/Main.elm --output=build/main.js --optimize
	uglifyjs build/main.js -c -m --in-situ
	cp -f public/index.html .
	cp -f public/style.css build/style.css
	# cp -f public/*.js build/

host:
	python3 -m webbrowser http://localhost:8123
	python3 -m http.server 8123

format:
	elm-format src/ --yes

clean:
	rm -rf build/

.PHONY: debug release format clean

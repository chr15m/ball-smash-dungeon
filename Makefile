build: $(shell find src) public/*
	mkdir -p build
	npx shadow-cljs release app
	rsync -aLz --exclude js --exclude '.*.swp' public/ build
	touch build

node_modules: package.json
	npm i

.PHONY: watch clean

watch: node_modules
	npx shadow-cljs watch app 

repl: node_modules
	npx shadow-cljs cljs-repl app

clean:
	rm -rf build

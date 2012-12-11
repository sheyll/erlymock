REBAR="./rebar"
SHELL = /bin/sh

.DEFAULT_GOAL := compile

.PHONY = compile clean test doc

doc:
	$(REBAR) doc skip_deps=true
	git checkout gh-pages	
	mv doc/* .
	rm -rf doc
	git add . 
	git commit -m "Update auto-generated E-Doc"
	git push origin gh-pages
	git checkout master

compile:
	$(REBAR) get-deps
	$(REBAR) compile

test:
	$(REBAR) skip_deps=true eunit

clean:
	$(REBAR) clean
	rm -rf doc

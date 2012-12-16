REBAR="./rebar"
SHELL = /bin/sh

.DEFAULT_GOAL := edoc

.PHONY = compile clean test edoc

edoc:
	$(REBAR) doc skip_deps=true
	git checkout gh-pages	
	mv doc/*.html .
	mv doc/*.css .
	mv doc/*.png .
	git add . 
	git commit -m "Update auto-generated E-Doc"
	git push origin gh-pages
	git checkout master

compile:
	$(REBAR) get-deps
	$(REBAR) compile

eunit: compile clean
	$(REBAR) skip_deps=true eunit

clean:
	$(REBAR) clean
	rm -f doc/*.html
	rm -f doc/*.css
	rm -f doc/*.png

REBAR="./rebar3"
SHELL = /bin/sh

.DEFAULT_GOAL := compile

.PHONY = compile clean test edoc dialyzer

edoc:
	$(REBAR) edoc
	git checkout gh-pages
	mv doc/*.html .
	mv doc/*.css .
	mv doc/*.png .
	git add .
	git commit -m "Update auto-generated E-Doc"
	git push origin gh-pages
	git checkout master

compile:
	$(REBAR) compile

eunit: compile clean
	$(REBAR) eunit

dialyzer: eunit
	$(REBAR) dialyzer

clean:
	$(REBAR) clean
	rm -f doc/*.html
	rm -f doc/*.css
	rm -f doc/*.png

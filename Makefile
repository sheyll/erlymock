REBAR="./rebar3"
SHELL = /bin/sh

.DEFAULT_GOAL := compile

.PHONY = compile clean test gh-pages dialyzer

gh-pages:
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

test: clean compile dialyzer
	$(REBAR) eunit

dialyzer: compile
	$(REBAR) dialyzer

clean:
	$(REBAR) clean
	rm -f doc/*.html
	rm -f doc/*.css
	rm -f doc/*.png
	rm -f TEST-*.xml

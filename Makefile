REBAR="./rebar3"
SHELL = /bin/sh

.DEFAULT_GOAL := compile

.PHONY = compile clean test gh-pages dialyzer

publish: dialyzer
	$(REBAR) hex publish
	$(REBAR) hex docs

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

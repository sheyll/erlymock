REBAR="./rebar3"
SHELL = /bin/sh

.DEFAULT_GOAL := compile

.PHONY: publish doc test dialyzer compile clean

publish: clean compile dialyzer test doc
	$(REBAR) hex publish

doc: 
	$(REBAR) hex docs

test: compile
	$(REBAR) eunit

dialyzer: compile
	$(REBAR) dialyzer

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean
	rm -f doc/*.html
	rm -f doc/*.css
	rm -f doc/*.png
	rm -f TEST-*.xml

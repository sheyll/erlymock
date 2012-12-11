REBAR="./rebar"
SHELL = /bin/sh

.DEFAULT_GOAL := compile

.PHONY = compile clean test doc

doc:
	$(REBAR) doc skip_deps=true
#	mv doc/*.html .


compile:
	$(REBAR) get-deps
	$(REBAR) compile

test:
	$(REBAR) skip_deps=true eunit

clean:
	$(REBAR) clean
	rm -rf doc

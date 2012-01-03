SHELL=/bin/bash
ERL ?= erl
APP := eml

.PHONY: deps

all: deps
	@./rebar compile

deps:
	@./rebar get-deps

examples: all
	@erl -pa ./ebin -noshell -s eml compile_examples -s init stop
	@(cd examples; erlc *.erl)

clean:
	@./rebar clean
	@rm -f examples/*.erl examples/*.beam

distclean: clean
	@./rebar delete-deps

test: local_clean
	@./rebar eunit

ct: all
	@./rebar -C rebar.config.test ct

ct2: all
	@./rebar -C rebar.config.test ct

ctv: all
	@./rebar -C rebar.config.test ct verbose=1

local_clean:
	@rm -f ./ebin/* .eunit/*

xref: all
	@./rebar xref

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

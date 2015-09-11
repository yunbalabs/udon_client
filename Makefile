REBAR := ./rebar -j8

.PHONY: all deps clean release

all: compile

compile: deps
	$(REBAR) compile

deps:
	$(REBAR) get-deps

clean:
	$(REBAR) clean

relclean:
	rm -rf rel/udon_client

release: all test
	dialyzer --src src/*.erl deps/*/src/*.erl

generate: compile
	cd rel && ../$(REBAR) generate

run: generate
	./rel/udon_client/bin/udon_client start

console: generate
	./rel/udon_client/bin/udon_client console

foreground: generate
	./rel/udon_client/bin/udon_client foreground

erl: compile
	erl -pa ebin/ -pa deps/*/ebin/ -s udon_client

REBAR=./rebar

.PHONY: deps get-deps

all:
	@$(REBAR) get-deps
	@$(REBAR) compile

tentacles_server:
	@$(REBAR) compile skip_deps=true

clean:
	@$(REBAR) clean

get-deps:
	@$(REBAR) get-deps

deps:
	@$(REBAR) compile

tests:
	@$(REBAR) skip_deps=true eunit

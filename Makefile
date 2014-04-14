REBAR=./rebar

all: compile

get-deps:
	@$(REBAR) get-deps

compile: get-deps
	@$(REBAR) compile

clean:
	@$(REBAR) clean

tests:
	@$(REBAR) --config "rebar.test.config" get-deps
	@$(REBAR) --config "rebar.test.config" compile
	@$(REBAR) --config "rebar.test.config" skip_deps=true eunit

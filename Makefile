.PHONY: compile rel test typecheck

REBAR=./rebar3

compile:
	$(REBAR) as prod compile

clean:
	$(REBAR) clean

test:
	$(REBAR) as test do eunit

typecheck:
	$(REBAR) dialyzer


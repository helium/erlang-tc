.PHONY: compile rel test typecheck

REBAR=./rebar3

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

test:compile
	$(REBAR) as test do eunit && $(REBAR) as test ct --verbose

typecheck:
	$(REBAR) dialyzer

ci:compile
	$(REBAR) dialyzer && $(REBAR) as test do eunit && $(REBAR) as test ct --verbose

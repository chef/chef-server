DEPS=$(CURDIR)/deps

all: compile eunit

clean:
	@rebar clean

compile: $(DEPS) check_calls
	@rebar compile

$(DEPS):
	@rebar get-deps

distclean:
	@rm -rf deps
	@rebar skip_deps=true clean

eunit:
	@rebar skip_deps=true eunit

test: eunit

dialyzer:
	@dialyzer -Wrace_conditions -Wunderspecs -r ebin

check_calls:
	@./check_calls

.PHONY: check_calls

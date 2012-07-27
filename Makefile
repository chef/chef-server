ERL = $(shell which erl)

ERLFLAGS= -pa $(CURDIR)/.eunit -pa $(CURDIR)/ebin -pa $(CURDIR)/*/ebin

REBAR=$(shell which rebar)

ifeq ($(REBAR),)
	$(error "Rebar not available on this system")
endif

all: compile eunit dialyzer

compile:
	@$(REBAR) compile

doc:
	@$(REBAR) doc

clean:
	@$(REBAR) clean

eunit: compile
	@$(REBAR) skip_deps=true eunit

dialyzer:
	@dialyzer -Wrace_conditions -r ebin

typer:
	typer -r ./src

shell: compile
# You often want *rebuilt* rebar tests to be available to the
# shell you have to call eunit (to get the tests
# rebuilt). However, eunit runs the tests, which probably
# fails (thats probably why You want them in the shell). This
# runs eunit but tells make to ignore the result.
	- @$(REBAR) eunit
	@$(ERL) $(ERLFLAGS)

distclean: clean
	@rm -rvf $(CURDIR)/deps/*

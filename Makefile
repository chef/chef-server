ERL = $(shell which erl)
PLTFILE = .depsolver.plt

ERLFLAGS= -pa ebin -pa $(CURDIR)/*/ebin

REBAR=$(shell which rebar)

ifeq ($(REBAR),)
	$(error "Rebar not available on this system")
endif

all: compile test

compile:
	@$(REBAR) compile

doc:
	@$(REBAR) doc

clean:
	@$(REBAR) clean

test: eunit

eunit:
	@$(REBAR) eunit

$(PLTFILE):
	@dialyzer --build_plt --apps stdlib crypto erts kernel public_key eunit --output_plt $(PLTFILE)

dialyzer: $(PLTFILE)
	@dialyzer --plt $(PLTFILE) -c ./src --src

clean-plt:
	rm -f $(PLTFILE)

shell: compile
# You often want *rebuilt* rebar tests to be available to the
# shell you have to call eunit (to get the tests
# rebuilt). However, eunit runs the tests, which probably
# fails (thats probably why You want them in the shell). This
# runs eunit but tells make to ignore the result.
	- @$(REBAR) eunit
	@$(ERL) $(ERLFLAGS)

distclean: clean clean-plt

ERL = $(shell which erl)
PLTFILE = .depsolver.plt

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

check-plt: $(PLTFILE)
	@dialyzer --plt $(PLTFILE) -c ./src --src

clean-plt:
	rm -f $(PLTFILE)

shell: compile
	@$(ERL) -pa ebin

distclean: clean clean-plt

PLT_DIR=$(CURDIR)/.plt
PLT=$(PLT_DIR)/dialyzer_plt
REBAR=$(shell which rebar)

ifeq ($(REBAR),)
	$(error "Rebar not available on this system")
endif

.PHONY: all deps compile test eunit ct rel/bookshelf rel doc build-plt \
	check-plt clean-plt

all : rel

deps :
	$(REBAR) get-deps

compile : deps
	$(REBAR) compile

test : eunit ct

eunit : compile
	$(REBAR) skip_deps=true eunit

ct : eunit
	$(REBAR) skip_deps=true ct

rel/bookshelf :
	$(REBAR) generate

rel : compile rel/bookshelf

doc:
	$(REBAR) doc

build-plt: compile
	$(REBAR) build-plt

check-plt: compile
	$(REBAR) check-plt

$(PLT): compile
	mkdir -p $(PLT_DIR)
	dialyzer --build_plt --output_plt $(PLT) \
		--apps erts kernel stdlib eunit compiler crypto

clean_plt:
	rm -rf $(PLT_DIR)

dialyzer: compile $(PLT)
	dialyzer --src --plt $(PLT) $(TEST_PLT) \
	-c ./bookshelf_store/src \
	-c ./bookshelf_wi/src

typer: compile $(PLT)
	typer --plt $(PLT) -r ./src

clean :
	$(REBAR) skip_deps=true clean
	rm -rf rel/bookshelf

distclean : clean clean-plt
	@git clean -fdx

start :
	@rel/bookshelf/bin/bookshelf start

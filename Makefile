LIBDIR=$(CURDIR)/lib
DEPS=$(CURDIR)/deps
PLT_DIR=$(CURDIR)/.plt
PLT=$(PLT_DIR)/dialyzer_plt
REBAR=$(shell which rebar)

ERLPATH= -pa $(DEPS)/cowboy/ebin -pa $(DEPS)/covertool/ebin \
	-pa $(DEPS)/edown/ebin -pa $(DEPS)/erlsom/ebin \
	-pa $(DEPS)/gen_leader/ebin \
	-pa $(DEPS)/gproc/ebin -pa $(DEPS)/iso8601/ebin \
	-pa $(DEPS)/opset/ebin -pa $(DEPS)/proper/ebin

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
# fixing more rebar idiocy
	ERL_FLAGS="-pa $(CURDIR)/lib/bookshelf_store/ebin" $(REBAR) skip_deps=true eunit

ct : eunit
	$(REBAR) skip_deps=true ct

rel/bookshelf :
	$(REBAR) generate

rel : compile rel/bookshelf

doc:
	$(REBAR) doc

$(PLT):
	mkdir -p $(PLT_DIR)
	dialyzer --build_plt --output_plt $(PLT) \
		$(ERLPATH) \
		--apps erts kernel stdlib eunit compiler crypto \
		cowboy edown inets erlsom gen_leader gproc iso8601 opset proper

clean_plt:
	rm -rf $(PLT_DIR)

dialyzer: $(PLT)
	@rebar compile
	dialyzer --no_check_plt --src --plt $(PLT) \
	$(ERLPATH) \
	-pa $(LIBDIR)/bookshelf_store/ebin \
	-pa $(LIBDIR)/bookshelf_wi/ebin \
	-c $(LIBDIR)/bookshelf_store/src \
	-c $(LIBDIR)/bookshelf_wi/src

typer: compile $(PLT)
	typer --plt $(PLT) -r $(LIBDIR)/bookshelf_store/src \
		-r $(LIBDIR)/bookshelf_wi/src

shell: compile
	erl -env ERL_LIBS $(CURDIR)/deps -pa $(CURDIR)/lib/bookshelf_store/ebin\
	 -pa $(CURDIR)/lib/bookshelf_wi/ebin -boot start_sasl -s bksw_app manual_start


clean :
	$(REBAR) skip_deps=true clean
	rm -rf rel/bookshelf
	rm -rf lib/bookshelf_store/logs
	rm -rf lib/bookshelf_wi/logs

distclean : clean clean-plt
	@git clean -fdx

start :
	@rel/bookshelf/bin/bookshelf start

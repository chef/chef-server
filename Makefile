LIBDIR=$(CURDIR)/lib
DEPS=$(CURDIR)/deps
PLT_DIR=$(CURDIR)/.plt
PLT=$(PLT_DIR)/dialyzer_plt
REBAR=$(shell which rebar)

ERLPATH= -pa $(DEPS)/webmachine/ebin -pa $(DEPS)/covertool/ebin \
	-pa $(DEPS)/edown/ebin -pa $(DEPS)/erlsom/ebin \
	-pa $(DEPS)/gen_leader/ebin \
	-pa $(DEPS)/gproc/ebin -pa $(DEPS)/iso8601/ebin \
	-pa $(DEPS)/mini_s3/ebin

ifeq ($(REBAR),)
	$(error "Rebar not available on this system")
endif

.PHONY: all deps compile test eunit ct rel/bookshelf rel doc build-plt \
	check-plt clean-plt

all: compile eunit dialyzer

clean:
	@$(REBAR) skip_deps=true clean
	@rm -rf ebin_dialyzer

clean_plt:
	rm -rf $(PLT_DIR)

distclean: clean clean-plt
	@rm -rf deps

allclean:
	@$(REBAR) clean

update: compile
	@cd rel/bookshelf

compile: $(DEPS)
	@$(REBAR) compile

dialyzer: $(PLT)
	@$(REBAR) compile
	dialyzer --no_check_plt -Wno_undefined_callbacks --src --plt $(PLT) \
	$(ERLPATH) \
	-pa $(LIBDIR)/bookshelf_store/ebin \
	-pa $(LIBDIR)/bookshelf_wi/ebin \
	-c $(LIBDIR)/bookshelf_store/src \
	-c $(LIBDIR)/bookshelf_wi/src

$(DEPS):
	@$(REBAR) get-deps

eunit: compile
	@$(REBAR) skip_deps=true eunit

test: eunit

rel/bookshelf:
	$(REBAR) generate

rel: compile rel/bookshelf

devrel: rel
	@/bin/echo -n Symlinking deps and apps into release
	@$(foreach dep,$(wildcard deps/*), /bin/echo -n .;rm -rf rel/bookshelf/lib/$(shell basename $(dep))-* \
	   && ln -sf $(abspath $(dep)) rel/bookshelf/lib;)
	@$(foreach app,$(wildcard lib/*), /bin/echo -n .;rm -rf rel/bookshelf/lib/$(shell basename $(app))-* \
	   && ln -sf $(abspath $(app)) rel/bookshelf/lib;)
	@/bin/echo done.
	@/bin/echo  Run \'make update\' to pick up changes in a running VM.

relclean:
	@rm -rf rel/bookshelf

# test : eunit ct

# eunit: compile
# 	# fixing more rebar idiocy
# 	ERL_FLAGS="-pa $(CURDIR)/lib/bookshelf_store/ebin" $(REBAR) skip_deps=true eunit

# ct : eunit
# 	ERL_FLAGS="-pa $(CURDIR)/lib/bookshelf_store/ebin" $(REBAR) skip_deps=true ct

# doc:
# 	$(REBAR) doc

# $(PLT):
# 	mkdir -p $(PLT_DIR)
# 	- dialyzer --build_plt --output_plt $(PLT) \
# 		$(ERLPATH) \
# 		--apps erts kernel stdlib eunit compiler crypto \
# 		webmachine edown inets erlsom gen_leader gproc iso8601 \
# 		xmerl mini_s3 mochiweb
# 	@if test ! -f $(PLT); then exit 2; fi

# typer: compile $(PLT)
# 	typer --plt $(PLT) -r $(LIBDIR)/bookshelf_store/src \
# 		-r $(LIBDIR)/bookshelf_wi/src

# shell: compile
# 	erl -env ERL_LIBS $(CURDIR)/deps -pa $(CURDIR)/lib/bookshelf_store/ebin\
# 	 -pa $(CURDIR)/lib/bookshelf_wi/ebin -boot start_sasl -s bksw_app manual_start

# start :
# 	@rel/bookshelf/bin/bookshelf start

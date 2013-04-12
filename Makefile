use_locked_config = $(wildcard USE_REBAR_LOCKED)
ifeq ($(use_locked_config),USE_REBAR_LOCKED)
  rebar_config = rebar.config.lock
else
  rebar_config = rebar.config
endif
REBAR = rebar -C $(rebar_config)

all: compile eunit dialyze

# Cleaning #####################################################################
clean:
	$(REBAR) clean

depclean:
	@echo "Deleting all dependencies"
	@rm -rf deps

relclean:
	@echo "Deleting rel/bifrost directory"
	@rm -rf rel/bifrost

allclean: depclean clean

distclean: relclean allclean

# Dependency Fetching ##########################################################

DEPS = $(CURDIR)/deps

$(DEPS):
	$(REBAR) get-deps

# Compilation ##################################################################
compile: $(DEPS)
	$(REBAR) compile

compile_app:
	$(REBAR) skip_deps=true compile

# Dialyzer #####################################################################

DIALYZER_OPTS = -Wrace_conditions -Wunderspecs

DEPS_PLT = bifrost.plt

ERLANG_DIALYZER_APPS = asn1 \
		       compiler \
		       crypto \
		       edoc \
		       edoc \
		       erts \
		       gs \
		       hipe \
		       inets \
		       kernel \
		       mnesia \
		       mnesia \
		       observer \
		       public_key \
		       runtime_tools \
		       runtime_tools \
		       ssl \
		       stdlib \
		       syntax_tools \
		       syntax_tools \
		       tools \
		       webtool \
		       xmerl

$(DEPS_PLT):
	dialyzer -nn --output_plt $(DEPS_PLT) --build_plt --apps $(ERLANG_DIALYZER_APPS)

dialyze: compile $(DEPS_PLT)
	dialyzer $(DIALYZER_OPTS) --plt $(DEPS_PLT) -r apps/bifrost/ebin

# Testing ######################################################################

test: eunit

eunit: compile
	$(REBAR) eunit skip_deps=true

eunit_app: compile_app
	$(REBAR) eunit apps=bifrost skip_deps=true

# Release Creation #############################################################

rel: compile test rel/bifrost

rel/bifrost:
	@cd rel
	$(REBAR) generate
	@echo '        (      (      (         )    (              '
	@echo '   (    )\ )   )\ )   )\ )   ( /(    )\ )    *   )  '
	@echo ' ( )\  (()/(  (()/(  (()/(   )\())  (()/(  ` )  /(  '
	@echo ' )((_)  /(_))  /(_))  /(_)) ((_)\    /(_))  ( )(_)) '
	@echo '((_)_  (_))   (_))_| (_))    []_[]  (_))   (_(_())  '
	@echo ' | _ ) |_ _|  | |_   | _ \   / _ \  / __|  |_   _|  '
	@echo ' | _ \  | |   | __|  |   /  | (_) | \__ \    | |    '
	@echo ' |___/ |___|  |_|    |_|_\   \___/  |___/    |_|    '

#
# Unsure if we'll need these targets anymore...
#

# devrel: rel
#	@/bin/echo -n Symlinking deps and apps into release
#	@$(foreach lib,$(wildcard apps/* deps/*), /bin/echo -n .;rm -rf rel/bifrost/lib/$(shell basename $(lib))-* \
#	   && ln -sf $(abspath $(lib)) rel/bifrost/lib;)
#	@/bin/echo done.
#	@/bin/echo  Run \'make update\' to pick up changes in a running VM.

# update: compile
#	@cd rel/bifrost;bin/bifrost restart

# update_app: compile_app
#	@cd rel/bifrost;bin/bifrost restart


# Release Preparation ##########################################################

BUMP ?= patch
prepare_release: distclean unlocked_deps unlocked_compile update_locked_config rel
	@echo 'release prepared, bumping version'
	@$(REBAR) bump-rel-version version=$(BUMP)

unlocked_deps:
	@echo 'Fetching deps as: rebar -C rebar.config'
	@rebar -C rebar.config get-deps

# When running the prepare_release target, we have to ensure that a
# compile occurs using the unlocked rebar.config. If a dependency has
# been removed, then using the locked version that contains the stale
# dep will cause a compile error.
unlocked_compile:
	@rebar -C rebar.config compile

update_locked_config:
	@rebar lock-deps ignore=meck skip_deps=true

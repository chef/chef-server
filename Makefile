DEPS=$(CURDIR)/deps

##DIALYZER_OPTS = -Wunderspecs
DIALYZER_OPTS = 

# List dependencies that should be included in a cached dialyzer PLT file.
DIALYZER_DEPS = deps/erlsom/ebin \
                deps/ibrowse/ebin \
                deps/iso8601/ebin \
                deps/mini_s3/ebin \
                deps/mochiweb/ebin \
                deps/webmachine/ebin

DEPS_PLT = bookshelf.plt

ERLANG_DIALYZER_APPS = asn1 \
                       compiler \
                       crypto \
                       edoc \
                       edoc \
                       erts \
                       eunit \
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

# The release branch should have a file named USE_REBAR_LOCKED
use_locked_config = $(wildcard USE_REBAR_LOCKED)
ifeq ($(use_locked_config),USE_REBAR_LOCKED)
  rebar_config = rebar.config.lock
else
  rebar_config = rebar.config
endif
REBAR = rebar -C $(rebar_config)

all: compile test dialyzer

compile: $(DEPS)
	@$(REBAR) compile

# Only include local PLT if we have deps that we are going to analyze
ifeq ($(strip $(DIALYZER_DEPS)),)
dialyzer: ~/.dialyzer_plt
	@dialyzer $(DIALYZER_OPTS) -r ebin
else
dialyzer: ~/.dialyzer_plt $(DEPS_PLT)
	@dialyzer $(DIALYZER_OPTS) --plts ~/.dialyzer_plt $(DEPS_PLT) -r ebin

$(DEPS_PLT):
	@dialyzer --build_plt $(DIALYZER_DEPS) --output_plt $(DEPS_PLT)
endif

~/.dialyzer_plt:
	@echo "ERROR: Missing ~/.dialyzer_plt. Please wait while a new PLT is compiled."
	dialyzer --build_plt --apps $(ERLANG_DIALYZER_APPS)
	@echo "now try your build again"

test: ct

eunit:
	$(REBAR) skip_deps=true eunit

ct: eunit
	rm -rf /tmp/bukkits
	mkdir -p /tmp/bukkits
	ERL_LIBS=`pwd`/deps:`pwd`/lib:$(ERL_LIBS) $(REBAR) skip_deps=true ct

compile_skip:
	@$(REBAR) compile skip_deps=true

# For a release-only project, this won't make much sense, but could be
# useful for release projects that have their own code
clean:
	@$(REBAR) clean skip_deps=true

allclean:
	@$(REBAR) clean

update: compile
	@cd rel/bookshelf;bin/bookshelf restart

distclean: relclean
	@rm -rf deps $(DEPS_PLT)
	@$(REBAR) clean

tags: TAGS

TAGS:
	find deps -name "*.[he]rl" -print | etags -

# Only do munge_apps if we have files in src/
all_src_files = $(wildcard src/*)
ifeq ($(strip $(all_src_files)),)
munge_apps:
	@true
else
munge_apps:
	@mkdir -p rel/apps/bookshelf
	@ln -sf `pwd`/ebin rel/apps/bookshelf
	@ln -sf `pwd`/priv rel/apps/bookshelf
	@cp rebar.config rel
	@echo '{deps_dir, ["../deps"]}.' >> rel/rebar.config
endif

generate: munge_apps
	@/bin/echo 'building OTP release package for bookshelf'
	@/bin/echo "using rebar as: $(REBAR)"
	@cd rel;$(REBAR) generate
	@rm -rf rel/apps rel/rebar.config
	@/bin/echo
	@/bin/echo '@@@@@@@    @@@@@@    @@@@@@   @@@  @@@   @@@@@@   @@@  @@@  @@@@@@@@  @@@       @@@@@@@@  '
	@/bin/echo '@@@@@@@@  @@@@@@@@  @@@@@@@@  @@@  @@@  @@@@@@@   @@@  @@@  @@@@@@@@  @@@       @@@@@@@@  '
	@/bin/echo '@@!  @@@  @@!  @@@  @@!  @@@  @@!  !@@  !@@       @@!  @@@  @@!       @@!       @@!       '
	@/bin/echo '!@   @!@  !@!  @!@  !@!  @!@  !@!  @!!  !@!       !@!  @!@  !@!       !@!       !@!       '
	@/bin/echo '@!@!@!@   @!@  !@!  @!@  !@!  @!@@!@!   !!@@!!    @!@!@!@!  @!!!:!    @!!       @!!!:!    '
	@/bin/echo '!!!@!!!!  !@!  !!!  !@!  !!!  !!@!!!     !!@!!!   !!!@!!!!  !!!!!:    !!!       !!!!!:    '
	@/bin/echo '!!:  !!!  !!:  !!!  !!:  !!!  !!: :!!        !:!  !!:  !!!  !!:       !!:       !!:       '
	@/bin/echo ':!:  !:!  :!:  !:!  :!:  !:!  :!:  !:!      !:!   :!:  !:!  :!:        :!:      :!:       '
	@/bin/echo ' :: ::::  ::::: ::  ::::: ::   ::  :::  :::: ::   ::   :::   :: ::::   :: ::::   ::       '
	@/bin/echo ':: : ::    : :  :    : :  :    :   :::  :: : :     :   : :  : :: ::   : :: : :   :        '
	@/bin/echo

rel: rel/bookshelf

devrel: rel
	@/bin/echo -n Symlinking deps and apps into release
	@$(foreach dep,$(wildcard deps/*), /bin/echo -n .;rm -rf rel/bookshelf/lib/$(shell basename $(dep))-* \
	   && ln -sf $(abspath $(dep)) rel/bookshelf/lib;)
	@rm -rf rel/bookshelf/lib/bookshelf-*;mkdir -p rel/bookshelf/lib/bookshelf
	@ln -sf `pwd`/ebin rel/bookshelf/lib/bookshelf
	@ln -sf `pwd`/priv rel/bookshelf/lib/bookshelf
	@/bin/echo done.
	@/bin/echo  Run \'make update\' to pick up changes in a running VM.

rel/bookshelf: compile generate

relclean:
	@rm -rf rel/bookshelf

$(DEPS):
	@echo "Fetching deps as: $(REBAR)"
	@$(REBAR) get-deps

BUMP ?= patch
prepare_release: distclean unlocked_deps unlocked_compile update_locked_config rel
	@echo 'release prepared, bumping $(BUMP) version'
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

.PHONY: distclean prepare_release update_locked_config unlocked_deps unlocked_compile update clean compile compile_skip allclean tags relclean devrel rel relclean generate munge_apps test

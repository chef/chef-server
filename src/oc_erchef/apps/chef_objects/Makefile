DEPS = $(CURDIR)/deps

DIALYZER_OPTS =

DIALYZER_DEPS = deps/chef_authn/ebin \
                deps/ej/ebin \
                deps/jiffy/ebin \
                deps/ibrowse/ebin \
                deps/bcrypt/ebin \
                deps/mini_s3/ebin

DEPS_PLT = chef_objects.plt

ERLANG_DIALYZER_APPS = asn1 \
                       compiler \
                       crypto \
                       edoc \
                       edoc \
                       erts \
                       eunit \
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
                       xmerl

all: compile eunit dialyzer

# Clean ebin and .eunit of this project
clean:
	@rebar clean skip_deps=true

# Clean this project and all deps
allclean:
	@rebar clean

compile: $(DEPS)
	@rebar compile

$(DEPS):
	@rebar get-deps

bundle:
	@cd priv/depselector_rb; bundle install --path .bundle

# Full clean and removal of all deps. Remove deps first to avoid
# wasted effort of cleaning deps before nuking them.
distclean:
	@rm -rf deps $(DEPS_PLT) priv/depselector_rb/.bundle
	@rebar clean

eunit: compile bundle
	@rebar skip_deps=true eunit

test: eunit

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

# ignore exit code of dialyzer command because some modules cause
# warnings, but those are ok to disregard
~/.dialyzer_plt:
	@echo "ERROR: Missing ~/.dialyzer_plt. Please wait while a new PLT is compiled."
	-dialyzer --build_plt --apps $(ERLANG_DIALYZER_APPS)
	@echo "now try your build again"

doc:
	@rebar doc skip_deps=true

tags:
	find src deps -name "*.[he]rl" -print | etags -

.PHONY: all compile eunit test dialyzer clean allclean distclean doc tags

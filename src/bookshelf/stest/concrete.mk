DEPS ?= $(CURDIR)/deps

DIALYZER_OPTS ?= -Wunderspecs

# Find all the deps the project has by searching the deps dir
ALL_DEPS = $(notdir $(wildcard deps/*))
# Create a list of deps that should be used by dialyzer by doing a
# complement on the sets
DEPS_LIST = $(filter-out $(DIALYZER_SKIP_DEPS), $(ALL_DEPS))
# Create the path structure from the dep names
# so dialyzer can find the .beam files in the ebin dir
# This list is then used by dialyzer in creating the local PLT
DIALYZER_DEPS = $(foreach dep,$(DEPS_LIST),deps/$(dep)/ebin)

DEPS_PLT = deps.plt

ERLANG_DIALYZER_APPS = asn1 \
                       compiler \
                       crypto \
                       edoc \
                       erts \
                       eunit \
                       gs \
                       hipe \
                       inets \
                       kernel \
                       mnesia \
                       observer \
                       public_key \
                       runtime_tools \
                       ssl \
                       stdlib \
                       syntax_tools \
                       tools \
                       webtool \
                       xmerl

all: .concrete/DEV_MODE compile eunit dialyzer $(ALL_HOOK)

.concrete/DEV_MODE:
	@mkdir -p .concrete
	@touch $@

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

# Full clean and removal of all deps. Remove deps first to avoid
# wasted effort of cleaning deps before nuking them.
distclean:
	@rm -rf deps $(DEPS_PLT)
	@rebar clean

eunit:
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

~/.dialyzer_plt:
	@echo "ERROR: Missing ~/.dialyzer_plt. Please wait while a new PLT is compiled."
	dialyzer --build_plt --apps $(ERLANG_DIALYZER_APPS)
	@echo "now try your build again"

doc:
	@rebar doc skip_deps=true

tags:
	find src deps -name "*.[he]rl" -print | etags -

.PHONY: all compile eunit test dialyzer clean allclean distclean doc tags

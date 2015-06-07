# =============================================================================
# Verify that the programs we need to run are installed on this system
# =============================================================================
ERL = $(shell which erl)

ifeq ($(ERL),)
$(error "Erlang not available on this system")
endif

# If building on travis, use the rebar in the current directory
ifeq ($(TRAVIS),true)
REBAR = $(CURDIR)/rebar
endif

# If there is a rebar in the current directory, use it
ifeq ($(wildcard rebar),rebar)
REBAR = $(CURDIR)/rebar
endif

# Fallback to rebar on PATH
REBAR ?= $(shell which rebar)

# And finally, prep to download rebar if all else fails
ifeq ($(REBAR),)
REBAR = $(CURDIR)/rebar
endif

# If we have a rebar.config.lock file, use it!
ifeq ($(wildcard rebar.config.lock),rebar.config.lock)
REBAR_CONFIG = rebar.config.lock
else
REBAR_CONFIG = rebar.config
endif

# This is the variable to use to respect the lock file
REBARC = $(REBAR) -C $(REBAR_CONFIG)

# For use on Travis CI, skip dialyzer for R14 and R15. Newer versions
# have a faster dialyzer that is less likely to cause a build timeout.
SKIP_DIALYZER ?= false
DIALYZER = dialyzer
R14 = $(findstring R14,$(TRAVIS_OTP_RELEASE))
R15 = $(findstring R15,$(TRAVIS_OTP_RELEASE))
ifneq ($(R14),)
DIALYZER = echo "SKIPPING dialyzer"
endif
ifneq ($(R15),)
DIALYZER = echo "SKIPPING dialyzer"
endif
ifneq ($(SKIP_DIALYZER),false)
DIALYZER = echo "SKIPPING dialyzer"
endif

REBAR_URL=https://github.com/rebar/rebar/wiki/rebar

DEPS ?= $(CURDIR)/deps

DIALYZER_OPTS ?=

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

ERLANG_DIALYZER_APPS ?= asn1 \
                        compiler \
                        crypto \
                        edoc \
                        erts \
                        inets \
                        kernel \
                        mnesia \
                        public_key \
                        ssl \
                        stdlib \
                        syntax_tools \
                        tools \
                        xmerl

PROJ ?= $(notdir $(CURDIR))

# Let's compute $(BASE_PLT_ID) that identifies the base PLT to use for this project
# and depends on your `$(ERLANG_DIALYZER_APPS)' list and your erlang version
ERLANG_VERSION := $(shell ERL_FLAGS="" $(ERL) -eval 'io:format("~s~n", [erlang:system_info(otp_release)]), halt().' -noshell)
MD5_BIN := $(shell which md5 || which md5sum)
ifeq ($(MD5_BIN),)
# neither md5 nor md5sum, we just take the project name
BASE_PLT_ID := $(PROJ)
else
BASE_PLT_ID := $(word 1, $(shell echo $(ERLANG_DIALYZER_APPS) | $(MD5_BIN)))
endif

ifeq ($(TRAVIS),true)
## If we're running on travis, pull the plt from S3
## We got them from https://github.com/esl/erlang-plts
## To add to the collection make sure they're public and match the erlang version
## reported by erlang:system_info(otp_release)
## s3cmd put --acl-public --guess-mime-type <FILENAME> s3://concrete-plts

BASE_PLT := travis-erlang-$(TRAVIS_OTP_RELEASE).plt
BASE_PLT_URL := http://s3.amazonaws.com/concrete-plts/$(BASE_PLT)
else
BASE_PLT := ~/.concrete_dialyzer_plt_$(BASE_PLT_ID)_$(ERLANG_VERSION).plt
endif

## The recursive call to $(MAKE) is here to make sure that we use a verion of
## DIALYZER_DEPS that's built after $(DEPS) is made. If we didn't do this,
## dialyzer wouldn't be able to figure out that it needs to build deps.plt
all: .concrete/DEV_MODE $(DEPS)
	@$(MAKE) all_but_dialyzer dialyzer

all_but_dialyzer: .concrete/DEV_MODE compile $(ALL_HOOK) eunit

$(REBAR):
	curl -Lo rebar $(REBAR_URL) || wget $(REBAR_URL)
	chmod a+x rebar

get-rebar: $(REBAR)

.concrete/DEV_MODE:
	@mkdir -p .concrete
	@touch $@

# Clean ebin and .eunit of this project
clean: $(CLEAN_HOOK)
	@$(REBARC) clean skip_deps=true

# Clean this project and all deps
# Newer rebar requires -r to recursively clean
allclean:
	@($(REBARC) --help 2>&1|grep -q recursive && $(REBARC) -r clean) || $(REBARC) clean

compile: $(DEPS)
	@$(REBARC) compile

$(DEPS):
	@$(REBARC) get-deps

# Full clean and removal of all deps. Remove deps first to avoid
# wasted effort of cleaning deps before nuking them.
distclean:
	@rm -rf deps $(DEPS_PLT)
	@$(REBARC) clean

eunit:
	@$(REBARC) skip_deps=true eunit

test: eunit

## Override DIALYZER_SRC if your beams live somewhere else
DIALYZER_SRC ?= -r ebin

# Only include local PLT if we have deps that we are going to analyze
ifeq ($(strip $(DIALYZER_DEPS)),)
dialyzer: $(BASE_PLT)
	@$(DIALYZER) $(DIALYZER_OPTS) --plts $(BASE_PLT) $(DIALYZER_SRC)
else
dialyzer: $(BASE_PLT) $(DEPS_PLT)
	@$(DIALYZER) $(DIALYZER_OPTS) --plts $(BASE_PLT) $(DEPS_PLT) $(DIALYZER_SRC)

$(DEPS_PLT):
	@$(DIALYZER) --build_plt $(DIALYZER_DEPS) --plts $(BASE_PLT) --output_plt $(DEPS_PLT)
endif

$(BASE_PLT):
ifeq ($(TRAVIS),true)
		@echo "Attempting to download PLT: $(BASE_PLT_URL)."
		-wget $(BASE_PLT_URL)

		@if [ -f $(BASE_PLT) ] ; then \
			echo "Downloaded PLT successfully to $(BASE_PLT)" ; \
		else \
			echo "Download failed. Please wait while a new PLT is compiled." ; \
			$(DIALYZER) --build_plt --apps $(ERLANG_DIALYZER_APPS) --output_plt $(BASE_PLT) ; \
			echo "now try your build again" ; \
		fi;

else
		@echo "Missing $(BASE_PLT). Please wait while a new PLT is compiled."
		$(DIALYZER) --build_plt --apps $(ERLANG_DIALYZER_APPS) --output_plt $(BASE_PLT)
		@echo "now try your build again"
endif

doc:
	@$(REBARC) doc skip_deps=true

shell:
	@$(ERL) -pa deps/*/ebin ebin .eunit

tags:
	find src deps -name "*.[he]rl" -print | etags -

# Releases via relx. we will install a local relx, as we do for rebar,
# if we don't find one on PATH.
RELX_CONFIG ?= $(CURDIR)/relx.config
RELX = $(shell which relx)
RELX_OPTS ?=
RELX_OUTPUT_DIR ?= _rel
ifeq ($(RELX),)
RELX = $(CURDIR)/relx
RELX_VERSION = 1.0.4
else
RELX_VERSION = $(shell relx --version)
endif
RELX_URL = https://github.com/erlware/relx/releases/download/v$(RELX_VERSION)/relx

# relx introduced a breaking change in v1: the output doesn't have the same structure
# see https://github.com/erlware/relx/releases/tag/v1.0.0
ifeq ($(shell echo $(RELX_VERSION) | head -c 1), 0)
RELX_RELEASE_DIR = $(RELX_OUTPUT_DIR)
else
RELX_RELEASE_DIR = $(RELX_OUTPUT_DIR)/$(PROJ)
endif

REL_HOOK ?= all_but_dialyzer

$(RELX):
	curl -Lo relx $(RELX_URL) || wget $(RELX_URL)
	chmod a+x relx

rel: relclean $(REL_HOOK) $(RELX)
	@$(RELX) -c $(RELX_CONFIG) -o $(RELX_OUTPUT_DIR) $(RELX_OPTS)

devrel: relclean $(REL_HOOK) $(RELX)
	@$(RELX) --dev-mode -c $(RELX_CONFIG) -o $(RELX_OUTPUT_DIR) $(RELX_OPTS)

relclean:
	rm -rf $(RELX_OUTPUT_DIR)

## Release prep and dep locking. These recipes use $(REBAR), not
## $(REBARC) in order to NOT use the lock file since they are
## concerned with the task of updating the lock file.
BUMP ?= patch
prepare_release: distclean unlocked_deps unlocked_compile update_locked_config rel
	@echo 'release prepared, bumping version'
	@$(REBAR) bump-rel-version version=$(BUMP)

unlocked_deps:
	@echo 'Fetching deps as: rebar -C rebar.config'
	@$(REBAR) -C rebar.config get-deps

# When running the prepare_release target, we have to ensure that a
# compile occurs using the unlocked rebar.config. If a dependency has
# been removed, then using the locked version that contains the stale
# dep will cause a compile error.
unlocked_compile:
	@$(REBAR) -C rebar.config compile

update_locked_config:
	@$(REBAR) lock-deps skip_deps=true


.PHONY: all all_but_dialyzer compile eunit test dialyzer clean allclean relclean distclean doc tags get-rebar rel devrel

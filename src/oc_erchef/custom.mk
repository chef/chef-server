SHELL := /bin/bash

PROJ = oc_erchef
DEVVM_PROJ = opscode-erchef

ALL_HOOK = bundle
CLEAN_HOOK = bundle_clean
REL_HOOK = compile bundle

GECODE_URL=https://chef-travis-ci-cache.s3.amazonaws.com/ubuntu-12.04/vendored-gecode.tar.gz
CT_DIR = common_test

ERLANG_DIALYZER_APPS = asn1 \
                       compiler \
                       crypto \
                       edoc \
                       eldap \
                       erts \
                       inets \
                       kernel \
                       mnesia \
                       public_key \
                       sasl \
                       ssl \
                       stdlib \
                       syntax_tools \
                       tools \
                       xmerl

DIALYZER_OPTS =

DIALYZER_SRC = -r apps/chef_db/ebin -r apps/chef_index/ebin -r apps/chef_objects/ebin -r apps/depsolver/ebin -r apps/oc_chef_authz/ebin -r apps/oc_chef_wm/ebin

ct: clean_ct compile
	time $(REBARC) ct skip_deps=true

ct_fast: clean_ct
	time $(REBARC) compile ct skip_deps=true

# Runs a specific test suite
# e.g. make ct_deliv_hand_user_authn
# supports a regex as argument, as long as it only matches one suite

APPS = $(notdir $(wildcard apps/*))

ct_%: clean_ct
	@ EXTRAS=$$(if [ -f "$(CT_DIR)/$*_SUITE.erl" ]; then \
		echo "$*"; \
	else \
		FIND_RESULT=$$(find "." -name "*$**_SUITE\.erl"); \
		[ -z "$$FIND_RESULT" ] && echo "No suite found with input '$*'" 1>&2 && exit 1; \
		NB_MACTHES=$$(echo "$$FIND_RESULT" | wc -l) && [[ $$NB_MACTHES != "       1" ]] && echo -e "Found $$NB_MACTHES suites matching input:\n$$FIND_RESULT" 1>&2 && exit 1; \
		SUITE=$$(echo "$$FIND_RESULT" | perl -wlne 'print $$1 if /\/([^\/]+)_SUITE\.erl/') && \
		APP=$$(echo "$$FIND_RESULT" | perl -wlne 'print $$1 if /\.\/apps\/([^\/]+)\/.*\/[^\/]+_SUITE\.erl/') && \
		SKIP_APPS=$$(echo "$(APPS)" | sed "s/$$APP//" | sed -E "s/[ ]+/,/g") && \
		echo "suites=$$SUITE skip_apps=$$SKIP_APPS"; \
	fi) && COMMAND="time $(REBAR) ct $$EXTRAS skip_deps=true" && echo $$COMMAND && eval $$COMMAND;

clean_ct:
	@rm -f $(CT_DIR)/*.beam
	@rm -rf logs

## Pull in devvm.mk for relxy goodness
include devvm.mk

bundle_clean:
	@cd apps/chef_objects/priv/depselector_rb; rm -rf .bundle

bundle:
	@echo bundling up depselector, This might take a while...
	@cd apps/chef_objects/priv/depselector_rb; bundle install --deployment --path .bundle

install:
	curl -Lo vendored-gecode.tar.gz $(GECODE_URL) || wget $(GECODE_URL)
	tar xf vendored-gecode.tar.gz
	cpanm --notest --quiet --local-lib=$(HOME)/perl5 local::lib
	cpanm --notest --quiet App::Sqitch
	@./rebar get-deps -C rebar.config.lock
	rm -f $(HOME)/.cpanm/work/*/build.log
	rm -f $(HOME)/.cpanm/build.log

travis: all
	PATH=~/perl5/bin:$(PATH) $(REBARC) skip_deps=true ct

DEVVM_DIR = $(DEVVM_ROOT)/_rel/oc_erchef

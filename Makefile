DEPS=$(CURDIR)/deps

## Set the environment variable $DB_TYPE to either mysql or pgsql
## to run the correct integration tests.
-include itest/$(DB_TYPE)_conf.mk

all: compile eunit dialyzer

clean:
	@rebar skip_deps=true clean

allclean:
	@rebar clean

distclean:
	@rebar skip_deps=true clean
	@rm -rf deps

compile: $(DEPS)
	@rebar compile

# Not yet since there are a *ton* of warnings that we need to weed through.
# @dialyzer -Wrace_conditions -Wunderspecs -r ebin

dialyzer:
	@dialyzer -Wrace_conditions -Wunderspecs -nn -r ebin

$(DEPS):
	@rebar get-deps

eunit: compile
	@rebar skip_deps=true eunit

test: eunit

tags:
	find src deps -name "*.[he]rl" -print | etags -

itest_bundler:
	cd itest; bundle install --binstubs

itest_create:
	@echo Creating integration test database
	@cd itest;bundle exec ./create_schema.rb ${DB_TYPE} create

itest_clean:
	@rm -f itest/*.beam
	@echo Dropping integration test database
	@cd itest;bundle exec ./create_schema.rb ${DB_TYPE} destroy

itest: compile itest_bundler itest_create itest_run itest_clean

itest_run:
	cd itest;erlc -pz ../deps/chef_objects/ebin *.erl
	@erl -pa deps/*/ebin -pa ebin -pa itest -noshell -eval "eunit:test(itest, [verbose])" \
	-s erlang halt -db_type $(DB_TYPE)

# The version of the open source schema that this version of the
# enterprise schema depends on.  Change this as new versions become
# available.
OSC_SCHEMA_VERSION=1.0.4

# The name of the database that will be created to run pgTAP tests
# against.
TEST_DB = opscode_chef_test

# Expression that captures all the SQL files that define test
# functions
TEST_FUNCTIONS = $(wildcard t/test_*.sql)

BASE_DIR = $(shell pwd)

# Sleep time to allow user to cancel sqitch operations
SLEEP_TIME = 5
all : setup_schema setup_tests test

clean:
	rm -Rf deps

# Installs dependencies locally; does not run migrations
install: deps/chef-server-schema

deps:
	mkdir deps

deps/chef-server-schema: deps
        # This can be an https URL when we open the open-source schema up
	cd deps; git clone https://github.com/opscode/chef-server-schema; cd chef-server-schema; git checkout $(OSC_SCHEMA_VERSION)

# Load up all schema changesets
setup_schema: install
	$(MAKE) -C deps/chef-server-schema setup_schema
	@echo "Deploying Enterprise Chef Server Schema on top..."
	@sqitch --engine pg --db-name $(TEST_DB) deploy --verify

# Load up all testing functions.  pgTAP libraries and open source
# schema test functions are loaded by the open source makefile target
setup_tests: install
	$(MAKE) -C deps/chef-server-schema setup_tests
	psql --dbname $(TEST_DB) \
	     --single-transaction \
	     --set ON_ERROR_STOP=1 \
	     --file t/monkey_patches.sql
	$(foreach file, $(TEST_FUNCTIONS), psql --dbname $(TEST_DB) --single-transaction --set ON_ERROR_STOP=1 --file $(file);)

test:
	@echo "Executing pgTAP tests in database '$(TEST_DB)'"
	@pg_prove --dbname $(TEST_DB) \
		  --verbose \
		  t/enterprise_chef_server_schema.pg

deploy:
	@if [ -z "$$EC_TARGET" ]; then \
		EC_TARGET="@`git describe`"; \
	fi; \
	if [ -z "$$OSC_TARGET" ]; then \
		OSC_TARGET="@$(OSC_SCHEMA_VERSION)"; \
	fi; \
	if [ -z "$$DB_USER" ]; then \
		read -p "Please enter the user to run as: " DB_USER; \
	fi; \
	echo "EC Target: $$EC_TARGET OSC Target: $$OSC_TARGET DB User: $$DB_USER"; \
	echo "Sleeping for $(SLEEP_TIME) seconds in case you want to cancel"; \
	sleep $(SLEEP_TIME); \
	if [ -z "$$DB_NAME" ]; then \
		sudo su -l $$DB_USER -c "cd $(BASE_DIR) && cd deps/chef-server-schema && sqitch deploy --to-target $$OSC_TARGET --verify  && cd ../.. && sqitch deploy --to-target $$EC_TARGET --verify"; \
	else \
		sudo su -l $$DB_USER -c "cd $(BASE_DIR) && cd deps/chef-server-schema && sqitch --db-name $$DB_NAME deploy --to-target $$OSC_TARGET --verify  && cd ../.. && sqitch --db-name $$DB_NAME deploy --to-target $$EC_TARGET --verify"; \
	fi

revert:
	@if [ -z "$$EC_TARGET" ]; then \
		read -p "Please enter the enterprise changeset: " EC_TARGET; \
	fi; \
	if [ -z "$$OSC_TARGET" ]; then \
		read -p "Please enter the osc changeset: " OSC_TARGET; \
	fi; \
	if [ -z "$$DB_USER" ]; then \
		read -p "Please enter the user to run as: " DB_USER; \
	fi; \
	echo "EC Target: $$EC_TARGET OSC Target: $$OSC_TARGET DB User: $$DB_USER"; \
	echo "Sleeping for $(SLEEP_TIME) seconds in case you want to cancel"; \
	sleep $(SLEEP_TIME); \
	if [ -z "$$DB_NAME" ]; then \
		sudo su -l $$DB_USER -c "cd $(BASE_DIR) && sqitch revert --to-target $$EC_TARGET -y && cd deps/chef-server-schema && sqitch revert --to-target $$OSC_TARGET -y && cd ../.."; \
	else \
		sudo su -l $$DB_USER -c "cd $(BASE_DIR) && sqitch --db-name $$DB_NAME revert --to-target $$EC_TARGET -y && cd deps/chef-server-schema && sqitch --db-name $$DB_NAME revert --to-target $$OSC_TARGET -y && cd ../.."; \
	fi

.PHONY: all clean install setup_schema setup_tests test deploy revert

TEST_DB = opscode_chef_test

all : setup_schema setup_tests test

TEST_FUNCTIONS = $(wildcard t/test_*.sql)

setup_schema:
	@echo "Dropping and recreating database '$(TEST_DB)'"
	@psql --dbname template1 --command 'DROP DATABASE IF EXISTS $(TEST_DB)'
	@createdb $(TEST_DB)
	@sqitch --engine pg --db-name $(TEST_DB) deploy

setup_tests:
	@psql --dbname $(TEST_DB) --command 'CREATE EXTENSION pgtap;'
	@psql --dbname $(TEST_DB) --command 'CREATE EXTENSION chef_pgtap;'
	$(foreach file, $(TEST_FUNCTIONS), psql --dbname $(TEST_DB) --single-transaction --set ON_ERROR_STOP=1 --file $(file);)

test:
	@echo "Executing pgTAP tests in database '$(TEST_DB)'"
	@pg_prove --dbname $(TEST_DB) --verbose t/chef_server_schema.pg

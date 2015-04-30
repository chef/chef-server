TEST_DB = bifrost_test

.PHONY : test

all : test

setup:
	@echo "Dropping and recreating database '$(TEST_DB)'"
	@psql --dbname template1 --single-transaction --command 'DROP DATABASE IF EXISTS $(TEST_DB)'
	@psql --dbname template1 --single-transaction --set ON_ERROR_STOP=1 --file sql/drop_roles.sql
	@createdb $(TEST_DB)
	@psql --dbname $(TEST_DB) --command 'CREATE EXTENSION pgtap;'
	@psql --dbname $(TEST_DB) --single-transaction --set ON_ERROR_STOP=1 --file t/custom_test_functions.sql
	@sqitch --engine pg --db-name $(TEST_DB) deploy
	@psql --dbname $(TEST_DB) --single-transaction --set ON_ERROR_STOP=1 --file sql/create_roles.sql
	@psql --dbname $(TEST_DB) --single-transaction --set ON_ERROR_STOP=1 --set database_name=$(TEST_DB) --file sql/permissions.sql

test: setup
	@echo "Executing pgTAP tests in database '$(TEST_DB)'"
	@pg_prove --dbname $(TEST_DB) --verbose --recurse

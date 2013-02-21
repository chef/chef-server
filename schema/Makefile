TEST_DB = heimdall_test

.PHONY : test

all : test

setup:
	@echo "Dropping and recreating database '$(TEST_DB)'"
	@dropdb $(TEST_DB)
	@createdb $(TEST_DB)
	@psql --dbname $(TEST_DB) --quiet --command 'CREATE EXTENSION pgtap;'
	@psql --dbname $(TEST_DB) --quiet --single-transaction --set ON_ERROR_STOP=1 --file sql/heimdall.sql

test: setup
	@echo "Executing pgTAP tests in database '$(TEST_DB)'"
	@pg_prove --dbname $(TEST_DB) --verbose --recurse

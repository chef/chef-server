install:
	bundle install
	psql -c 'create database oc_id_test;' -U postgres

travis:
	bundle exec rake travis:ci

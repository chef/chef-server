install:
	bundle install --without development --deployment
	psql -c 'create database oc_id_test;' -U postgres

travis: ci
	
ci:
	bundle exec rake bk:ci

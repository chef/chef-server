install:
	bundle config set --local without development
    bundle config set force_ruby_platform true
	bundle install --deployment
	psql -c 'create database oc_id_test;' -U postgres

travis: ci
	
ci:
	bundle exec rake bk:ci

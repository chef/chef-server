# TODO - looks like we can get built in support for rvm
# with versions availalbe, and not have to manage it oursleves?
install:
	bundle install
	rvm install --binary 2.1.4
	rvm use 2.1.4
	psql -c 'create database oc_id_test;' -U postgres

travis:
	bundle exec rake travis:ci

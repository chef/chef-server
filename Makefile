all : rel

deps/covertool deps/cowboy deps/erlsom deps/iso8601 :
	@rebar get-deps

deps : deps/covertool deps/cowboy deps/erlsom deps/iso8601

compile : deps
	@rebar compile

rel/bookshelf :
	@rebar generate

rel : compile rel/bookshelf

clean :
	@rebar skip_deps=true clean
	@rm -rf rel/bookshelf

distclean : clean
	@git clean -fdx
	@rm -rf .python

test : eunit nosetests

eunit : compile
	@rebar skip_deps=true eunit

nosetests : .python/bin/s3tests-generate-objects
	@S3TEST_CONF=etc/nosetests.conf .python/bin/nosetests --with-xunit

.python/bin/s3tests-generate-objects : .python/bin/nosetests
	@.python/bin/pip install .python/s3-tests

.python/bin/nosetests : .python/s3-tests
	@.python/bin/pip install -r .python/s3-tests/requirements.txt

.python/s3-tests : .python/bin/activate
	@git clone https://github.com/opscode/s3-tests.git .python/s3-tests

.python/bin/activate :
	@virtualenv `pwd`/.python

start :
	@rel/bookshelf/bin/bookshelf start

console :
	@rel/bookshelf/bin/bookshelf console

stop :
	@rel/bookshelf/bin/bookshelf stop ; exit 0

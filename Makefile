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

test : eunit nosetests

eunit : deps
	@rebar skip_deps=true eunit

nosetests : bin/s3tests-generate-objects
	@S3TEST_CONF=etc/nosetests.conf bin/nosetests --with-xunit

bin/s3tests-generate-objects : bin/nosetests
	@cd s3-tests && ../bin/pip install . && cd ..

bin/nosetests : s3-tests
	@bin/pip install -r s3-tests/requirements.txt

s3-tests : bin/activate
	@git clone https://github.com/opscode/s3-tests.git
	@pushd s3-tests
	@bin/pip install -r requirements.txt
	@bin/pip install .
	@popd

bin/activate :
	@virtualenv -p ${PYTHON:-python} .

start :
	@rel/bookshelf/bin/bookshelf start

console :
	@rel/bookshelf/bin/bookshelf console

stop :
	@rel/bookshelf/bin/bookshelf stop

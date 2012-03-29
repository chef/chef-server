PYTHON=python

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
test : unit integration
unit : compile
	@rebar skip_deps=true eunit
integration : s3-tests
	@cd s3-tests &&	\
	export S3TEST_CONF=`pwd`/../etc/nosetests.conf && \
	./virtualenv/bin/nosetests --with-xunit
s3-tests : s3-tests/.git s3-tests/virtualenv
s3-tests/virtualenv : s3-tests/virtualenv/bin/s3tests-generate-objects
s3-tests/virtualenv/bin/s3tests-generate-objects : s3-tests/virtualenv/bin/nosetests
	@cd s3-tests && \
	./virtualenv/bin/python setup.py develop --allow-hosts None
s3-tests/virtualenv/bin/nosetests : s3-tests/virtualenv/bin/activate
	@cd s3-tests && \
	./virtualenv/bin/pip install -r requirements.txt
s3-tests/virtualenv/bin/activate : s3-tests/.git
	@cd s3-tests && \
	virtualenv --python ${PYTHON} virtualenv
s3-tests/.git :
	@git clone https://github.com/opscode/s3-tests.git
start :
	@rel/bookshelf/bin/bookshelf start
console :
	@rel/bookshelf/bin/bookshelf console
ping :
	@rel/bookshelf/bin/bookshelf ping
stop :
	@rel/bookshelf/bin/bookshelf stop ; exit 0

export PATH=/opt/asdf/shims:/opt/asdf/bin:/opt/ci-studio-common/bin:/go/bin:/go/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/opt/chefdk/bin:/opt/asdf/installs/ruby/2.5.1/bin
export PERL5LIB=~/perl5/lib/perl5/x86_64-linux-gnu-thread-multi:~/perl5/lib/perl5:/etc/perl:/usr/local/lib/perl/5.14.2:/usr/local/share/perl/5.14.2:/usr/lib/perl5:/usr/share/perl5:/usr/lib/perl/5.14:/usr/share/perl/5.14:/usr/local/lib/site_perl
export LUALIB=~/.luarocks/lib/lua/5.2
export PGPORT=5432
export PGUSER=postgres
export ELIXIR_VERSION=1.4
export ERLANG_VERSION=18.3
export USE_SYSTEM_GECODE=1
export CHEFDK_GECODE_PATH=/opt/chefdk/embedded/lib/ruby/gems/2.5.0/gems/dep-selector-libgecode-1.3.1/lib/dep-selector-libgecode/vendored-gecode
export LIBRARY_PATH=$CHEFDK_GECODE_PATH/lib 
export LD_LIBRARY_PATH=$CHEFDK_GECODE_PATH/lib 
export CPLUS_INCLUDE_PATH=$CHEFDK_GECODE_PATH/include

apt-get --purge remove -y postgresql-9.3
apt-get update -y && apt-get install -y lua5.1 luarocks postgresql-9.6
cp /workdir/scripts/bk_tests/pb_hba.conf /etc/postgresql/9.6/main/pg_hba.conf
asdf global erlang 18.3
gem install bundler --version '1.17.3' --no-document
luarocks install --local lpeg
luarocks install --local lua-cjson
eval $(luarocks path)
cpanm --notest --quiet --local-lib=$HOME/perl5 local::lib && eval $(perl -I ~/perl5/lib/perl5/ -Mlocal::lib)
cpanm --notest --quiet App::Sqitch
service postgresql restart

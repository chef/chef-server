apt-get --purge remove -y postgresql-9.3
apt-get update -y && apt-get install -y lua5.1 luarocks postgresql-9.6
cp /workdir/scripts/bk_tests/pb_hba.conf /etc/postgresql/9.6/main/pg_hba.conf
# this is needed until the erlang version is installed in the docker container
# then it can be removed
asdf install erlang 21.1
asdf local erlang 21.1
erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell
gem install bundler --version '~> 1.17' --no-document
export LUALIB=~/.luarocks/lib/lua/5.2
luarocks install --local lpeg
luarocks install --local lua-cjson
eval $(luarocks path)
cpanm --notest --quiet --local-lib=$HOME/perl5 local::lib && eval $(perl -I ~/perl5/lib/perl5/ -Mlocal::lib)
cpanm --notest --quiet App::Sqitch
env
service postgresql restart

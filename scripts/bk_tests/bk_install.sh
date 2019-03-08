apt-get --purge remove -y postgresql-9.3
apt-get update -y && apt-get install -y lua5.1 luarocks postgresql-9.6
cp /workdir/scripts/bk_tests/pb_hba.conf /etc/postgresql/9.6/main/pg_hba.conf
asdf global erlang 18.3
gem install bundler --version '1.17.3' --no-document
gem uninstall bundler -v '>= 2.0.1' -a  # we uninstall bundler 2.0.1 as asdf defaults to it
export LUALIB=~/.luarocks/lib/lua/5.2
luarocks install --local lpeg
luarocks install --local lua-cjson
eval $(luarocks path)
cpanm --notest --quiet --local-lib=$HOME/perl5 local::lib && eval $(perl -I ~/perl5/lib/perl5/ -Mlocal::lib)
cpanm --notest --quiet App::Sqitch
service postgresql restart

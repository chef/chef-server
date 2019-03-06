apt-get update -y && apt-get install -y lua5.1 luarocks
gem install bundler --version '1.17.3' --no-document
luarocks install --local lpeg
luarocks install --local lua-cjson
eval $(luarocks path)
cpanm --notest --quiet --local-lib=$HOME/perl5 local::lib && eval $(perl -I ~/perl5/lib/perl5/ -Mlocal::lib)
cpanm --notest --quiet App::Sqitch
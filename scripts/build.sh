#!/usr/bin/env bash
export COMPONENT_NAME=`basename $1`

case $COMPONENT_NAME in
  oc_erchef)
    echo  "Setting env for oc_erchef"
    # TODO - can we just define this as $COMPONENT/travis-env.sh, and source it?
    # Perl for local sqitch
    export PERL5LIB=~/perl5/lib/perl5/x86_64-linux-gnu-thread-multi:~/perl5/lib/perl5:/etc/perl:/usr/local/lib/perl/5.14.2:/usr/local/share/perl/5.14.2:/usr/lib/perl5:/usr/share/perl5:/usr/lib/perl/5.14:/usr/share/perl/5.14:/usr/local/lib/site_perl
    eval $(perl -I ~/perl5/lib/perl5/ -Mlocal::lib)
    # Setup for gecode dependency build
    export USE_SYSTEM_GECODE=1
    export LIBRARY_PATH=$TRAVIS_BUILD_DIR/src/oc_erchef/vendored-gecode/lib
    export LD_LIBRARY_PATH=$TRAVIS_BUILD_DIR/src/oc_erchef/vendored-gecode/lib
    export CPLUS_INCLUDE_PATH=$TRAVIS_BUILD_DIR/src/oc_erchef/vendored-gecode/include
    ;;
  omnibus)
    export USE_OMNIBUS_FILES=0
    export LUALIB=~/.luarocks/lib/lua/5.2
	  eval $(luarocks path)
    #
	  source $(rvm 2.1.4 do rvm env --path)
    bundle install
    ;;
  oc-id)
	  source $(rvm 2.1.4 do rvm env --path)
    bundle install
    ;;
  chef-mover)
    ;;
esac
cd $TRAVIS_BUILD_DIR/$1 && make travis

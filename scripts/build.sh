#!/usr/bin/env bash
case $1 in
  src/oc_erchef)
    # TODO - can we just define this as $COMPONENT/travis-env.sh, and source it?
    # Perl for local sqitch
    export PERL5LIB=~/perl5/lib/perl5/x86_64-linux-gnu-thread-multi:~/perl5/lib/perl5:/etc/perl:/usr/local/lib/perl/5.14.2:/usr/local/share/perl/5.14.2:/usr/lib/perl5:/usr/share/perl5:/usr/lib/perl/5.14:/usr/share/perl/5.14:/usr/local/lib/site_perl
    eval $(perl -I ~/perl5/lib/perl5/ -Mlocal::lib)
    # Setup for gecode build
    export USE_SYSTEM_GECODE=1
    export LIBRARY_PATH=vendored-gecode/lib
    export LD_LIBRARY_PATH=vendored-gecode/lib
    export CPLUS_INCLUDE_PATH=vendored-gecode/include
    ;;
  omnibus)
    export USE_OMNIBUS_FILES=0
    export LUALIB=~/.luarocks/lib/lua/5.2
	  eval $(luarocks path)
    ;;
  # Note: src/oc-id and src/chef-mover have no special
  # environment requirements.
esac

cd $TRAVIS_BUILD_DIR/$1 && make travis

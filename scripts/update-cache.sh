#!/usr/bin/env bash
export CACHE_NAME=`basename $1`
source $TRAVIS_BUILD_DIR/scripts/s3cache.sh
case $CACHE_NAME in
  oc_erchef)
    cachelist=$(cat <<EOF
$HOME/.cpanm/
$HOME/.cpan/
$HOME/perl5/
$TRAVIS_BUILD_DIR/$1/travis-erlang-$TRAVIS_OTP_VERSION.plt
$TRAVIS_BUILD_DIR/$1/deps.plt
$TRAVIS_BUILD_DIR/$1/apps/chef_objects/priv/depselector_rb/.bundle
EOF
)
    ;;
  oc-id)
    cachelist=$(cat <<EOF
$HOME/.rvm
$TRAVIS_BUILD_DIR/$1/vendor/bundle
EOF
)
    ;;
  chef-mover)
    cachelist=''
    ;;
  omnibus)
    cachelist='.luarocks/ build/chef/chef-server/omnibus/.bundle/'
    ;;
esac

for entry in $cachelist; do
  # HEre we will force everything to be relative to $HOME
  # and sync it up with our original cache files
  # TODO can we just make a tar directly, not worrying about
  # rsync?
  rsync -azr --delete  ${entry#$HOME} $HOME/.realcache/cacheroot/
done

cd $CACHE_DIR
tar czf $CACHE_FILE *

original=`md5sum $OLD_CACHE_FILE`
new=`md5sum $CACHE_FILE`

if [ "$orginal" == "$new" ]; then
  echo "No changes in cache detected, not uploading."
else
  s3cache_put
fi


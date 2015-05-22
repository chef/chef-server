#!/usr/bin/env bash
export CACHE_NAME=`basename $1`
source $TRAVIS_BUILD_DIR/scripts/s3cache.sh
case $CACHE_NAME in
  oc_erchef)
    # Prevent build log from changing cache cand causing repackage
    rm -f $HOME/.cpanm/work/*/build.log
    rm -f $HOME/.cpanm/build.log
    cachelist=$(cat <<EOF
$HOME/.cpanm
$HOME/perl5
$TRAVIS_BUILD_DIR/$1/travis-erlang-$TRAVIS_OTP_RELEASE.plt
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
    cachelist=$(cat <<EOF
$HOME/.rvm
EOF
)
    ;;
  omnibus)
    cachelist=$(cat <<EOF
$HOME/.rvm
$HOME/.luarocks
$TRAVIS_BUILD_DIR/$1/.bundle
EOF
)
    ;;
esac

cd $HOME
for entry in $cachelist; do
  # Here we will force everything to be relative to $HOME
  # and sync it up with our original cache files
  rsync -azrR --delete  ${entry#$HOME/} $HOME/.realcache/cacheroot
done

cd $CACHE_DIR
tar czf $NEW_CACHE_FILE .
# TODO I think updated timestamp but no content changes are causing different
#      sums every time.  There's probably a better way to handle this.
echo "Comparing $OLD_CACHE_FILE to $NEW_CACHE_FILE"
original=`md5sum $OLD_CACHE_FILE`
new=`md5sum $NEW_CACHE_FILE`

if [ "$orginal" == "$new" ]; then
  echo "No changes in cache detected, not uploading."
else
  echo "Uploading changed cache"
  s3cache_put
fi


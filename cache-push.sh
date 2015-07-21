#!/usr/bin/env bash

regex=".*transferred: 0"
while read f; do
  results=`rsync -azrR --delete --stats $HOME/$f $HOME/.realcache | grep "files transferred: 0"`
  if [ "$results" == "" ]
  then
    echo "Update to cache due to change in $HOME/$f"
    touch $HOME/.realcache # Ensure timestamp is updated so cacher knows we've changed.
  fi
done < $HOME/build/chef/chef-server/.cache-files

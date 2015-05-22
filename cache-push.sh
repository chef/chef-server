#!/usr/bin/env bash
while read f; do
  rsync -azrR --delete --stats $HOME/$f $HOME/.realcache
done < $HOME/build/chef/chef-server/.cache-files

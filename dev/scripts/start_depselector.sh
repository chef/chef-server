#!/bin/bash
#
# Start depselector in the forground in order to see any error
# messages it is producing.  This is useful in some debugging
# situations.
#
# The erlang runtime would open these FDs for us. If we don't open
# them they get reserved by the ruby VM and then application fails to
# start.
ERLOUT=$(mktemp)
echo "Depselector erlang port responses sent to $ERLOUT"
exec 3<&0
exec 4>"$ERLOUT"

export PATH=/opt/opscode/embedded/bin:$PATH

ruby /opt/opscode/embedded/service/opscode-erchef/lib/chef_objects-*/priv/depselector_rb/depselector.rb

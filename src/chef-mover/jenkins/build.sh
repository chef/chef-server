#!/bin/bash -xe

# Build the project. If building a release tag, create a tarball and upload to
# s3. To customize for another project, you should only need to edit
# PROJ_NAME and the make command.

PROJ_NAME=mover

export PATH=/opt/ruby1.9/bin:/opt/erlang/bin:$PATH:/usr/local/bin
export USE_SYSTEM_GECODE=1

make distclean
git clean -fdX
./rebar3 update
./rebar3 do clean -a, compile, release || exit 1

if git describe --tags --match='[0-9]*.[0-9]*.[0-9]*' --exact-match
then
    VERSION=$(git describe --tags --exact-match --match='[0-9]*.[0-9]*.[0-9]*')
else
    VERSION=`cat _build/default/rel/${PROJ_NAME}/releases/RELEASES|grep '{release,' |cut -d ',' -f 3|sed 's/"//g'|sed 's/}//g'`
fi
PACKAGE=${PROJ_NAME}-${VERSION}.tar.gz
cd _build/default/rel
tar zcf $PACKAGE $PROJ_NAME/
s3cmd put $PACKAGE s3://opscode-ci/artifacts/ubuntu-10.04/x86_64/chef-mover/$PACKAGE
s3cmd put $PACKAGE s3://opscode-ci/artifacts/ubuntu-12.04/x86_64/chef-mover/$PACKAGE

#!/bin/bash -xe

# Build oc_bifrost. If building a release tag, create a tarball and upload to
# s3. To customize for another project, you should only need to edit
# PROJ_NAME and the make command.

PROJ_NAME=oc_bifrost

export PATH=/opt/erlang/bin:$PATH:/usr/local/bin
jenkins/builder_info.rb
source machine_info

make distclean rel || exit 1

# If the string is null, then the git command returned false
if git describe --tags --match='[0-9]*.[0-9]*.[0-9]*' --exact-match
then
    VERSION=$(git describe --tags --exact-match --match='[0-9]*.[0-9]*.[0-9]*')
    PACKAGE=${PROJ_NAME_}${VERSION}.tar.gz
else
    REL_VERSION=`cat relx.config|grep '{release,' |cut -d ',' -f 3|sed 's/"//g'|sed 's/}//g'`
    GIT_SHA=`git rev-parse --short HEAD`
    VERSION=${REL_VERSION}-${GIT_SHA}
    PACKAGE=${PROJ_NAME_}${VERSION}.tar.gz
fi

cd _rel

# Yep, hard-coding platform and version. We're building ubuntu only, 10.04 only,
# but need to distribute to both.
tar zcf $PACKAGE $PROJ_NAME/
s3cmd put $PACKAGE s3://opscode-ci/artifacts/ubuntu-10.04/x86_64/$PROJ_NAME/$PACKAGE
s3cmd put $PACKAGE s3://opscode-ci/artifacts/ubuntu-12.04/x86_64/$PROJ_NAME/$PACKAGE

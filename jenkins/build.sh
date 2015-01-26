#!/bin/bash -xe

# Build oc_erchef. If building a release tag, create a tarball and upload to
# s3. To customize for another project, you should only need to edit
# PROJ_NAME and the make command.

PROJ_NAME=oc_erchef

export PATH=/opt/ruby1.9/bin:$PATH:/usr/local/bin
jenkins/builder_info.rb
source machine_info
ARTIFACT_BASE=opscode-ci/artifacts/$os/$machine/$PROJ_NAME

make distclean rel || exit 1

# If the string is null, then the git command returned false
if git describe --tags --match='[0-9]*.[0-9]*.[0-9]*' --exact-match
then
    VERSION=$(git describe --tags --exact-match --match='[0-9]*.[0-9]*.[0-9]*')
    PACKAGE=${PROJ_NAME}-${VERSION}.tar.gz
    cd rel
    tar zcf $PACKAGE $PROJ_NAME/
    s3cmd put $PACKAGE s3://$ARTIFACT_BASE/$PACKAGE
else
    REL_VERSION=`cat rel/reltool.config|grep '{rel,.*"oc_erchef"'|cut -d ',' -f 3|sed 's/"//g'`
    GIT_SHA=`git rev-parse --short HEAD`
    VERSION=${REL_VERSION}-${GIT_SHA}
    PACKAGE=${PROJ_NAME}-${VERSION}.tar.gz
    cd rel
    tar zcf $PACKAGE $PROJ_NAME/
    s3cmd put $PACKAGE s3://$ARTIFACT_BASE/$PACKAGE
fi

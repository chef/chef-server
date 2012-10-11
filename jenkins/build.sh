#!/bin/bash -xe

# Build oc_erchef. If building a release tag, create a tarball and upload to
# s3. To customize for another project, you should only need to edit
# PROJ_NAME and the make command.

PROJ_NAME=oc_erchef

export PATH=$PATH:/usr/local/bin
ARTIFACT_BASE=opscode-ci/artifacts/$PROJ_NAME

make distclean rel || exit 1

BUCKET=oc-erchef-artifacts

# If the string is null, then the git command returned false
if git describe --tags --match='*-[0-9]*.[0-9]*.[0-9]*' --exact-match
then
    VERSION=$(git describe --tags --exact-match --match='*-[0-9]*.[0-9].*[0-9]*')
    PACKAGE=$PROJ_NAME_$VERSION.tar.gz
    cd rel
    tar zcvf $PACKAGE $PROJ_NAME/
    s3cmd put --progress $PACKAGE s3://$ARTIFACT_BASE/$PACKAGE
else
    echo "There was no exact match for release tags found, so artifacts are not being uploaded."
fi

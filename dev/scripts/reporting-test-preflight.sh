#!/bin/bash
#
# Copyright 2015-2018 Chef Software, Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

REQUIRED_VARS_ERROR=""
DIR=`pwd`

if [ -z "$CHEF_SERVER_LATEST" ];
then
    REQUIRED_VARS_ERROR="${REQUIRED_VARS_ERROR}CHEF_SERVER_LATEST unset and is required.\n"
elif [ ! -f $CHEF_SERVER_LATEST ];
then
    echo "\$CHEF_SERVER_LATEST was set to $CHEF_SERVER_LATEST but a file by that name does not exist in $DIR"
    exit 1
fi

if [ -z "$CHEF_SERVER_LATEST_MINUS_1" ];
then
    REQUIRED_VARS_ERROR="${REQUIRED_VARS_ERROR}CHEF_SERVER_LATEST_MINUS_1 unset and is required.\n"
elif [ ! -f $CHEF_SERVER_LATEST_MINUS_1 ];
then
    echo "\$CHEF_SERVER_LATEST_MINUS_1 was set to $CHEF_SERVER_LATEST_MINUS_1 but a file by that name does not exist in $DIR"
    exit 1
fi

if [ -z "$ENTERPRISE_CHEF_SERVER_11" ];
then
    REQUIRED_VARS_ERROR="${REQUIRED_VARS_ERROR}ENTERPRISE_CHEF_SERVER_11 unset and is required.\n"
elif [ ! -f $ENTERPRISE_CHEF_SERVER_11 ];
then
    echo "\$ENTERPRISE_CHEF_SERVER_11 was set to $ENTERPRISE_CHEF_SERVER_11 but a file by that name does not exist in $DIR"
    exit 1
fi

if [ -z "$REPORTING_LAST_RELEASE" ];
then
    REQUIRED_VARS_ERROR="${REQUIRED_VARS_ERROR}REPORTING_LAST_RELEASE unset and is required.\n"
elif [ ! -f $REPORTING_LAST_RELEASE ];
then
    echo "\$REPORTING_LAST_RELEASE was set to $REPORTING_LAST_RELEASE but a file by that name does not exist in $DIR"
    exit 1
fi

if [ -z "$REPORTING_LATEST" ];
then
    REQUIRED_VARS_ERROR="${REQUIRED_VARS_ERROR}REPORTING_LATEST unset and is required."
elif [ ! -f $REPORTING_LATEST ];
then
    echo "\$REPORTING_LATEST was set to $REPORTING_LATEST but a file by that name does not exist in $DIR"
    exit 1
fi

if [ -n "$REQUIRED_VARS_ERROR" ];
then
    echo -e $REQUIRED_VARS_ERROR
    exit 1;
fi

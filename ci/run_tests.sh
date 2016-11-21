#!/bin/sh
set -evx

chef-server-ctl test -J $WORKSPACE/pedant.xml --all

#!/bin/sh
#   Author: Bryan McLellan <btm@loftninjas.org>
#   Copyright 2010 Opscode, Inc.
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.

# Detects stale location constraints left by a resource move without a matching unmove

CRM=/usr/sbin/crm
GREP=/bin/grep

OK=0
WARNING=1
CRITICAL=2
UNKNOWN=3

fail=`$CRM configure show 2>&1 | $GREP failed`
fail_exit=$?
 
# Typically a permissions issue, but the cluster may be down. 
# Other tools should monitor the cluster (see crm_mon -s)
if [ $fail_exit -eq 0 ]; then
  echo "UNKNOWN: $fail"
  exit $UNKNOWN
fi

success=`$CRM configure show | $GREP '^location cli-standby'`
success_exit=$?

if [ $success_exit -eq 0 ]; then
  echo "CRITICAL: $success"
  exit $CRITICAL
elif [ $success_exit -eq 1 ]; then
  echo "OK: No location constraints found"
  exit $OK
else
  echo "UNKNOWN: grep broke"
  exit $UNKNOWN
fi


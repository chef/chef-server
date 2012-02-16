#!/bin/sh

cd /srv/opscode-certificate/current

CUKE_OUTPUT=/tmp/nagios-cucumber.out
NAGIOS_OUTPUT=/var/lib/nagios3/rw/nagios.cmd

cucumber --format Opscode::Cucumber::Formatter::Nagios > $CUKE_OUTPUT
return_code=$?

# return_code 0 is OK; anything else is 2 -- CRITICAL.
if [ $return_code -eq 0 ]; then
    # nothing, leave it at 0
    true
else
    return_code=2
fi

# Do all this echo stuff and pipe it to cat, on the hopes that the file
# will get all the text at once -- to prevent race conditions with Nagios
# reading the file.
(/bin/echo -n "[`date +%s`] "
 /bin/echo -n 'PROCESS_SERVICE_CHECK_RESULT;'
 /bin/echo -n 'Certificate Service Cert Generation;'
 /bin/echo -n "$return_code;"
 cat $CUKE_OUTPUT | grep -v 'Using the default profile') | cat >> $NAGIOS_OUTPUT

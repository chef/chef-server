#!/usr/bin/perl -w
###
### Nagios plugin for CouchDB available Orgs monitoring
###
# 
# Author Nathan Haneysmith <nathan@opscode.com>
#

use strict;
use Getopt::Long;

my $ret = undef;
my $orgs = "0";

if (!eval("require LWP::UserAgent;")) {
	print "LWP::UserAgent module not installed\n";
  exit(3);
}

if (!eval("require JSON;")) {
	print "JSON module not installed\n";
  exit(3);
}

# --
# Print out the usage message
# --
sub usage {
    print "usage: check_opscode_avail_orgs.pl -H <host> \n";
    print "       Optional parameters:\n";
    print "       --port <port> \n";
    print "       --Co <critical number of orgs> \n";
    print "       --Wo <warning number of orgs> \n";
}

$|=1;

# --
# Parse arguments and read Configuration
# --
my ($host, $port, $criticalOrgs, $warningOrgs);
GetOptions (
    'host=s' => \$host,
    'H=s' => \$host,
    'port=i' => \$port,
    'Co=i' => \$criticalOrgs,
    'Wo=i' => \$warningOrgs
);

if (!$host ) {
    usage();
    exit(1);
}

if (!$port) {
    $port = "5984";
}

my $totalTime = time();

my $URI = "http://$host:$port/opscode_account_internal/_design/Mixlib%3A%3AAuthorization%3A%3AModels%3A%3AOrganizationInternal-48acbb3cdff0332031068da2ff17175b/_view/by_state_count?key=%22unassigned%22";

my $response = LWP::UserAgent->new(timeout => 5)->request(HTTP::Request->new('GET', $URI));

if ($response->is_success) {
  my $json = JSON::decode_json($response->content);
  foreach  my $row(@{$json->{rows}}){
    $orgs = $row->{value};
  }
} else {
	print "UNKNOWN: Connect failed: " . $response->status_line . "\n";
        exit 3;
}

my $statusString = "$orgs avail orgs";
if ($criticalOrgs && $criticalOrgs > $orgs) {
  print "CRITICAL: Number of available orgs less than $criticalOrgs ($statusString)\n";
  exit 2;
}
if ($warningOrgs && $warningOrgs > $orgs) {
  print "WARNING: Number of available orgs less than $warningOrgs ($statusString)\n";
  exit 1;
}

print "OK: $statusString\n";
exit 0;


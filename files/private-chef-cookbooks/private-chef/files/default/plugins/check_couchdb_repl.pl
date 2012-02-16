#!/usr/bin/perl -w

use strict;
use lib "/usr/lib/nagios/plugins";
use utils qw(%ERRORS);

my $USAGE = <<"EOH";
Usage: $0 couchdb couchdb_backup warn_threshold crit_threshold

Verify that two couchdbs are moving in lockstep

Example:
  $0 http://couchdbauthz.opscode.com:5986/authorization http://couchdb.opscode.com:5986/authorization_backup

EOH

if (@ARGV != 4) {
  print $USAGE;
  exit(1);
}

my ($couchdb_primary, $couchdb_backup, $warn_threshold, $crit_threshold) = @ARGV;

our $exit_level = undef;
my $now = time;

#
# Get the backup and primary update times
#
my $backup = get_current_update_time($couchdb_backup);
my $primary = get_last_update_time($couchdb_primary);
# If the backup is ahead of the primary (has caught up since the last time we looked),
# find out the latest update on the primary
if ($backup->{update_time} >= $primary->{update_time})
{
  $primary = get_current_update_time($couchdb_primary);
}

my $output = "$couchdb_primary was updated to seq # $primary->{update_seq} at $primary->{update_time}; $couchdb_backup was replicated to seq # $backup->{update_seq} at $backup->{update_time}.";

#
# If the backup has been older than the primary for more than <threshold> seconds, we have an issue.
#
if ($backup->{update_time} < $primary->{update_time})
{
  my $time_since_primary = $now - $primary->{update_time};
  if ($time_since_primary > $crit_threshold)
  {
    report_critical("$couchdb_backup has been older than $couchdb_primary for at least ${time_since_primary}s.  $output\n");
  }
  elsif ($time_since_primary > $warn_threshold)
  {
    report_warning("$couchdb_backup has been older than $couchdb_primary for at least ${time_since_primary}s.  $output\n");
  }
}

if ($exit_level)
{
  exit $ERRORS{$exit_level};
}

print "CHECK COUCHDB REPL OK: $output\n";
exit $ERRORS{'OK'};


sub get_current_update_time
{
  my ($couchdb_url) = @_;
  my $last_update = get_last_update_time($couchdb_url);
  my $new_update = { update_seq => -1, update_time => -1 };

  #
  # Get the current update sequence #
  #
  my $couch_output = `curl $couchdb_url 2>&1`;
  if ($couch_output !~ /"update_seq"\s*:\s*(\d+)/)
  {
    report_critical("$couchdb_url failed to return update_seq.  Output: $couch_output\n");
    return $new_update;
  }
  $new_update->{update_seq} = $1;

  #
  # Find out if the sequence number changed
  #
  if ($new_update->{update_seq} == $last_update->{update_seq} || $last_update->{update_seq} == -1)
  {
    $new_update->{update_time} = $last_update->{update_time};
    if ($new_update->{update_time} == -1)
    {
      report_warning("The Couch database at '$couchdb_url' has never changed since the monitoring software has started up, which is suspicious and could indicate that monitoring is pointing at a defunct database.\n");
    }
  }
  elsif ($new_update->{update_seq} > $last_update->{update_seq})
  {
    $new_update->{update_time} = $now;
  }
  else
  {
    my $filename = get_last_update_filename($couchdb_url);
    report_critical("Couch has opened a hole in time.  Last recorded update seq for '$couchdb_url' is LATER then the current one: previous seq is $last_update->{update_seq}, current seq is $new_update->{update_seq}.  Remove $filename (once you are satisfied the world is not exploding) to clear this error.\n");
    return $new_update;
  }

  #
  # Write the current sequence number and update time to the file so we can compare next time.
  #
  save_last_update_time($couchdb_url, $new_update);

  return $new_update;
}

sub get_last_update_filename
{
  my ($couchdb_url) = @_;
  my $tmpfile = "couchdb_repl_${couchdb_url}";
  $tmpfile =~ s/[^A-Za-z0-9]/_/g;
  $tmpfile = "/tmp/$tmpfile";
  return $tmpfile;
}

sub get_last_update_time
{
  my ($couchdb_url) = @_;
  my $filename = get_last_update_filename($couchdb_url);
  if (open(my $read_fh, $filename))
  {
    my $update_seq = readline($read_fh);
    my $update_time = readline($read_fh);
    chomp $update_seq;
    chomp $update_time;
    close($read_fh);
    return { update_seq => $update_seq, update_time => $update_time };
  }

  report_critical("$filename does not exist.  Age of $couchdb_url unknown.  This should correct itself on the next run; otherwise this monitor is broken.\n");
  return { update_seq => -1, update_time => -1 };
}

sub save_last_update_time
{
  my ($couchdb_url, $update_info) = @_;

  my $filename = get_last_update_filename($couchdb_url);
  if (open(my $write_fh, ">$filename"))
  {
    print $write_fh "$update_info->{update_seq}\n";
    print $write_fh "$update_info->{update_time}\n";
    close($write_fh);
  }
  else
  {
    report_critical("Could not write to $filename.");
  }
}

sub report_critical
{
  my ($msg) = @_;
  print "CHECK COUCHDB REPL CRITICAL: $msg";
  $exit_level = 'CRITICAL';
}

sub report_warning
{
  my ($msg) = @_;
  print "CHECK COUCHDB REPL WARNING: $msg";
  $exit_level = 'WARNING' if !defined($exit_level) || $exit_level ne 'CRITICAL';
}

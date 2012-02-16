#!/usr/bin/perl -w

####################################################
# check_drbd v0.5.3			           #
# by Brandon Lee Poyner    bpoyner / CCAC.edu	   #
####################################################

use strict;
use File::Basename;
use Getopt::Long;

my $drbd_proc='/proc/drbd';
my $drbd_devices=0;
my ($drbd_expect, $drbd_role, $drbd_version, $debug_mode); 
my (%options, %cs, %st, %ld, %ds, %check, %warning, %critical);

my $prog_name=basename($0);
my $prog_revision='0.5.3';

my %errorcodes = (
	'OK' => { 'retvalue' => 0 },
	'WARNING' => { 'retvalue' => 1 },
	'CRITICAL' => { 'retvalue' => 2 },
	'UNKNOWN' => { 'retvalue' => 3 }
);

# 
# Define various states and default alarm values
#
my %state = ( 
	      'Primary' => { 'value' => 'OK', 'type' => 'st' },
	      'Secondary' => { 'value' => 'OK', 'type' => 'st' },
	      'Unknown' => { 'value' => 'CRITICAL', 'type' => 'st' },
	      'StandAlone' => { 'value' => 'WARNING', 'type' => 'cs' },
	      'Unconnected' => { 'value' => 'CRITICAL', 'type' => 'cs' },
	      'Timeout' => { 'value' => 'CRITICAL', 'type' => 'cs' },
	      'BrokenPipe' => { 'value' => 'CRITICAL', 'type' => 'cs' },
	      'WFConnection' => { 'value' => 'CRITICAL', 'type' => 'cs' },
              'WFReportParams' => { 'value' => 'CRITICAL', 'type' => 'cs' },
	      'Connected' => { 'value' => 'OK', 'type' => 'cs' },
	      'Unconfigured' => { 'value' => 'CRITICAL', 'type' => 'cs' },
	      # DRBD 0.6
	      'SyncingAll' => { 'value' => 'WARNING', 'type' => 'cs' },
              'SyncingQuick' => { 'value' => 'WARNING', 'type' => 'cs' },
	      'SyncPaused' => { 'value' => 'CRITICAL', 'type' => 'cs' },
	      # DRBD 0.7
	      'WFBitMapS' => { 'value' => 'CRITICAL', 'type' => 'cs' },
	      'WFBitMapT' => { 'value' => 'CRITICAL', 'type' => 'cs' },
	      'SyncSource' => { 'value' => 'WARNING', 'type' => 'cs' }, 
              'SyncTarget' => { 'value' => 'WARNING', 'type' => 'cs' },
	      'PausedSyncS' => { 'value' => 'CRITICAL', 'type' => 'cs' },
	      'PausedSyncT' => { 'value' => 'CRITICAL', 'type' => 'cs' },
	      'NetworkFailure' => { 'value' => 'CRITICAL', 'type' => 'cs' },
	      'SkippedSyncS' => { 'value' => 'CRITICAL', 'type' => 'cs' },
	      'SkippedSyncT' => { 'value' => 'CRITICAL', 'type' => 'cs' },
	      'Consistent' => { 'value' => 'OK', 'type' => 'ld' }, 
	      'Inconsistent' => { 'value' => 'CRITICAL', 'type' => 'ld' },
              # DRBD 8.0
	      'UpToDate' => { 'value' => 'OK', 'type' => 'ds' },
	      'Consistent' => { 'value' => 'OK', 'type' => 'ds' },
	      'Negotiating' => { 'value' => 'WARNING', 'type' => 'ds' },
	      'Attaching' => { 'value' => 'WARNING', 'type' => 'ds' },
	      'Diskless' => { 'value' => 'CRITICAL', 'type' => 'ds' },
	      'Failed' => { 'value' => 'CRITICAL', 'type' => 'ds' },
	      'Outdated' => { 'value' => 'CRITICAL', 'type' => 'ds' },
	      'Inconsistent' => { 'value' => 'CRITICAL', 'type' => 'ds' },
	      'DUnknown' => { 'value' => 'CRITICAL', 'type' => 'ds' },
	      # DRBD 8.2
	      'VerifyS' => { 'value' => 'WARNING', 'type' => 'cs' },
	      'VerifyT' => { 'value' => 'WARNING', 'type' => 'cs' },
	      # DRBD 8.3
	      'Disconnecting' => { 'value' => 'WARNING', 'type' => 'cs' },
	      'ProtocolError' => { 'value' => 'CRITICAL', 'type' => 'cs' },
	      'TearDown' => { 'value' => 'WARNING', 'type' => 'cs' },
	      'StartingSyncS' => { 'value' => 'WARNING', 'type' => 'cs' },
	      'StartingSyncT' => { 'value' => 'WARNING', 'type' => 'cs' },
	      'WFSyncUUID' => { 'value' => 'WARNING', 'type' => 'cs' }
);

&parse_options;
&parse_proc;
&parse_drbd_devices;
&check_drbd_state;
&report_status;
&myexit('UNKNOWN',"$prog_name should never reach here");

sub print_usage {
	print <<EOF
Usage: $prog_name [-d <All|Configured|...>] [-e expect] [-p proc] [-r role] [-o states] [-w states] [-c states] [--debug]
	Options:
	-d STRING [default: $drbd_devices.  Example: 0,1,2 ]
	-p STRING [default: $drbd_proc.  Use '-' for stdin]
	-e STRING [Must be this connected state. Example: Connected]
	-r STRING [Must be this node state. Example: Primary]
	-o STRING [Change value to OK. Example: StandAlone]
	-w STRING [Change value to WARNING. Example: SyncingAll]
	-c STRING [Change value to CRITICAL. Example: Inconsistent,WFConnection]
EOF
}

sub print_revision {
	print <<EOF;
$prog_name $prog_revision

The nagios plugins come with ABSOLUTELY NO WARRANTY. You may redistribute
copies of the plugins under the terms of the GNU General Public License.
For more information about these matters, see the file named COPYING.
EOF

}

sub print_help {
	&print_revision;
	print "\n";
	&print_usage;
        print <<EOF;

Send email to nagios-users\@lists.sourceforge.net if you have questions
regarding use of this software. To submit patches or suggest improvements,
send email to bpoyner\@ccac.edu
EOF
	exit $errorcodes{'UNKNOWN'}->{'retvalue'};
}

sub parse_options {
	my ($help, $version, $debug, $ok_string, $warning_string, 
	    $critical_string); 
	#
	# Get command line options
	#
	GetOptions("h|help" => \$help,
		"V|version" => \$version,
		"d|device|devices=s" => \$drbd_devices,
		"e|expect=s" => \$drbd_expect,
		"p|proc=s" => \$drbd_proc,
		"r|role=s" => \$drbd_role,
		"o|ok=s" => \$ok_string,
		"w|warning=s" => \$warning_string,
		"c|critical=s" => \$critical_string,
		"debug" => \$debug);
	if (defined($help) && ($help ne "")) {
		&print_help;
		exit $errorcodes{'UNKNOWN'}->{'retvalue'};
	}
	if (defined($version) && ($version ne "")) {
		&print_revision;
		exit $errorcodes{'UNKNOWN'}->{'retvalue'};
	}
	if (defined($drbd_expect) && ($drbd_expect ne "")) {
		# User requested the connected state to be very specific
		&change_values($drbd_expect,'cs','expect','connected state');
	}
	if (defined($drbd_role) && ($drbd_role ne "")) {
		# User requested the node state to be very specific
		&change_values($drbd_role,'st','role','node state');
	}
	if (defined($ok_string) && ($ok_string ne "")) {
		# User requested certain values to be OK
		&set_values($ok_string,'OK');
	}
	if (defined($warning_string) && ($warning_string ne "")) {
		# User requested certain values to be WARNING
		&set_values($warning_string,'WARNING');
	}
	if (defined($critical_string) && ($critical_string ne "")) {
		# User requested certain values to be CRITICAL
		&set_values($critical_string,'CRITICAL');
	}
	if (defined($debug) && ($debug ne "")) {
		# 
		# Debugging information
		#
		$debug_mode=1;
		print STDERR "<$prog_name settings>\n";
		print STDERR "DRBD Devices: $drbd_devices\n";
		printf STDERR "DRBD Proc: %s\n", defined($drbd_proc)?$drbd_proc:"";
		printf STDERR "DRBD Expect: %s\n", defined($drbd_expect)?$drbd_expect:"";
		printf STDERR "DRBD Role: %s\n", defined($drbd_role)?$drbd_role:"";
		my (@ok, @critical, @warning);
		for my $key ( keys %state ) {
			if ($state{$key}->{'value'} eq 'OK') {
				push(@ok,$key);
			}
			if ($state{$key}->{'value'} eq 'WARNING') {
				push(@warning,$key);
			}
			if ($state{$key}->{'value'} eq 'CRITICAL') {
				push(@critical,$key);
			}
		}
		printf STDERR "DRBD OK: %s\n", join(" ",sort(@ok));
		printf STDERR "DRBD WARNING: %s\n", join(" ",sort(@warning));
		printf STDERR "DRBD CRITICAL: %s\n", join(" ",sort(@critical));
		print STDERR "</$prog_name settings>\n";
	}
}

sub parse_proc {
	#
	# Read in contents of proc file, feed results into hashes
	#
	my $input;
	if ( $drbd_proc ne "-" ) {
		$input = "DRBD";
		if ( ! -e $drbd_proc ) {
			&myexit('UNKNOWN',"No such file $drbd_proc");
		}
		open(DRBD, "$drbd_proc") || 
			&myexit('UNKNOWN',"Could not open $drbd_proc");
	} else {
		$input = "STDIN";
	}
	while(<$input>) {
		if (/^version: (\d+).(\d+)/) {
			$drbd_version = "$1.$2";
		}
		if (/^\s?(\d+):.* cs:(\w+)/) {
			$cs{$1} = $2;
		}
		if (/^\s?(\d+):.* st:(\w+)\//) {
			$st{$1} = $2;
		}
		if (/^\s?(\d+):.* ld:(\w+)/) {
			$ld{$1} = $2;
		}
		if (/^\s?(\d+):.* ds:(\w+)/) {
			$ds{$1} = $2;
		}
	}
	if ( $drbd_proc ne "-" ) {
		close(DRBD);
	}
	if (defined($debug_mode) && ($debug_mode == 1)) {
		# 
		# Debugging information
		#
		print STDERR "<$prog_name devices found>\n";
		for my $key ( sort keys %cs ) {
			printf STDERR "Found Device $key $cs{$key}%s%s%s\n", defined($st{$key})?" $st{$key}":"", defined($ld{$key})?" $ld{$key}":"", defined($ds{$key})?" $ds{$key}":"";
		}
		print STDERR "</$prog_name devices found>\n";
	}
}

sub parse_drbd_devices {
	#
	# Determine which DRBD devices to monitor
	#
	my @devices;
	if ($drbd_devices =~ /^all$/i) {
		for my $device ( keys %cs ) {
			push(@devices,$device);
		}
	} elsif ($drbd_devices =~ /^configured$/i) {
		for my $device ( keys %cs ) {
			next if ($cs{$device} eq "Unconfigured");
			push(@devices,$device);
		}
	} else {
		@devices = split(/,/,$drbd_devices);
	}
	foreach my $device (@devices) {
		if (!(defined($cs{$device}))) {
			&myexit('UNKNOWN',"Could not find device $device");
		}
		$check{$device} = 1;
	}
	if (int(keys %check) == 0) {
		&myexit('UNKNOWN',"No configured devices found");
	}
	if (defined($debug_mode) && ($debug_mode == 1)) {
		# 
		# Debugging information
		#
		print STDERR "<$prog_name devices to check>\n";
		for my $key ( sort keys %check ) {
			printf STDERR "Checking enabled for device $key\n";
		}
		print STDERR "</$prog_name devices to check>\n";
	}
}

sub check_drbd_state {
	for my $drbd_device ( sort keys %check ) {
		if ((defined($drbd_version)) && ($drbd_version >= '8.0')) {
			#
			# We're dealing with version 8.0 or greater 
			# Set data state
			#
			if ((defined($ds{$drbd_device})) &&
			    (defined($state{$ds{$drbd_device}}))) {
				$state{$ds{$drbd_device}}->{$drbd_device}->{'level'} = 1;
			} elsif (defined($ds{$drbd_device})) {
				&myexit('CRITICAL',"Data state unknown value '$ds{$drbd_device}' for device $drbd_device");
			}
		}
		if ((defined($drbd_version)) && ($drbd_version == '0.7')) {
			#
			# We're dealing with version 0.7 
			# Set local data consistency
			#
			if ((defined($ld{$drbd_device})) &&
			    (defined($state{$ld{$drbd_device}}))) {
				$state{$ld{$drbd_device}}->{$drbd_device}->{'level'} = 1;
			} elsif (defined($ld{$drbd_device})) {
				&myexit('CRITICAL',"Local data consistency unknown value '$ld{$drbd_device}' for device $drbd_device");
			}
		}
		#
		# Check for a state value (Primary, Secondary, etc)
		#
		if ((defined($st{$drbd_device})) &&
		    (defined($state{$st{$drbd_device}}))) {
			$state{$st{$drbd_device}}->{$drbd_device}->{'level'} = 1;
		} elsif (defined($st{$drbd_device})) {
			&myexit('CRITICAL',"Node state unknown value '$st{$drbd_device}' for device $drbd_device");
		}
		# 
		# Check for a connected state value (Connected, StandAlone, etc)
		#
		if (defined($state{$cs{$drbd_device}})) {
			$state{$cs{$drbd_device}}->{$drbd_device}->{'level'} = 1;
		} else {
			&myexit('CRITICAL',"Connection state unknown value '$cs{$drbd_device}' for device $drbd_device");
		}
		# 
		# Debugging information
		#
		if (defined($debug_mode) && ($debug_mode == 1)) {
			print STDERR "<$prog_name device $drbd_device status>\n";
			for my $key ( keys %state ) {
				if (defined($state{$key}->{$drbd_device}->{'level'})) {
					print STDERR "$key $state{$key}->{'value'}\n";
				}
			}
			print STDERR "</$prog_name device $drbd_device status>\n";
		}
		#
		# Determine if any values are CRITICAL or WARNING
		#
		for my $key ( keys %state ) {
			if (defined($state{$key}->{$drbd_device}->{'level'})) {
				if ($state{$key}->{'value'} eq "CRITICAL") {
					$critical{$drbd_device} = 1;
				}
				if ($state{$key}->{'value'} eq "WARNING") {
					$warning{$drbd_device} = 1;
				}
			}
		}
	}
}

sub report_status {
	my $message;
	my $critical_count=int(keys %critical);
	my $warning_count=int(keys %warning);
	if ($critical_count > 0) {
		#
		# We found a CRITICAL situation
		#
		my $i = 0;
		for my $device (sort keys %critical) {
			$message.=sprintf("Device %d%s $cs{$device}%s%s", $device,defined($st{$device})?" $st{$device}":"",defined($ld{$device})?" $ld{$device}":"",defined($ds{$device})?" $ds{$device}":""); 
			$i++;
			if ($i != $critical_count) {
				$message.=", ";
			}
		}
		&myexit('CRITICAL',$message);
	} elsif ($warning_count > 0) {
		#
		# We found a WARNING situation
		#
		my $i = 0;
		for my $device (sort keys %warning) {
			$message.=sprintf("Device %d%s $cs{$device}%s%s", $device,defined($st{$device})?" $st{$device}":"",defined($ld{$device})?" $ld{$device}":"",defined($ds{$device})?" $ds{$device}":""); 
			$i++;
			if ($i != $warning_count) {
				$message.=", ";
			}
		}
		&myexit('WARNING',$message);
	} else {
		#
		# Everything checks out OK
		#
		my $device_count=int(keys %check);
		if ($device_count == 1) {
			for my $device ( sort keys %check ) {
				$message=sprintf("Device %d%s $cs{$device}%s%s", $device,defined($st{$device})?" $st{$device}":"",defined($ld{$device})?" $ld{$device}":"",defined($ds{$device})?" $ds{$device}":"");
			}
		} else {
			my $i = 0;
			for my $device ( sort keys %check ) {
				$message.=sprintf("Dev %d %0.3s%0.3s%0.3s%0.3s", $device,defined($st{$device})?"$st{$device}":"",$cs{$device},defined($ld{$device})?"$ld{$device}":"",defined($ds{$device})?"$ds{$device}":"");
				$i++;
				if ($i != $device_count) {
					$message.=", ";
				}
			}
		}
		&myexit('OK',$message);
	}
}

sub set_values {
	#
	# Set item to value requested
	#
	my ($items,$value) = @_;
	my @items = split(/,/,$items);
	foreach my $item (@items) {
		if (defined($state{$item})) {
			$state{$item}->{'value'} = "$value";
		} else {
			print STDERR "State '$item' not found\n"; 
		}
	}
}

sub change_values {
	#
	# Look for all values of a given type, set requested value to OK
	# and all other values to CRITICAL
	#
	my ($argument,$type,$error1,$error2) = @_;
	if ((defined($state{$argument})) && 
	    ($state{$argument}->{'type'} eq "$type")) {
		for my $key ( keys %state ) {
			if ($state{$key}->{'type'} eq "$type") {
				if ($key eq $argument) {
					&set_values($argument,'OK');
				} else {
					&set_values($key,'CRITICAL');
				}
			} 
		}
	} else {
		&myexit('UNKNOWN',"$error1 option only works for $error2");
	}
}

sub myexit {
	#
	# Print error message and exit
	#
	my ($error, $message) = @_;
	if (!(defined($errorcodes{$error}))) {
		printf STDERR "Error code $error not known\n";
		print "DRBD UNKNOWN: $message\n";
		exit $errorcodes{'UNKNOWN'}->{'retvalue'};
	}
	print "DRBD $error: $message\n";
	exit $errorcodes{$error}->{'retvalue'};
}

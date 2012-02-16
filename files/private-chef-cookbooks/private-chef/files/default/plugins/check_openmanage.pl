#!/usr/bin/perl
#
# Nagios plugin
#
# Monitor Dell server hardware status using Dell OpenManage Server
# Administrator, either locally via NRPE, or remotely via SNMP.
#
# $Id: check_openmanage 18839 2010-12-13 09:48:46Z trondham $
#
# Copyright (C) 2008-2010 Trond H. Amundsen
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

require 5.006;  # Perl v5.6.0 or newer is required
use strict;
use warnings;
use POSIX qw(isatty ceil);
use Getopt::Long qw(:config no_ignore_case);

# Global (package) variables used throughout the code
use vars qw( $NAME $VERSION $AUTHOR $CONTACT $E_OK $E_WARNING $E_CRITICAL
	     $E_UNKNOWN $FW_LOCK $USAGE $HELP $LICENSE
	     $snmp_session $snmp_error $omreport $globalstatus $global
	     $linebreak $omopt_chassis $omopt_system $blade
	     $exit_code $snmp
	     %check %opt %reverse_exitcode %status2nagios
	     %snmp_status %snmp_probestatus %probestatus2nagios %sysinfo
	     %blacklist %nagios_alert_count %count %snmp_enclosure %snmp_controller
	     @perl_warnings @controllers @enclosures @perfdata
	     @report_storage @report_chassis @report_other
	  );

#---------------------------------------------------------------------
# Initialization and global variables
#---------------------------------------------------------------------

# Collect perl warnings in an array
$SIG{__WARN__} = sub { push @perl_warnings, [@_]; };

# Version and similar info
$NAME    = 'check_openmanage';
$VERSION = '3.6.3';
$AUTHOR  = 'Trond H. Amundsen';
$CONTACT = 't.h.amundsen@usit.uio.no';

# Exit codes
$E_OK       = 0;
$E_WARNING  = 1;
$E_CRITICAL = 2;
$E_UNKNOWN  = 3;

# Firmware update lock file [FIXME: location on Windows?]
$FW_LOCK = '/var/lock/.spsetup';  # default on Linux

# Usage text
$USAGE = <<"END_USAGE";
Usage: $NAME [OPTION]...
END_USAGE

# Help text
$HELP = <<'END_HELP';

GENERAL OPTIONS:

   -p, --perfdata      Output performance data [default=no]
   -t, --timeout       Plugin timeout in seconds [default=30]
   -c, --critical      Custom temperature critical limits
   -w, --warning       Custom temperature warning limits
   -d, --debug         Debug output, reports everything
   -h, --help          Display this help text
   -V, --version       Display version info

SNMP OPTIONS:

   -H, --hostname      Hostname or IP (required for SNMP)
   -C, --community     SNMP community string [default=public]
   -P, --protocol      SNMP protocol version [default=2]
   --port              SNMP port number [default=161]
   -6, --ipv6          Use IPv6 instead of IPv4 [default=no]
   --tcp               Use TCP instead of UDP [default=no]

OUTPUT OPTIONS:

   -i, --info          Prefix any alerts with the service tag
   -e, --extinfo       Append system info to alerts
   -s, --state         Prefix alerts with alert state
   -S, --short-state   Prefix alerts with alert state abbreviated
   -o, --okinfo        Verbosity when check result is OK
   -I, --htmlinfo      HTML output with clickable links

CHECK CONTROL AND BLACKLISTING:

   -a, --all           Check everything, even log content
   -b, --blacklist     Blacklist missing and/or failed components
   --only              Only check a certain component or alert type
   --check             Fine-tune which components are checked
   --no-storage        Don't check storage

For more information and advanced options, see the manual page or URL:
  http://folk.uio.no/trondham/software/check_openmanage.html
END_HELP

# Version and license text
$LICENSE = <<"END_LICENSE";
$NAME $VERSION
Copyright (C) 2008-2010 $AUTHOR
License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.

Written by $AUTHOR <$CONTACT>
END_LICENSE

# Options with default values
%opt = ( 'blacklist'         => [],       # blacklisting
	 'check'             => [],       # check control
	 'critical'          => [],       # temperature critical limits
	 'warning'           => [],       # temperature warning limits
	 'timeout'           => 30,       # default timeout is 30 seconds
	 'debug'             => 0,        # debugging / verbose output
	 'help'              => 0,        # display help output
	 'perfdata'          => undef,    # output performance data
	 'info'              => 0,        # display servicetag
	 'extinfo'           => 0,        # display extra info
	 'htmlinfo'          => undef,    # html tags in output
	 'postmsg'           => undef,    # post message
	 'state'             => 0,        # display alert type
	 'short-state'       => 0,        # display alert type (short)
	 'okinfo'            => 0,        # default "ok" output level
	 'linebreak'         => undef,    # specify linebreak
	 'version'           => 0,        # plugin version info
         'all'               => 0,        # check everything
	 'only'              => undef,    # only one component
	 'no_storage'        => 0,        # don't check storage
	 'omreport'          => undef,    # omreport path
	 'port'              => 161,      # default SNMP port
	 'hostname'          => undef,    # hostname or IP
	 'community'         => 'public', # SMNP v1 or v2c
	 'protocol'          => 2,        # default SNMP protocol 2c
	 'ipv6'              => 0,        # default is IPv4
	 'tcp'               => 0,        # default is UDP
	 'username'          => undef,    # SMNP v3
	 'authpassword'      => undef,    # SMNP v3
	 'authkey'           => undef,    # SMNP v3
	 'authprotocol'      => undef,    # SMNP v3
	 'privpassword'      => undef,    # SMNP v3
	 'privkey'           => undef,    # SMNP v3
	 'privprotocol'      => undef,    # SMNP v3
         'use_get_table'     => 0,        # hack for SNMPv3 on Windows with net-snmp
       );

# Get options
GetOptions('b|blacklist=s'      => \@{ $opt{blacklist} },
	   'check=s'            => \@{ $opt{check} },
	   'c|critical=s'       => \@{ $opt{critical} },
	   'w|warning=s'        => \@{ $opt{warning} },
	   't|timeout=i'        => \$opt{timeout},
	   'd|debug'            => \$opt{debug},
	   'h|help'             => \$opt{help},
	   'V|version'          => \$opt{version},
	   'p|perfdata:s'       => \$opt{perfdata},
	   'i|info'             => \$opt{info},
	   'e|extinfo'          => \$opt{extinfo},
	   'I|htmlinfo:s'       => \$opt{htmlinfo},
	   'postmsg=s'          => \$opt{postmsg},
	   's|state'            => \$opt{state},
	   'S|short-state'      => \$opt{shortstate},
	   'o|ok-info=i'        => \$opt{okinfo},
	   'linebreak=s'        => \$opt{linebreak},
	   'a|all'              => \$opt{all},
	   'only=s'             => \$opt{only},
	   'no-storage'         => \$opt{no_storage},
	   'omreport=s'         => \$opt{omreport},
	   'port=i'             => \$opt{port},
	   'H|hostname=s'       => \$opt{hostname},
	   'C|community=s'      => \$opt{community},
	   'P|protocol=i'       => \$opt{protocol},
	   '6|ipv6'             => \$opt{ipv6},
	   'tcp'                => \$opt{tcp},
	   'U|username=s'       => \$opt{username},
	   'authpassword=s'     => \$opt{authpassword},
	   'authkey=s'          => \$opt{authkey},
	   'authprotocol=s'     => \$opt{authprotocol},
	   'privpassword=s'     => \$opt{privpassword},
	   'privkey=s'          => \$opt{privkey},
	   'privprotocol=s'     => \$opt{privprotocol},
           'use-get_table'      => \$opt{use_get_table},
	  ) or do { print $USAGE; exit $E_UNKNOWN };

# If user requested help
if ($opt{help}) {
    print $USAGE, $HELP;
    exit $E_OK;
}

# If user requested version info
if ($opt{version}) {
    print $LICENSE;
    exit $E_OK;
}

# Setting timeout
$SIG{ALRM} = sub {
    print "PLUGIN TIMEOUT: $NAME timed out after $opt{timeout} seconds\n";
    exit $E_UNKNOWN;
};
alarm $opt{timeout};

# If we're using SNMP
$snmp = defined $opt{hostname} ? 1 : 0;

# SNMP session variables
$snmp_session = undef;
$snmp_error   = undef;

# The omreport command
$omreport = undef;

# Check flags, override available with the --check option
%check = ( 'storage'     => 1,   # check storage subsystem
	   'memory'      => 1,   # check memory (dimms)
	   'fans'        => 1,   # check fan status
	   'power'       => 1,   # check power supplies
	   'temp'        => 1,   # check temperature
	   'cpu'         => 1,   # check processors
	   'voltage'     => 1,   # check voltage
	   'batteries'   => 1,   # check battery probes
	   'amperage'    => 1,   # check power consumption
	   'intrusion'   => 1,   # check intrusion detection
	   'sdcard'      => 1,   # check removable flash media (SD cards)
	   'alertlog'    => 0,   # check the alert log
	   'esmlog'      => 0,   # check the ESM log (hardware log)
	   'esmhealth'   => 1,   # check the ESM log overall health
	 );

# Default line break
$linebreak = isatty(*STDOUT) ? "\n" : '<br/>';

# Line break from option
if (defined $opt{linebreak}) {
    if ($opt{linebreak} eq 'REG') {
	$linebreak = "\n";
    }
    elsif ($opt{linebreak} eq 'HTML') {
	$linebreak = '<br/>';
    }
    else {
	$linebreak = $opt{linebreak};
    }
}

# Exit with status=UNKNOWN if there is firmware upgrade in progress
if (!$snmp && -f $FW_LOCK) {
    print "MONITORING DISABLED - Firmware update in progress ($FW_LOCK exists)\n";
    exit $E_UNKNOWN;
}

# List of controllers and enclosures
@controllers = ();  # controllers
@enclosures  = ();  # enclosures
%snmp_enclosure   = ();  # enclosures

# Messages
@report_storage = ();  # messages with associated nagios level (storage)
@report_chassis = ();  # messages with associated nagios level (chassis)
@report_other   = ();  # messages with associated nagios level (other)

# Counters for everything
%count
  = (
     'pdisk'  => 0, # number of physical disks
     'vdisk'  => 0, # number of logical drives (virtual disks)
     'temp'   => 0, # number of temperature probes
     'volt'   => 0, # number of voltage probes
     'amp'    => 0, # number of amperage probes
     'intr'   => 0, # number of intrusion probes
     'dimm'   => 0, # number of memory modules
     'mem'    => 0, # total memory
     'fan'    => 0, # number of fan probes
     'cpu'    => 0, # number of CPUs
     'bat'    => 0, # number of batteries
     'power'  => 0, # number of power supplies
     'sd'     => 0, # number of SD cards
     'esm'    => {
		  'Critical'     => 0, # critical entries in ESM log
		  'Non-Critical' => 0, # warning entries in ESM log
		  'Ok'           => 0, # ok entries in ESM log
		 },
     'alert'  => {
		  'Critical'     => 0, # critical entries in alert log
		  'Non-Critical' => 0, # warning entries in alert log
		  'Ok'           => 0, # ok entries in alert log
		 },
    );

# Performance data
@perfdata = ();

# Global health status
$global         = 1;      # default is to check global status
$globalstatus   = $E_OK;  # default global health status is "OK"

# Nagios error levels reversed
%reverse_exitcode
  = (
     $E_OK       => 'OK',
     $E_WARNING  => 'WARNING',
     $E_CRITICAL => 'CRITICAL',
     $E_UNKNOWN  => 'UNKNOWN',
    );

# OpenManage (omreport) and SNMP error levels
%status2nagios
  = (
     'Unknown'         => $E_CRITICAL,
     'Critical'        => $E_CRITICAL,
     'Non-Critical'    => $E_WARNING,
     'Ok'              => $E_OK,
     'Non-Recoverable' => $E_CRITICAL,
     'Other'           => $E_CRITICAL,
    );

# Status via SNMP
%snmp_status
  = (
     1 => 'Other',
     2 => 'Unknown',
     3 => 'Ok',
     4 => 'Non-Critical',
     5 => 'Critical',
     6 => 'Non-Recoverable',
    );

# Probe Status via SNMP
%snmp_probestatus
  = (
     1  => 'Other',               # probe status is not one of the following:
     2  => 'Unknown',             # probe status is unknown (not known or monitored)
     3  => 'Ok',                  # probe is reporting a value within the thresholds
     4  => 'nonCriticalUpper',    # probe has crossed upper noncritical threshold
     5  => 'criticalUpper',       # probe has crossed upper critical threshold
     6  => 'nonRecoverableUpper', # probe has crossed upper non-recoverable threshold
     7  => 'nonCriticalLower',    # probe has crossed lower noncritical threshold
     8  => 'criticalLower',       # probe has crossed lower critical threshold
     9  => 'nonRecoverableLower', # probe has crossed lower non-recoverable threshold
     10 => 'failed',              # probe is not functional
    );

# Probe status translated to Nagios alarm levels
%probestatus2nagios
  = (
     'Other'               => $E_CRITICAL,
     'Unknown'             => $E_CRITICAL,
     'Ok'                  => $E_OK,
     'nonCriticalUpper'    => $E_WARNING,
     'criticalUpper'       => $E_CRITICAL,
     'nonRecoverableUpper' => $E_CRITICAL,
     'nonCriticalLower'    => $E_WARNING,
     'criticalLower'       => $E_CRITICAL,
     'nonRecoverableLower' => $E_CRITICAL,
     'failed'              => $E_CRITICAL,
    );

# System information gathered
%sysinfo
  = (
     'bios'     => 'N/A',  # BIOS version
     'biosdate' => 'N/A',  # BIOS release date
     'serial'   => 'N/A',  # serial number (service tag)
     'model'    => 'N/A',  # system model
     'rev'      => q{},    # system revision
     'osname'   => 'N/A',  # OS name
     'osver'    => 'N/A',  # OS version
     'om'       => 'N/A',  # OMSA version
     'bmc'      => 0,      # HAS baseboard management controller (BMC)
     'rac'      => 0,      # HAS remote access controller (RAC)
     'rac_name' => 'N/A',  # remote access controller (RAC)
     'bmc_fw'   => 'N/A',  # BMC firmware
     'rac_fw'   => 'N/A',  # RAC firmware
    );

# Adjust which checks to perform
adjust_checks() if defined $opt{check};

# Blacklisted components
%blacklist = defined $opt{blacklist} ? %{ get_blacklist() } : ();

# If blacklisting is in effect, don't check global health status
if (scalar keys %blacklist > 0) {
    $global = 0;
}

# Take into account new hardware and blades
$omopt_chassis = 'chassis';  # default "chassis" option to omreport
$omopt_system  = 'system';   # default "system" option to omreport
$blade         = 0;          # if this is a blade system

# Some initializations and checking before we begin
if ($snmp) {
    snmp_initialize();    # initialize SNMP
    snmp_check();         # check that SNMP works
    snmp_detect_blade();  # detect blade via SNMP
}
else {
    # Find the omreport binary
    find_omreport();
    # Check help output from omreport, see which options are available.
    # Also detecting blade via omreport.
    check_omreport_options();
}


#---------------------------------------------------------------------
# Helper functions
#---------------------------------------------------------------------

#
# Store a message in one of the message arrays
#
sub report {
    my ($type, $msg, $exval, $id) = @_;
    defined $id or $id = q{};

    my %type2array
      = (
	 'storage' => \@report_storage,
	 'chassis' => \@report_chassis,
	 'other'   => \@report_other,
	);

    return push @{ $type2array{$type} }, [ $msg, $exval, $id ];
}


#
# Run command, put resulting output lines in an array and return a
# pointer to that array
#
sub run_command {
    my $command = shift;

    open my $CMD, '-|', $command
      or do { report('other', "Couldn't run command '$command': $!", $E_UNKNOWN)
		and return [] };
    my @lines = <$CMD>;
    close $CMD
      or do { report('other', "Couldn't close filehandle for command '$command': $!", $E_UNKNOWN)
		and return \@lines };
    return \@lines;
}

#
# Run command, put resulting output in a string variable and return it
#
sub slurp_command {
    my $command = shift;

    open my $CMD, '-|', $command
      or do { report('other', "Couldn't run command '$command': $!", $E_UNKNOWN) and return };
    my $rawtext = do { local $/ = undef; <$CMD> }; # slurping
    close $CMD;

    # NOTE: We don't check the return value of close() since omreport
    # does something weird sometimes.

    return $rawtext;
}

#
# Initialize SNMP
#
sub snmp_initialize {
    # Legal SNMP v3 protocols
    my $snmp_v3_privprotocol = qr{\A des|aes|aes128|3des|3desde \z}xms;
    my $snmp_v3_authprotocol = qr{\A md5|sha \z}xms;

    # Parameters to Net::SNMP->session()
    my %param
      = (
	 '-port'     => $opt{port},
	 '-hostname' => $opt{hostname},
	 '-version'  => $opt{protocol},
	);

    # Setting the domain (IP version and transport protocol)
    my $transport = $opt{tcp} ? 'tcp' : 'udp';
    my $ipversion = $opt{ipv6} ? 'ipv6' : 'ipv4';
    $param{'-domain'} = "$transport/$ipversion";

    # Parameters for SNMP v3
    if ($opt{protocol} == 3) {

	# Username is mandatory
	if (defined $opt{username}) {
	    $param{'-username'} = $opt{username};
	}
	else {
	    print "SNMP ERROR: With SNMPv3 the username must be specified\n";
	    exit $E_UNKNOWN;
	}

	# Authpassword is optional
	if (defined $opt{authpassword}) {
	    $param{'-authpassword'} = $opt{authpassword};
	}

	# Authkey is optional
	if (defined $opt{authkey}) {
	    $param{'-authkey'} = $opt{authkey};
	}

	# Privpassword is optional
	if (defined $opt{privpassword}) {
	    $param{'-privpassword'} = $opt{privpassword};
	}

	# Privkey is optional
	if (defined $opt{privkey}) {
	    $param{'-privkey'} = $opt{privkey};
	}

	# Privprotocol is optional
	if (defined $opt{privprotocol}) {
	    if ($opt{privprotocol} =~ m/$snmp_v3_privprotocol/xms) {
		$param{'-privprotocol'} = $opt{privprotocol};
	    }
	    else {
		print "SNMP ERROR: Unknown privprotocol '$opt{privprotocol}', "
		  . "must be one of [des|aes|aes128|3des|3desde]\n";
		exit $E_UNKNOWN;
	    }
	}

	# Authprotocol is optional
	if (defined $opt{authprotocol}) {
	    if ($opt{authprotocol} =~ m/$snmp_v3_authprotocol/xms) {
		$param{'-authprotocol'} = $opt{authprotocol};
	    }
	    else {
		print "SNMP ERROR: Unknown authprotocol '$opt{authprotocol}', "
		  . "must be one of [md5|sha]\n";
		exit $E_UNKNOWN;
	    }
	}
    }
    # Parameters for SNMP v2c or v1
    elsif ($opt{protocol} == 2 or $opt{protocol} == 1) {
	$param{'-community'} = $opt{community};
    }
    else {
	print "SNMP ERROR: Unknown SNMP version '$opt{protocol}'\n";
	exit $E_UNKNOWN;
    }

    # Try to initialize the SNMP session
    if ( eval { require Net::SNMP; 1 } ) {
	($snmp_session, $snmp_error) = Net::SNMP->session( %param );
	if (!defined $snmp_session) {
	    printf "SNMP: %s\n", $snmp_error;
	    exit $E_UNKNOWN;
	}
    }
    else {
	print "ERROR: You need perl module Net::SNMP to run $NAME in SNMP mode\n";
	exit $E_UNKNOWN;
    }
    return;
}

#
# Checking if SNMP works by probing for "chassisModelName", which all
# servers should have
#
sub snmp_check {
    my $chassisModelName = '1.3.6.1.4.1.674.10892.1.300.10.1.9.1';
    my $result = $snmp_session->get_request(-varbindlist => [$chassisModelName]);

    # Typically if remote host isn't responding
    if (!defined $result) {
	printf "SNMP CRITICAL: %s\n", $snmp_session->error;
	exit $E_CRITICAL;
    }

    # If OpenManage isn't installed or is not working
    if ($result->{$chassisModelName} =~ m{\A noSuch (Instance|Object) \z}xms) {
	print "ERROR: (SNMP) OpenManage is not installed or is not working correctly\n";
	exit $E_UNKNOWN;
    }
    return;
}

#
# Detecting blade via SNMP
#
sub snmp_detect_blade {
    my $DellBaseBoardType = '1.3.6.1.4.1.674.10892.1.300.80.1.7.1.1';
    my $result = $snmp_session->get_request(-varbindlist => [$DellBaseBoardType]);

    # Identify blade. Older models (4th and 5th gen models) and/or old
    # OMSA (4.x) don't have this OID. If we get "noSuchInstance" or
    # similar, we assume that this isn't a blade
    if (exists $result->{$DellBaseBoardType} && $result->{$DellBaseBoardType} eq '3') {
	$blade = 1;
    }
    return;
}

#
# Locate the omreport binary
#
sub find_omreport {
    # If user has specified path to omreport
    if (defined $opt{omreport} and -x $opt{omreport}) {
	$omreport = qq{"$opt{omreport}"};
	return;
    }

    # Possible full paths for omreport
    my @omreport_paths
      = (
	 '/opt/dell/srvadmin/bin/omreport',              # default on Linux with OMSA >= 6.2.0
	 '/usr/bin/omreport',                            # default on Linux with OMSA < 6.2.0
	 '/opt/dell/srvadmin/oma/bin/omreport.sh',       # alternate on Linux
	 '/opt/dell/srvadmin/oma/bin/omreport',          # alternate on Linux
	 'C:\Program Files (x86)\Dell\SysMgt\oma\bin\omreport.exe', # default on Windows x64
	 'C:\Program Files\Dell\SysMgt\oma\bin\omreport.exe',       # default on Windows x32
	 'c:\progra~1\dell\sysmgt\oma\bin\omreport.exe', # 8bit legacy default on Windows x32
	 'c:\progra~2\dell\sysmgt\oma\bin\omreport.exe', # 8bit legacy default on Windows x64
	);

    # Find the one to use
  OMREPORT_PATH:
    foreach my $bin (@omreport_paths) {
	if (-x $bin) {
	    $omreport = qq{"$bin"};
	    last OMREPORT_PATH;
	}
    }

    # Exit with status=UNKNOWN if OM is not installed, or we don't
    # have permission to execute the binary
    if (!defined $omreport) {
	print "ERROR: Dell OpenManage Server Administrator (OMSA) is not installed\n";
	exit $E_UNKNOWN;
    }
    return;
}

#
# Checks output from 'omreport -?' and searches for arguments to
# omreport, to accommodate deprecated options "chassis" and "system"
# (on newer hardware), as well as blade servers.
#
sub check_omreport_options {
    foreach (@{ run_command("$omreport -? 2>&1") }) {
       if (m/\A servermodule /xms) {
	   # If "servermodule" argument to omreport exists, use it
	   # instead of argument "system"
           $omopt_system = 'servermodule';
       }
       elsif (m/\A mainsystem /xms) {
	   # If "mainsystem" argument to omreport exists, use it
	   # instead of argument "chassis"
           $omopt_chassis = 'mainsystem';
       }
       elsif (m/\A modularenclosure /xms) {
	   # If "modularenclusure" argument to omreport exists, assume
	   # that this is a blade
           $blade = 1;
       }
    }
    return;
}

#
# Read the blacklist option and return a hash containing the
# blacklisted components
#
sub get_blacklist {
    my @bl = ();
    my %blacklist = ();

    if (scalar @{ $opt{blacklist} } >= 0) {
	foreach my $black (@{ $opt{blacklist} }) {
	    my $tmp = q{};
	    if (-f $black) {
		open my $BL, '<', $black
		  or do { report('other', "Couldn't open blacklist file $black: $!", $E_UNKNOWN)
			    and return {} };
		chomp($tmp = <$BL>);
		close $BL;
	    }
	    else {
		$tmp = $black;
	    }
	    push @bl, $tmp;
	}
    }

    return {} if $#bl < 0;

    # Parse blacklist string, put in hash
    foreach my $black (@bl) {
	my @comps = split m{/}xms, $black;
	foreach my $c (@comps) {
	    next if $c !~ m/=/xms;
	    my ($key, $val) = split /=/xms, $c;
	    my @vals = split /,/xms, $val;
	    $blacklist{$key} = \@vals;
	}
    }

    return \%blacklist;
}

#
# Read the check option and adjust the hash %check, which is a rough
# list of components to be checked
#
sub adjust_checks {
    my @cl = ();

    # First, take the '--no-storage' option
    if ($opt{no_storage}) {
	$check{storage} = 0;
    }

    # Adjust checking based on the '--all' option
    if ($opt{all}) {
	# Check option usage
	if (defined $opt{only} and $opt{only} !~ m{\A critical|warning \z}xms) {
	    print qq{ERROR: Wrong simultaneous usage of the "--all" and "--only" options\n};
	    exit $E_UNKNOWN;
	}
	if (scalar @{ $opt{check} } > 0) {
	    print qq{ERROR: Wrong simultaneous usage of the "--all" and "--check" options\n};
	    exit $E_UNKNOWN;
	}

	# set the check hash to check everything
	map { $_ = 1 } values %check;

	return;
    }

    # Adjust checking based on the '--only' option
    if (defined $opt{only} and $opt{only} !~ m{\A critical|warning \z}xms) {
	# Check option usage
	if (scalar @{ $opt{check} } > 0) {
	    print qq{ERROR: Wrong simultaneous usage of the "--only" and "--check" options\n};
	    exit $E_UNKNOWN;
	}
	if (! exists $check{$opt{only}} && $opt{only} ne 'chassis') {
	    print qq{ERROR: "$opt{only}" is not a known keyword for the "--only" option\n};
	    exit $E_UNKNOWN;
	}

	# reset the check hash
	map { $_ = 0 } values %check;

	# adjust the check hash
	if ($opt{only} eq 'chassis') {
	    map { $check{$_} = 1 } qw(memory fans power temp cpu voltage
				      batteries amperage intrusion esmhealth);
	}
	else {
	    $check{$opt{only}} = 1;
	}

	return;
    }

    # Adjust checking based on the '--check' option
    if (scalar @{ $opt{check} } >= 0) {
	foreach my $check (@{ $opt{check} }) {
	    my $tmp = q{};
	    if (-f $check) {
		open my $CL, '<', $check
		  or do { report('other', "Couldn't open check file $check: $!", $E_UNKNOWN) and return };
		chomp($tmp = <$CL>);
		close $CL;
	    }
	    else {
		$tmp = $check;
	    }
	    push @cl, $tmp;
	}
    }

    return if $#cl < 0;

    # Parse checklist string, put in hash
    foreach my $check (@cl) {
	my @checks = split /,/xms, $check;
	foreach my $c (@checks) {
	    next if $c !~ m/=/xms;
	    my ($key, $val) = split /=/xms, $c;
	    $check{$key} = $val;
	}
    }

    # Check if we should check global health status
  CHECK_KEY:
    foreach (keys %check) {
	next CHECK_KEY if $_ eq 'esmlog';   # not part of global status
	next CHECK_KEY if $_ eq 'alertlog'; # not part of global status

	if ($check{$_} == 0) { # found something with checking turned off
	    $global = 0;
	    last CHECK_KEY;
	}
    }

    return;
}

#
# Runs omreport and returns an array of anonymous hashes containing
# the output.
# Takes one argument: string containing parameters to omreport
#
sub run_omreport {
    my $command = shift;
    my @output  = ();
    my @keys    = ();

    # Errors that are OK. Some low-end poweredge (and blades) models
    # don't have RAID controllers, intrusion detection sensor, or
    # redundant/instrumented power supplies etc.
    my $ok_errors
      = qr{
            Intrusion\sinformation\sis\snot\sfound\sfor\sthis\ssystem  # No intrusion probe
          | No\sinstrumented\spower\ssupplies\sfound\son\sthis\ssystem # No instrumented PS (blades/low-end)
          | No\sbattery\sprobes\sfound\son\sthis\ssystem               # No battery probes
          | Invalid\scommand:\spwrmonitoring                           # Old hardware
          | Hardware\sor\sfeature\snot\spresent\.                      # SD cards
          | Invalid\scommand:\sremovableflashmedia                     # SD cards with old OMSA
          | Error\sCorrection;                                         # Memory stuff. Not really an error (new in OMSA 6.4)
#          | Current\sprobes\snot\sfound                                # OMSA + RHEL5.4 bug
#          | No\scontrollers\sfound                                     # No RAID controller
        }xms;

    # Errors that are OK on blade servers
    my $ok_blade_errors
      = qr{
              No\sfan\sprobes\sfound\son\sthis\ssystem   # No fan probes
      }xms;

    # Run omreport and fetch output
    my $rawtext = slurp_command("$omreport $command -fmt ssv 2>&1");
    return [] if !defined $rawtext;

    # Workaround for Openmanage BUG introduced in OMSA 5.5.0
    $rawtext =~ s{\n;}{;}gxms if $command eq 'storage controller';

    # Report if no controllers found
    if ($command eq 'storage controller' and $rawtext =~ m{No\scontrollers\sfound}xms) {
	report('storage', 'Storage Error! No controllers found', $E_UNKNOWN);
    }

    # Openmanage sometimes puts a linebreak between "Error" and the
    # actual error text
    $rawtext =~ s{^Error\s*\n}{Error: }xms;

    # Parse output, store in array
    for ((split m{\n}xms, $rawtext)) {
	if (m{\AError}xms) {
	    next if m{$ok_errors}xms;
	    next if ($blade and m{$ok_blade_errors}xms);
	    report('other', "Problem running 'omreport $command': $_", $E_UNKNOWN);
	}

	next if !m/(.*?;){2}/xms;  # ignore lines with less than 3 fields
	my @vals = split /;/xms;
	if ($vals[0] =~ m/\A (Index|ID|Severity|Processor|Current\sSpeed|Connector\sName) \z/xms) {
	    @keys = @vals;
	}
	else {
	    my $i = 0;
	    push @output, { map { $_ => $vals[$i++] } @keys };
	}

    }

    # Finally, return the collected information
    return \@output;
}

#
# Checks if a component is blacklisted. Returns 1 if the component is
# blacklisted, 0 otherwise. Takes two arguments:
#   arg1: component name
#   arg2: component id or index
#
sub blacklisted {
    my $name = shift;  # component name
    my $id   = shift;  # component id
    my $ret  = 0;      # return value

    if (defined $blacklist{$name}) {
	foreach my $comp (@{ $blacklist{$name} }) {
	    if (defined $id and ($comp eq $id or uc($comp) eq 'ALL')) {
		$ret = 1;
	    }
	}
    }

    return $ret;
}

# Converts the NexusID from SNMP to our version
sub convert_nexus {
    my $nexus = shift;
    $nexus =~ s{\A \\}{}xms;
    $nexus =~ s{\\}{:}gxms;
    return $nexus;
}

# Sets custom temperature thresholds based on user supplied options
sub custom_temperature_thresholds {
    my $type   = shift; # type of threshold, either w (warning) or c (critical)
    my %thres  = ();    # will contain the thresholds
    my @limits = ();    # holds the input

    my @opt =  $type eq 'w' ? @{ $opt{warning} } : @{ $opt{critical} };

    if (scalar @opt >= 0) {
	foreach my $t (@opt) {
	    my $tmp = q{};
	    if (-f $t) {
		open my $F, '<', $t
		  or do { report('other', "Couldn't open temperature threshold file $t: $!",
				 $E_UNKNOWN) and return {} };
		$tmp = <$F>;
		close $F;
	    }
	    else {
		$tmp = $t;
	    }
	    push @limits, $tmp;
	}
    }

    # Parse checklist string, put in hash
    foreach my $th (@limits) {
	my @tmp = split m{,}xms, $th;
	foreach my $t (@tmp) {
	    next if $t !~ m{=}xms;
	    my ($key, $val) = split m{=}xms, $t;
	    if ($val =~ m{/}xms) {
		my ($max, $min) = split m{/}xms, $val;
		$thres{$key}{max} = $max;
		$thres{$key}{min} = $min;
	    }
	    else {
		$thres{$key}{max} = $val;
	    }
	}
    }

    return \%thres;
}


# Gets the output from SNMP result according to the OIDs checked
sub get_snmp_output {
    my ($result,$oidref) = @_;
    my @temp   = ();
    my @output = ();

    foreach my $oid (keys %{ $result }) {
	my $short = $oid;
	$short =~ s{\s}{}gxms;                   # remove whitespace
	$short =~ s{\A (.+) \. (\d+) \z}{$1}xms; # remove last number
	my $id = $2;
	if (exists $oidref->{$short}) {
	    $temp[$id]{$oidref->{$short}} = $result->{$oid};
	}
    }

    # Remove any empty indexes
    foreach my $out (@temp) {
	if (defined $out) {
	    push @output, $out;
	}
    }

    return \@output;
}


# Map the controller or other item in-place
sub map_item {
    my ($key, $val, $list)  = @_;

    foreach my $lst (@{ $list }) {
	if (!exists $lst->{$key}) {
	    $lst->{$key} = $val;
	}
    }
    return;
}

# Return the URL for official Dell documentation for a specific
# PowerEdge server
sub documentation_url {
    my $model = shift;

    # create model short form, e.g. "r710"
    $model =~ s{\A PowerEdge \s (.+?) \z}{lc($1)}exms;

    # special case for blades (e.g. M600, M710), they have common
    # documentation
    $model =~ s{\A m\d+ \z}{m}xms;

    return 'http://support.dell.com/support/edocs/systems/pe' . $model . '/';
}

# Return the URL for warranty information for a server with a given
# serial number (servicetag)
sub warranty_url {
    my $tag = shift;

    # Dell support sites for different parts of the world
    my %supportsite
      = (
	 'emea' => 'http://support.euro.dell.com/support/topics/topic.aspx/emea/shared/support/my_systems_info/',
	 'ap'   => 'http://supportapj.dell.com/support/topics/topic.aspx/ap/shared/support/my_systems_info/en/details?',
	 'glob' => 'http://support.dell.com/support/topics/global.aspx/support/my_systems_info/details?',
	);

    # warranty URLs for different country codes
    my %url
      = (
	 # EMEA
	 'at' => $supportsite{emea} . 'de/details?c=at&l=de&ServiceTag=',  # Austria
	 'be' => $supportsite{emea} . 'nl/details?c=be&l=nl&ServiceTag=',  # Belgium
	 'cz' => $supportsite{emea} . 'cs/details?c=cz&l=cs&ServiceTag=',  # Czech Republic
	 'de' => $supportsite{emea} . 'de/details?c=de&l=de&ServiceTag=',  # Germany
	 'dk' => $supportsite{emea} . 'da/details?c=dk&l=da&ServiceTag=',  # Denmark
	 'es' => $supportsite{emea} . 'es/details?c=es&l=es&ServiceTag=',  # Spain
	 'fi' => $supportsite{emea} . 'fi/details?c=fi&l=fi&ServiceTag=',  # Finland
	 'fr' => $supportsite{emea} . 'fr/details?c=fr&l=fr&ServiceTag=',  # France
	 'gr' => $supportsite{emea} . 'en/details?c=gr&l=el&ServiceTag=',  # Greece
	 'it' => $supportsite{emea} . 'it/details?c=it&l=it&ServiceTag=',  # Italy
	 'il' => $supportsite{emea} . 'en/details?c=il&l=en&ServiceTag=',  # Israel
	 'me' => $supportsite{emea} . 'en/details?c=me&l=en&ServiceTag=',  # Middle East
	 'no' => $supportsite{emea} . 'no/details?c=no&l=no&ServiceTag=',  # Norway
	 'nl' => $supportsite{emea} . 'nl/details?c=nl&l=nl&ServiceTag=',  # The Netherlands
	 'pl' => $supportsite{emea} . 'pl/details?c=pl&l=pl&ServiceTag=',  # Poland
	 'pt' => $supportsite{emea} . 'en/details?c=pt&l=pt&ServiceTag=',  # Portugal
	 'ru' => $supportsite{emea} . 'ru/details?c=ru&l=ru&ServiceTag=',  # Russia
	 'se' => $supportsite{emea} . 'sv/details?c=se&l=sv&ServiceTag=',  # Sweden
	 'uk' => $supportsite{emea} . 'en/details?c=uk&l=en&ServiceTag=',  # United Kingdom
	 'za' => $supportsite{emea} . 'en/details?c=za&l=en&ServiceTag=',  # South Africa
	 # America
	 'br' => $supportsite{glob} . 'c=br&l=pt&ServiceTag=',  # Brazil
	 'ca' => $supportsite{glob} . 'c=ca&l=en&ServiceTag=',  # Canada
	 'mx' => $supportsite{glob} . 'c=mx&l=es&ServiceTag=',  # Mexico
	 'us' => $supportsite{glob} . 'c=us&l=en&ServiceTag=',  # USA
	 # Asia/Pacific
	 'au' => $supportsite{ap} . 'c=au&l=en&ServiceTag=',  # Australia
	 'cn' => $supportsite{ap} . 'c=cn&l=zh&ServiceTag=',  # China
	 'in' => $supportsite{ap} . 'c=in&l=en&ServiceTag=',  # India
	 # default fallback
	 'XX' => $supportsite{glob} . 'ServiceTag=',  # default
	);

    if (exists $url{$opt{htmlinfo}}) {
	return $url{$opt{htmlinfo}} . $tag;
    }
    else {
	return $url{XX} . $tag;
    }
}


# This helper function returns the corresponding value of a hash key,
# but takes into account that the key may not exist
sub get_hashval {
    my $key  = shift || return undef;
    my $hash = shift;
    return defined $hash->{$key} ? $hash->{$key} : "Undefined value $key";
}

# Find component status from hash
sub get_snmp_status {
    my $key  = shift || return 'Unknown';
    return exists $snmp_status{$key} ? $snmp_status{$key} : 'Unknown';
}

# Find component status from hash
sub get_snmp_probestatus {
    my $key  = shift || return 'Unknown';
    return exists $snmp_probestatus{$key} ? $snmp_probestatus{$key} : 'Unknown';
}

# Check that a hash entry is defined and not an empty string. Return a
# chosen string (parameter) if these conditions are not met
sub get_nonempty_string {
    my $key  = shift;  # key to check
    my $hash = shift;  # hash where the key belongs
    my $alt  = shift;  # alternate return value
    if (defined $hash->{$key} and $hash->{$key} ne q{}) {
	return $hash->{$key};
    }
    return $alt;
}


#---------------------------------------------------------------------
# Check functions
#---------------------------------------------------------------------

#-----------------------------------------
# Check global health status
#-----------------------------------------
sub check_global {
    my $health = $E_OK;

    if ($snmp) {
	#
	# Checks global status, i.e. both storage and chassis
	#
	my $systemStateGlobalSystemStatus = '1.3.6.1.4.1.674.10892.1.200.10.1.2.1';
	my $result = $snmp_session->get_request(-varbindlist => [$systemStateGlobalSystemStatus]);
	if (!defined $result) {
	    printf "SNMP ERROR [global]: %s\n", $snmp_error;
	    exit $E_UNKNOWN;
	}
	$health = $status2nagios{get_snmp_status($result->{$systemStateGlobalSystemStatus})};
    }
    else {
	#
	# NB! This does not check storage, only chassis...
	#
	foreach (@{ run_command("$omreport $omopt_system -fmt ssv") }) {
	    next if !m/;/xms;
	    next if m/\A SEVERITY;COMPONENT/xms;
	    if (m/\A (.+?);Main\sSystem(\sChassis)? /xms) {
		$health = $status2nagios{$1};
		last;
	    }
	}
    }

    # Return the status
    return $health;
}


#-----------------------------------------
# STORAGE: Check controllers
#-----------------------------------------
sub check_controllers {
    return if blacklisted('ctrl', 'all');

    my $nexus    = undef;
    my $name     = undef;
    my $state    = undef;
    my $status   = undef;
    my $minfw    = undef;
    my $mindr    = undef;
    my $firmware = undef;
    my $driver   = undef;
    my $minstdr  = undef;  # Minimum required Storport driver version
    my $stdr     = undef;  # Storport driver version
    my @output   = ();

    if ($snmp) {
	my %ctrl_oid
	  = (
	     '1.3.6.1.4.1.674.10893.1.20.130.1.1.1'  => 'controllerNumber',
	     '1.3.6.1.4.1.674.10893.1.20.130.1.1.2'  => 'controllerName',
	     '1.3.6.1.4.1.674.10893.1.20.130.1.1.5'  => 'controllerState',
	     '1.3.6.1.4.1.674.10893.1.20.130.1.1.8'  => 'controllerFWVersion',
	     '1.3.6.1.4.1.674.10893.1.20.130.1.1.38' => 'controllerComponentStatus',
	     '1.3.6.1.4.1.674.10893.1.20.130.1.1.39' => 'controllerNexusID',
	     '1.3.6.1.4.1.674.10893.1.20.130.1.1.41' => 'controllerDriverVersion',
	     '1.3.6.1.4.1.674.10893.1.20.130.1.1.44' => 'controllerMinFWVersion',
	     '1.3.6.1.4.1.674.10893.1.20.130.1.1.45' => 'controllerMinDriverVersion',
	     '1.3.6.1.4.1.674.10893.1.20.130.1.1.55' => 'controllerStorportDriverVersion',
	     '1.3.6.1.4.1.674.10893.1.20.130.1.1.56' => 'controllerMinRequiredStorportVer',
	    );

	# We use get_table() here for the odd case where a server has
	# two or more controllers, and where some OIDs are missing on
	# one of the controllers.
	my $controllerTable = '1.3.6.1.4.1.674.10893.1.20.130.1';
	my $result = $snmp_session->get_table(-baseoid => $controllerTable);

	if (!defined $result) {
	    report('storage', 'Storage Error! No controllers found', $E_UNKNOWN);
	    return;
	}

	@output = @{ get_snmp_output($result, \%ctrl_oid) };
    }
    else {
	@output = @{ run_omreport('storage controller') };
    }

    my %ctrl_state
      = (
	 0 => 'Unknown',
	 1 => 'Ready',
	 2 => 'Failed',
	 3 => 'Online',
	 4 => 'Offline',
	 6 => 'Degraded',
	);

  CTRL:
    foreach my $out (@output) {
	if ($snmp) {
	    $name     = $out->{controllerName} || 'Unknown controller';
	    $state    = get_hashval($out->{controllerState}, \%ctrl_state);
	    $status   = get_snmp_status($out->{controllerComponentStatus});
	    $minfw    = $out->{controllerMinFWVersion} || undef;
	    $mindr    = $out->{controllerMinDriverVersion} || undef;
	    $firmware = $out->{controllerFWVersion} || 'N/A';
	    $driver   = $out->{controllerDriverVersion} || 'N/A';
	    $minstdr  = $out->{'controllerMinRequiredStorportVer'} || undef;
	    $stdr     = $out->{controllerStorportDriverVersion} || undef;
	    $nexus    = convert_nexus(($out->{controllerNexusID} || 9999));
	}
	else {
	    $nexus    = get_nonempty_string('ID', $out, '9999');
	    $name     = get_nonempty_string('Name', $out, 'Unknown controller');
	    $state    = get_nonempty_string('State', $out, 'Unknown state');
	    $status   = get_nonempty_string('Status', $out, 'Unknown');
	    $minfw    = $out->{'Minimum Required Firmware Version'} ne 'Not Applicable'
	      ? $out->{'Minimum Required Firmware Version'} : undef;
	    $mindr    = $out->{'Minimum Required Driver Version'} ne 'Not Applicable'
	      ? $out->{'Minimum Required Driver Version'} : undef;
	    $firmware = $out->{'Firmware Version'} ne 'Not Applicable'
	      ? $out->{'Firmware Version'} : 'N/A';
	    $driver   = $out->{'Driver Version'} ne 'Not Applicable'
	      ? $out->{'Driver Version'} : 'N/A';
	    $minstdr  = (exists $out->{'Minimum Required Storport Driver Version'}
			 and $out->{'Minimum Required Storport Driver Version'} ne 'Not Applicable')
	      ? $out->{'Minimum Required Storport Driver Version'} : undef;
	    $stdr     = (exists $out->{'Storport Driver Version'}
			 and $out->{'Storport Driver Version'} ne 'Not Applicable')
	      ? $out->{'Storport Driver Version'} : undef;
	}

	$name =~ s{\s+\z}{}xms; # remove trailing whitespace
	push @controllers, $nexus;

	# Collecting some storage info
	$sysinfo{'controller'}{$nexus}{'id'}       = $nexus;
	$sysinfo{'controller'}{$nexus}{'name'}     = $name;
	$sysinfo{'controller'}{$nexus}{'driver'}   = $driver;
	$sysinfo{'controller'}{$nexus}{'firmware'} = $firmware;
	$sysinfo{'controller'}{$nexus}{'storport'} = $stdr;

	# Store controller info for future use (SNMP)
	if ($snmp) {
	    $snmp_controller{$out->{controllerNumber}} = $nexus;
	}

	next CTRL if blacklisted('ctrl', $nexus);

	# Special case: old firmware
	if (!blacklisted('ctrl_fw', $nexus) && defined $minfw) {
	    chomp $firmware;
	    my $msg = sprintf q{Controller %d [%s]: Firmware '%s' is out of date},
	      $nexus, $name, $firmware;
	    report('storage', $msg, $E_WARNING, $nexus);
	}
	# Special case: old driver
	if (!blacklisted('ctrl_driver', $nexus) && defined $mindr) {
	    chomp $driver;
	    my $msg = sprintf q{Controller %d [%s]: Driver '%s' is out of date},
	      $nexus, $name, $driver;
	    report('storage', $msg, $E_WARNING, $nexus);
	}
	# Special case: old storport driver
	if (!blacklisted('ctrl_stdr', $nexus) && defined $minstdr) {
	    chomp $stdr;
	    my $msg = sprintf q{Controller %d [%s]: Storport driver '%s' is out of date},
	      $nexus, $name, $stdr;
	    report('storage', $msg, $E_WARNING, $nexus);
	}
	# Ok
	if ($status eq 'Ok' or ($status eq 'Non-Critical'
				and (defined $minfw or defined $mindr or defined $minstdr))) {
	    my $msg = sprintf 'Controller %d [%s] is %s',
	      $nexus, $name, $state;
	    report('storage', $msg, $E_OK, $nexus);
	}
        # Default
	else {
	    my $msg = sprintf 'Controller %d [%s] needs attention: %s',
	      $nexus, $name, $state;
	    report('storage', $msg, $status2nagios{$status}, $nexus);
	}
    }
    return;
}


#-----------------------------------------
# STORAGE: Check physical drives
#-----------------------------------------
sub check_physical_disks {
    return if $#controllers == -1;
    return if blacklisted('pdisk', 'all');

    my $nexus    = undef;
    my $name     = undef;
    my $state    = undef;
    my $status   = undef;
    my $fpred    = undef;
    my $progr    = undef;
    my $ctrl     = undef;
    my $vendor   = undef;  # disk vendor
    my $product  = undef;  # product ID
    my $capacity = undef;  # disk length (size) in bytes
    my $media    = undef;  # media type (e.g. HDD, SSD)
    my $bus      = undef;  # bus protocol (e.g. SAS, SATA)
    my $spare    = undef;  # spare state (e.g. global hotspare)
    my @output  = ();

    if ($snmp) {
	my %pdisk_oid
	  = (
	     '1.3.6.1.4.1.674.10893.1.20.130.4.1.2'  => 'arrayDiskName',
	     '1.3.6.1.4.1.674.10893.1.20.130.4.1.3'  => 'arrayDiskVendor',
	     '1.3.6.1.4.1.674.10893.1.20.130.4.1.4'  => 'arrayDiskState',
	     '1.3.6.1.4.1.674.10893.1.20.130.4.1.6'  => 'arrayDiskProductID',
	     '1.3.6.1.4.1.674.10893.1.20.130.4.1.9'  => 'arrayDiskEnclosureID',
	     '1.3.6.1.4.1.674.10893.1.20.130.4.1.10' => 'arrayDiskChannel',
	     '1.3.6.1.4.1.674.10893.1.20.130.4.1.11' => 'arrayDiskLengthInMB',
	     '1.3.6.1.4.1.674.10893.1.20.130.4.1.15' => 'arrayDiskTargetID',
	     '1.3.6.1.4.1.674.10893.1.20.130.4.1.21' => 'arrayDiskBusType',
	     '1.3.6.1.4.1.674.10893.1.20.130.4.1.22' => 'arrayDiskSpareState',
	     '1.3.6.1.4.1.674.10893.1.20.130.4.1.24' => 'arrayDiskComponentStatus',
	     '1.3.6.1.4.1.674.10893.1.20.130.4.1.26' => 'arrayDiskNexusID',
	     '1.3.6.1.4.1.674.10893.1.20.130.4.1.31' => 'arrayDiskSmartAlertIndication',
	     '1.3.6.1.4.1.674.10893.1.20.130.4.1.35' => 'arrayDiskMediaType',
	     '1.3.6.1.4.1.674.10893.1.20.130.5.1.7'  => 'arrayDiskEnclosureConnectionControllerNumber',
	     '1.3.6.1.4.1.674.10893.1.20.130.6.1.7'  => 'arrayDiskChannelConnectionControllerNumber',
	    );
	my $result = undef;
	if ($opt{use_get_table}) {
	    my $arrayDiskTable = '1.3.6.1.4.1.674.10893.1.20.130.4';
	    my $arrayDiskEnclosureConnectionControllerNumber = '1.3.6.1.4.1.674.10893.1.20.130.5.1.7';
	    my $arrayDiskChannelConnectionControllerNumber = '1.3.6.1.4.1.674.10893.1.20.130.6.1.7';

	    $result  = $snmp_session->get_table(-baseoid => $arrayDiskTable);
	    my $ext1 = $snmp_session->get_table(-baseoid => $arrayDiskEnclosureConnectionControllerNumber);
	    my $ext2 = $snmp_session->get_table(-baseoid => $arrayDiskChannelConnectionControllerNumber);

	    if (defined $result) {
		defined $ext1 && map { $$result{$_} = $$ext1{$_} } keys %{ $ext1 };
		defined $ext2 && map { $$result{$_} = $$ext2{$_} } keys %{ $ext2 };
	    }
	}
	else {
	    $result = $snmp_session->get_entries(-columns => [keys %pdisk_oid]);
	}

	if (!defined $result) {
	    printf "SNMP ERROR [storage / pdisk]: %s.\n", $snmp_session->error;
	    $snmp_session->close;
	    exit $E_UNKNOWN;
	}

	@output = @{ get_snmp_output($result, \%pdisk_oid) };
    }
    else {
	foreach my $c (@controllers) {
	    # This blacklists disks with broken firmware, which includes
	    # illegal XML characters that makes openmanage choke on itself
	    next if blacklisted('ctrl_pdisk', $c);

	    push @output, @{ run_omreport("storage pdisk controller=$c") };
	    map_item('ctrl', $c, \@output);
	}
    }

    my %spare_state
      = (
	 1  => 'VD member',    # disk is a member of a virtual disk
	 2  => 'DG member',    # disk is a member of a disk group
	 3  => 'Global HS',    # disk is a global hot spare
	 4  => 'Dedicated HS', # disk is a dedicated hot spare
	 5  => 'no',           # not a spare
	 99 => 'n/a',          # not applicable
	);

    my %media_type
      = (
	 1 => 'unknown',
	 2 => 'HDD',
	 3 => 'SSD',
	);

    my %bus_type
      = (
	 1 => 'SCSI',
	 2 => 'IDE',
	 3 => 'Fibre Channel',
	 4 => 'SSA',
	 6 => 'USB',
	 7 => 'SATA',
	 8 => 'SAS',
	);

    my %pdisk_state
      = (
	 0  => 'Unknown',
	 1  => 'Ready',
	 2  => 'Failed',
	 3  => 'Online',
	 4  => 'Offline',
	 6  => 'Degraded',
	 7  => 'Recovering',
	 11 => 'Removed',
	 15 => 'Resynching',
         22 => 'Replacing', # FIXME: this one is not defined in the OMSA MIBs
	 24 => 'Rebuilding',
	 25 => 'No Media',
	 26 => 'Formatting',
	 28 => 'Diagnostics',
	 34 => 'Predictive failure',
	 35 => 'Initializing',
	 39 => 'Foreign',
	 40 => 'Clear',
	 41 => 'Unsupported',
	 53 => 'Incompatible',
	);

    # Check physical disks on each of the controllers
  PDISK:
    foreach my $out (@output) {
	if ($snmp) {
	    $name     = $out->{arrayDiskName} || 'Unknown disk';
	    $state    = get_hashval($out->{arrayDiskState}, \%pdisk_state);
	    $status   = get_snmp_status($out->{arrayDiskComponentStatus});
	    $fpred    = defined $out->{arrayDiskSmartAlertIndication}
	      && $out->{arrayDiskSmartAlertIndication} == 2 ? 1 : 0;
	    $progr    = q{};
	    $nexus    = convert_nexus(($out->{arrayDiskNexusID} || 9999));
	    $vendor   = $out->{arrayDiskVendor} || 'Unknown vendor';
	    $product  = $out->{arrayDiskProductID} || 'Unknown product ID';
	    $spare    = get_hashval($out->{arrayDiskSpareState}, \%spare_state);
	    $bus      = exists $out->{arrayDiskBusType}
	      ? get_hashval($out->{arrayDiskBusType}, \%bus_type) : undef;
	    $media    = exists $out->{arrayDiskMediaType}
	      ? get_hashval($out->{arrayDiskMediaType}, \%media_type) : undef;
	    $capacity = exists $out->{arrayDiskLengthInMB}
	      ? $out->{arrayDiskLengthInMB} * 1024**2 : -1;

	    # try to find the controller where the disk belongs
	    if (exists $out->{arrayDiskEnclosureConnectionControllerNumber}) {
		# for disks that are attached to an enclosure
		$ctrl = $snmp_controller{$out->{arrayDiskEnclosureConnectionControllerNumber}};
	    }
	    elsif (exists $out->{arrayDiskChannelConnectionControllerNumber}) {
		# for disks that are not attached to an enclosure
		$ctrl = $snmp_controller{$out->{arrayDiskChannelConnectionControllerNumber}};
	    }
	    else {
		# last resort... use the nexus id (old/broken hardware)
		$ctrl = $nexus;
		$ctrl =~ s{\A (\d+) : .* \z}{$1}xms;
	    }
	}
	else {
	    $name     = get_nonempty_string('Name', $out, 'Unknown disk');
	    $state    = get_nonempty_string('State', $out, 'Unknown state');
	    $status   = get_nonempty_string('Status', $out, 'Unknown');
	    $fpred    = lc(get_nonempty_string('Failure Predicted', $out, q{})) eq 'yes' ? 1 : 0;
	    $progr    = ' [' . get_nonempty_string('Progress', $out, q{}) . ']';
	    $nexus    = join q{:}, $out->{ctrl}, $out->{'ID'};
	    $vendor   = get_nonempty_string('Vendor ID', $out, 'Unknown Vendor');
	    $product  = get_nonempty_string('Product ID', $out, 'Unknown Product ID');
	    $media    = get_nonempty_string('Media', $out, undef);
	    $bus      = get_nonempty_string('Bus Protocol', $out, undef);
	    $spare    = get_nonempty_string('Hot Spare', $out, q{});
	    $ctrl     = $out->{ctrl};
	    $capacity = get_nonempty_string('Capacity', $out, q{});
	    $capacity =~ s{\A .*? \((\d+) \s bytes\) \z}{$1}xms;
	    if ($capacity eq 'Unavailable') {
		$capacity = -1;
	    }
	}

	next PDISK if blacklisted('pdisk', $nexus);
	$count{pdisk}++;

        $vendor  =~ s{\s+\z}{}xms; # remove trailing whitespace
        $product =~ s{\s+\z}{}xms; # remove trailing whitespace

	# If the disk is bad, the vendor field may be empty
	if ($vendor eq q{}) { $vendor = 'Unknown Vendor'; }

	# Hot spare stuff
	if ($spare eq 'Global') { $spare = 'Global HS'; }
	elsif ($spare eq 'Dedicated') { $spare = 'Dedicated HS'; }
	elsif ($spare !~ m{\A Global|Dedicated}xms) { $spare = undef; }

	# Calculate human readable capacity
	if ($capacity == -1) {
	    # capacity is unknown
	    $capacity = 'Unknown Size';
	}
	else {
	    $capacity = ceil($capacity / 1000**3) >= 1000
	      ? sprintf '%.1fTB', ($capacity / 1000**4)
		: sprintf '%.0fGB', ($capacity / 1000**3);
	    $capacity = '450GB' if $capacity eq '449GB';  # quick fix for 450GB disks
	    $capacity = '300GB' if $capacity eq '299GB';  # quick fix for 300GB disks
	    $capacity = '146GB' if $capacity eq '147GB';  # quick fix for 146GB disks
	    $capacity = '100GB' if $capacity eq '99GB';   # quick fix for 100GB disks
	}

	# Capitalize only the first letter of the vendor name
	$vendor = (substr $vendor, 0, 1) . lc (substr $vendor, 1, length $vendor);

	# Remove unnecessary trademark rubbish from vendor name
	$vendor =~ s{\(tm\)\z}{}xms;

	# bus and media aren't always defined
	my $busmedia = q{};
	if    (defined $bus && defined $media)   { $busmedia = "$bus-$media "; }
	elsif (defined $bus && ! defined $media) { $busmedia = "$bus ";        }
	elsif (! defined $bus && defined $media) { $busmedia = "$media ";      }

	# Special case: Failure predicted
	if ($fpred) {
	    my $msg = sprintf '%s [%s %s, %s] on ctrl %d needs attention: Failure Predicted',
	      $name, $vendor, $product, $capacity, $ctrl;
            $msg .= " ($state)" if $state ne 'Predictive failure';
	    report('storage', $msg,
                   ($status2nagios{$status} == $E_CRITICAL ? $E_CRITICAL : $E_WARNING), $nexus);
	}
	# Special case: Rebuilding / Replacing
	elsif ($state =~ m{\A Rebuilding|Replacing \z}xms) {
	    my $msg = sprintf '%s [%s %s, %s] on ctrl %d is %s%s',
	      $name, $vendor, $product, $capacity, $ctrl, $state, $progr;
	    report('storage', $msg, $E_WARNING, $nexus);
	}
	# Default
	elsif ($status ne 'Ok') {
	    my $msg =  sprintf '%s [%s %s, %s] on ctrl %d needs attention: %s',
	      $name, $vendor, $product, $capacity, $ctrl, $state;
	    report('storage', $msg, $status2nagios{$status}, $nexus);
	}
	# Ok
	else {
	    my $msg = sprintf '%s [%s%s] on ctrl %d is %s',
	      $name, $busmedia, $capacity, $ctrl, $state;
	    if (defined $spare) { $msg .= " ($spare)"; }
	    report('storage', $msg, $E_OK, $nexus);
	}
    }
    return;
}


#-----------------------------------------
# STORAGE: Check logical drives
#-----------------------------------------
sub check_virtual_disks {
    return if $#controllers == -1;
    return if blacklisted('vdisk', 'all');

    my $name   = undef;
    my $nexus  = undef;
    my $dev    = undef;
    my $state  = undef;
    my $status = undef;
    my $layout = undef;
    my $size   = undef;
    my $progr  = undef;
    my $ctrl   = undef;
    my @output = ();

    if ($snmp) {
	my %vdisk_oid
	  = (
	     '1.3.6.1.4.1.674.10893.1.20.140.1.1.3'  => 'virtualDiskDeviceName',
	     '1.3.6.1.4.1.674.10893.1.20.140.1.1.4'  => 'virtualDiskState',
	     '1.3.6.1.4.1.674.10893.1.20.140.1.1.6'  => 'virtualDiskLengthInMB',
	     '1.3.6.1.4.1.674.10893.1.20.140.1.1.13' => 'virtualDiskLayout',
	     '1.3.6.1.4.1.674.10893.1.20.140.1.1.20' => 'virtualDiskComponentStatus',
	     '1.3.6.1.4.1.674.10893.1.20.140.1.1.21' => 'virtualDiskNexusID',
	    );
	my $result = undef;
	if ($opt{use_get_table}) {
	    my $virtualDiskTable = '1.3.6.1.4.1.674.10893.1.20.140.1';
	    $result = $snmp_session->get_table(-baseoid => $virtualDiskTable);
	}
	else {
	    $result = $snmp_session->get_entries(-columns => [keys %vdisk_oid]);
	}

	# No logical drives is OK
	return if !defined $result;

	@output = @{ get_snmp_output($result, \%vdisk_oid) };
    }
    else {
	foreach my $c (@controllers) {
	    push @output, @{ run_omreport("storage vdisk controller=$c") };
	    map_item('ctrl', $c, \@output);
	}
    }

    my %vdisk_state
      = (
	 0  => 'Unknown',
	 1  => 'Ready',
	 2  => 'Failed',
	 3  => 'Online',
	 4  => 'Offline',
	 6  => 'Degraded',
	 15 => 'Resynching',
	 16 => 'Regenerating',
	 24 => 'Rebuilding',
	 26 => 'Formatting',
	 32 => 'Reconstructing',
	 35 => 'Initializing',
	 36 => 'Background Initialization',
	 38 => 'Resynching Paused',
	 52 => 'Permanently Degraded',
	 54 => 'Degraded Redundancy',
	);

    my %vdisk_layout
      = (
	 1  => 'Concatenated',
	 2  => 'RAID-0',
	 3  => 'RAID-1',
	 7  => 'RAID-5',
	 8  => 'RAID-6',
	 10 => 'RAID-10',
	 12 => 'RAID-50',
	 19 => 'Concatenated RAID-1',
	 24 => 'RAID-60',
         25 => 'CacheCade',
	);

    # Check virtual disks on each of the controllers
  VDISK:
    foreach my $out (@output) {
	if ($snmp) {
	    $dev    = $out->{virtualDiskDeviceName} || 'Unknown device';
	    $state  = get_hashval($out->{virtualDiskState}, \%vdisk_state);
	    $layout = get_hashval($out->{virtualDiskLayout}, \%vdisk_layout);
	    $status = get_snmp_status($out->{virtualDiskComponentStatus});
	    $size   = sprintf '%.2f GB', ($out->{virtualDiskLengthInMB} || 0) / 1024;
	    $progr  = q{};  # not available via SNMP
	    $nexus  = convert_nexus(($out->{virtualDiskNexusID} || 9999));
	}
	else {
	    $dev    = get_nonempty_string('Device Name', $out, 'Unknown device');
	    $state  = get_nonempty_string('State', $out, 'Unknown state');
	    $status = get_nonempty_string('Status', $out, 'Unknown');
	    $layout = get_nonempty_string('Layout', $out, 'Unknown layout');
	    $size   = get_nonempty_string('Size', $out, 'Unavailable');
	    $size   =~ s{\A (.*GB).* \z}{$1}xms;
	    $progr  = ' [' . get_nonempty_string('Progress', $out, q{}) . ']';
	    $ctrl   = $out->{ctrl};
	    $nexus  = join q{:}, $ctrl, get_nonempty_string('ID', $out, '9999');
	}

	next VDISK if blacklisted('vdisk', $nexus);
	$count{vdisk}++;

	# The device name is undefined sometimes
	$dev = q{} if !defined $dev;

	# Special case: Regenerating
	if ($state eq 'Regenerating') {
	    my $msg = sprintf q{Logical Drive '%s' [%s, %s] is %s%s},
	      $dev, $layout, $size, $state, $progr;
	    report('storage', $msg, $E_WARNING, $nexus);
	}
	# Default
	elsif ($status ne 'Ok') {
	    my $msg = sprintf q{Logical Drive '%s' [%s, %s] needs attention: %s},
	      $dev, $layout, $size, $state;
	    report('storage', $msg, $status2nagios{$status}, $nexus);
	}
	# Ok
	else {
	    my $msg = sprintf q{Logical Drive '%s' [%s, %s] is %s},
	      $dev, $layout, $size, $state;
	    report('storage', $msg, $E_OK, $nexus);
	}
    }
    return;
}


#-----------------------------------------
# STORAGE: Check cache batteries
#-----------------------------------------
sub check_cache_battery {
    return if $#controllers == -1;
    return if blacklisted('bat', 'all');

    my $id     = undef;
    my $nexus  = undef;
    my $state  = undef;
    my $status = undef;
    my $ctrl   = undef;
    my $learn  = undef; # learn state
    my $pred   = undef; # battery's ability to be charged
    my @output = ();

    if ($snmp) {
	my %bat_oid
	  = (
	     '1.3.6.1.4.1.674.10893.1.20.130.15.1.4'  => 'batteryState',
	     '1.3.6.1.4.1.674.10893.1.20.130.15.1.6'  => 'batteryComponentStatus',
	     '1.3.6.1.4.1.674.10893.1.20.130.15.1.9'  => 'batteryNexusID',
	     '1.3.6.1.4.1.674.10893.1.20.130.15.1.10' => 'batteryPredictedCapacity',
	     '1.3.6.1.4.1.674.10893.1.20.130.15.1.12' => 'batteryLearnState',
	     '1.3.6.1.4.1.674.10893.1.20.130.16.1.5'  => 'batteryConnectionControllerNumber',
	    );
	my $result = undef;
	if ($opt{use_get_table}) {
	    my $batteryTable = '1.3.6.1.4.1.674.10893.1.20.130.15';
            my $batteryConnectionTable = '1.3.6.1.4.1.674.10893.1.20.130.16';

	    $result = $snmp_session->get_table(-baseoid => $batteryTable);
            my $ext = $snmp_session->get_table(-baseoid => $batteryConnectionTable);

	    if (defined $result) {
                defined $ext && map { $$result{$_} = $$ext{$_} } keys %{ $ext };
            }
	}
	else {
	    $result = $snmp_session->get_entries(-columns => [keys %bat_oid]);
	}

	# No cache battery is OK
	return if !defined $result;

	@output = @{ get_snmp_output($result, \%bat_oid) };
    }
    else {
	foreach my $c (@controllers) {
	    push @output, @{ run_omreport("storage battery controller=$c") };
	    map_item('ctrl', $c, \@output);
	}
    }

    my %bat_state
      = (
	 0  => 'Unknown',
	 1  => 'Ready',
	 2  => 'Failed',
	 6  => 'Degraded',
	 7  => 'Reconditioning',
	 9  => 'High',
	 10 => 'Power Low',
	 12 => 'Charging',
	 21 => 'Missing',
	 36 => 'Learning',
	);

    # Specifies the learn state activity of the battery
    my %bat_learn_state
      = (
	 1  => 'Failed',
	 2  => 'Active',
	 4  => 'Timed out',
	 8  => 'Requested',
	 16 => 'Idle',
	);

    # This property displays the battery's ability to be charged
    my %bat_pred_cap
      = (
	 1 => 'Failed',  # The battery cannot be charged and needs to be replaced
	 2 => 'Ready',   # The battery can be charged to full capacity
	 4 => 'Unknown', # The battery is completing a Learn cycle. The charge capacity of the
                         # battery cannot be determined until the Learn cycle is complete
	);

    # Check battery on each of the controllers
  BATTERY:
    foreach my $out (@output) {
	if ($snmp) {
	    $status = get_snmp_status($out->{batteryComponentStatus});
	    $state  = get_hashval($out->{batteryState}, \%bat_state);
	    $learn  = get_hashval($out->{batteryLearnState}, \%bat_learn_state);
	    $pred   = get_hashval($out->{batteryPredictedCapacity}, \%bat_pred_cap);
	    $ctrl   = ($out->{batteryConnectionControllerNumber} || 10000) - 1;
	    $nexus  = convert_nexus(($out->{batteryNexusID} || 9999));
	    $id     = $nexus;
	    $id     =~ s{\A \d+:(\d+) \z}{$1}xms;
	}
	else {
	    $id     = get_nonempty_string('ID', $out, 9999);
	    $state  = get_nonempty_string('State', $out, 'Unknown state');
	    $status = get_nonempty_string('Status', $out, 'Unknown');
	    $learn  = get_nonempty_string('Learn State', $out, 'Unknown learn state');
	    $pred   = get_nonempty_string('Predicted Capacity Status', $out, 'Unknown predicted capacity status');
	    $ctrl   = $out->{'ctrl'};
	    $nexus  = join q{:}, $out->{ctrl}, $id;
	}

	next BATTERY if blacklisted('bat', $nexus);

	# Special case: Charging
	if ($state eq 'Charging') {
	    if ($pred eq 'Failed') {
		my $msg = sprintf 'Cache Battery %d in controller %d is %s (%s) [replace battery]',
		  $id, $ctrl, $state, $pred;
		report('storage', $msg, $E_CRITICAL, $nexus);
	    }
	    else {
		next BATTERY if blacklisted('bat_charge', $nexus);
		my $msg = sprintf 'Cache Battery %d in controller %d is %s (%s) [probably harmless]',
		  $id, $ctrl, $state, $pred;
		report('storage', $msg, $E_WARNING, $nexus);
	    }
	}
	# Special case: Learning (battery learns its capacity)
	elsif ($state eq 'Learning') {
	    if ($learn eq 'Failed') {
		my $msg = sprintf 'Cache Battery %d in controller %d is %s (%s)',
		  $id, $ctrl, $state, $learn;
		report('storage', $msg, $E_CRITICAL, $nexus);
	    }
	    else {
		next BATTERY if blacklisted('bat_charge', $nexus);
		my $msg = sprintf 'Cache Battery %d in controller %d is %s (%s) [probably harmless]',
		  $id, $ctrl, $state, $learn;
		report('storage', $msg, $E_WARNING, $nexus);
	    }
	}
	# Special case: Power Low (first part of recharge cycle)
	elsif ($state eq 'Power Low') {
	    next BATTERY if blacklisted('bat_charge', $nexus);
	    my $msg = sprintf 'Cache Battery %d in controller %d is %s [probably harmless]',
	      $id, $ctrl, $state;
	    report('storage', $msg, $E_WARNING, $nexus);
	}
	# Special case: Degraded and Non-Critical (usually part of recharge cycle)
	elsif ($state eq 'Degraded' && $status eq 'Non-Critical') {
	    next BATTERY if blacklisted('bat_charge', $nexus);
	    my $msg = sprintf 'Cache Battery %d in controller %d is %s (%s) [probably harmless]',
	      $id, $ctrl, $state, $status;
	    report('storage', $msg, $E_WARNING, $nexus);
	}
	# Default
	elsif ($status ne 'Ok') {
	    my $msg = sprintf 'Cache Battery %d in controller %d needs attention: %s (%s)',
	      $id, $ctrl, $state, $status;
	    report('storage', $msg, $status2nagios{$status}, $nexus);
	}
	# Ok
	else {
	    my $msg = sprintf 'Cache Battery %d in controller %d is %s',
	      $id, $ctrl, $state;
	    report('storage', $msg, $E_OK, $nexus);
	}
    }
    return;
}


#-----------------------------------------
# STORAGE: Check connectors (channels)
#-----------------------------------------
sub check_connectors {
    return if $#controllers == -1;
    return if blacklisted('conn', 'all');

    my $nexus  = undef;
    my $name   = undef;
    my $state  = undef;
    my $status = undef;
    my $type   = undef;
    my $ctrl   = undef;
    my @output = ();

    if ($snmp) {
        my %conn_oid
          = (
             '1.3.6.1.4.1.674.10893.1.20.130.2.1.2'  => 'channelName',
             '1.3.6.1.4.1.674.10893.1.20.130.2.1.3'  => 'channelState',
             '1.3.6.1.4.1.674.10893.1.20.130.2.1.8'  => 'channelComponentStatus',
	     '1.3.6.1.4.1.674.10893.1.20.130.2.1.9'  => 'channelNexusID',
             '1.3.6.1.4.1.674.10893.1.20.130.2.1.11' => 'channelBusType',
            );
	my $result = undef;
	if ($opt{use_get_table}) {
	    my $channelTable = '1.3.6.1.4.1.674.10893.1.20.130.2';
	    $result = $snmp_session->get_table(-baseoid => $channelTable);
	}
	else {
	    $result = $snmp_session->get_entries(-columns => [keys %conn_oid]);
	}

        if (!defined $result) {
            printf "SNMP ERROR [storage / channel]: %s.\n", $snmp_session->error;
            $snmp_session->close;
            exit $E_UNKNOWN;
        }

	@output = @{ get_snmp_output($result, \%conn_oid) };
    }
    else {
        foreach my $c (@controllers) {
            push @output, @{ run_omreport("storage connector controller=$c") };
	    map_item('ctrl', $c, \@output);
        }
    }

    my %conn_state
      = (
         0 => 'Unknown',
         1 => 'Ready',
         2 => 'Failed',
         3 => 'Online',
         4 => 'Offline',
         6 => 'Degraded',
        );

    my %conn_bustype
      = (
         1 => 'SCSI',
         2 => 'IDE',
         3 => 'Fibre Channel',
         4 => 'SSA',
         6 => 'USB',
         7 => 'SATA',
         8 => 'SAS',
        );

    # Check connectors on each of the controllers
  CHANNEL:
    foreach my $out (@output) {
        if ($snmp) {
            $name   = $out->{channelName} || 'Unknown channel';
            $status = get_snmp_status($out->{channelComponentStatus});
            $state  = get_hashval($out->{channelState}, \%conn_state);
            $type   = get_hashval($out->{channelBusType}, \%conn_bustype);
	    $nexus  = convert_nexus(($out->{channelNexusID} || 9999));
	    $ctrl   = $nexus;
	    $ctrl   =~ s{(\d+):\d+}{$1}xms;
        }
        else {
            $name   = get_nonempty_string('Name', $out, 'Unknown channel');
            $state  = get_nonempty_string('State', $out, 'Unknown state');
	    $status = get_nonempty_string('Status', $out, 'Unknown');
            $type   = get_nonempty_string('Connector Type', $out, 'Unknown type');
	    $ctrl   = $out->{ctrl};
	    $nexus  = join q{:}, $out->{ctrl}, $out->{'ID'};
        }

        next CHANNEL if blacklisted('conn', $nexus);

	my $msg = sprintf '%s [%s] on controller %d is %s',
	  $name, $type, $ctrl, $state;
        report('storage', $msg, $status2nagios{$status}, $nexus);
    }
    return;
}


#-----------------------------------------
# STORAGE: Check enclosures
#-----------------------------------------
sub check_enclosures {
    return if blacklisted('encl', 'all');

    my $id       = undef;
    my $nexus    = undef;
    my $name     = undef;
    my $state    = undef;
    my $status   = undef;
    my $firmware = undef;
    my $ctrl     = undef;
    my $occupied_slots = undef; # number of occupied slots
    my $total_slots    = undef; # number of total slots
    my @output   = ();

    if ($snmp) {
        my %encl_oid
          = (
             '1.3.6.1.4.1.674.10893.1.20.130.3.1.1'  => 'enclosureNumber',
             '1.3.6.1.4.1.674.10893.1.20.130.3.1.2'  => 'enclosureName',
             '1.3.6.1.4.1.674.10893.1.20.130.3.1.4'  => 'enclosureState',
	     '1.3.6.1.4.1.674.10893.1.20.130.3.1.19' => 'enclosureChannelNumber',
             '1.3.6.1.4.1.674.10893.1.20.130.3.1.24' => 'enclosureComponentStatus',
	     '1.3.6.1.4.1.674.10893.1.20.130.3.1.25' => 'enclosureNexusID',
	     '1.3.6.1.4.1.674.10893.1.20.130.3.1.26' => 'enclosureFirmwareVersion',
	     '1.3.6.1.4.1.674.10893.1.20.130.3.1.31' => 'enclosureOccupiedSlotCount', # new in OMSA 6.3.0
	     '1.3.6.1.4.1.674.10893.1.20.130.3.1.32' => 'enclosureTotalSlots', # new in OMSA 6.3.0
            );
	my $result = undef;
	if ($opt{use_get_table}) {
	    my $enclosureTable = '1.3.6.1.4.1.674.10893.1.20.130.3';
	    $result = $snmp_session->get_table(-baseoid => $enclosureTable);
	}
	else {
	    $result = $snmp_session->get_entries(-columns => [keys %encl_oid]);
	}

        # No enclosures is OK
        return if !defined $result;

	@output = @{ get_snmp_output($result, \%encl_oid) };
    }
    else {
	foreach my $c (@controllers) {
	    push @output, @{ run_omreport("storage enclosure controller=$c") };
	    map_item('ctrl', $c, \@output);
	}
    }

    my %encl_state
      = (
         0 => 'Unknown',
         1 => 'Ready',
         2 => 'Failed',
         3 => 'Online',
         4 => 'Offline',
         6 => 'Degraded',
        );

  ENCLOSURE:
    foreach my $out (@output) {
        if ($snmp) {
            $id       = ($out->{enclosureNumber} || 10000) - 1;
            $name     = $out->{enclosureName} || 'Unknown enclosure';
            $state    = get_hashval($out->{enclosureState}, \%encl_state);
            $status   = get_snmp_status($out->{enclosureComponentStatus});
	    $firmware = $out->{enclosureFirmwareVersion} || 'N/A';
	    $nexus    = convert_nexus(($out->{enclosureNexusID} || 9999));
	    $ctrl     = $nexus;
	    $ctrl     =~ s{\A (\d+):.* \z}{$1}xms;
	    # for the next two, a value of 9999 means feature not available
	    $occupied_slots = defined $out->{enclosureOccupiedSlotCount}
	      && $out->{enclosureOccupiedSlotCount} != 9999
		? $out->{enclosureOccupiedSlotCount} : undef;
	    $total_slots    = defined $out->{enclosureTotalSlots}
	      && $out->{enclosureTotalSlots} != 9999
		? $out->{enclosureTotalSlots} : undef;
        }
        else {
            $id       = get_nonempty_string('ID', $out, 9999);
            $name     = get_nonempty_string('Name', $out, 'Unknown enclosure');
            $state    = get_nonempty_string('State', $out, 'Unknown state');
	    $status   = get_nonempty_string('Status', $out, 'Unknown');
	    $firmware = get_nonempty_string('Firmware Version', $out, 'N/A');
	    $firmware =~ s{Not\sApplicable}{N/A}xms;
	    $nexus    = join q{:}, $out->{ctrl}, $id;
	    $ctrl     = $out->{ctrl};
        }

        $name     =~ s{\s+\z}{}xms; # remove trailing whitespace
        $firmware =~ s{\s+\z}{}xms; # remove trailing whitespace

	# store enclosure data for future use
	if ($snmp) {
	    $snmp_enclosure{$out->{enclosureNumber}}{id}    = $id;
	    $snmp_enclosure{$out->{enclosureNumber}}{name}  = $name;
	    $snmp_enclosure{$out->{enclosureNumber}}{nexus} = $nexus;
	}
	else {
	    push @enclosures, { 'id'    => $id,
				'ctrl'  => $out->{ctrl},
				'name'  => $name };
	}

	# Collecting some storage info
	$sysinfo{'enclosure'}{$nexus}{'id'}       = $nexus;
	$sysinfo{'enclosure'}{$nexus}{'name'}     = $name;
	$sysinfo{'enclosure'}{$nexus}{'firmware'} = $firmware;

        next ENCLOSURE if blacklisted('encl', $nexus);

	my $msg = q{};
	if (defined $occupied_slots && defined $total_slots) {
	    $msg = sprintf 'Enclosure %s [%s, %d/%d slots occupied] on ctrl %d is %s',
	      $nexus, $name, $occupied_slots, $total_slots, $ctrl, $state;
	}
	else {
	    $msg = sprintf 'Enclosure %s [%s] on controller %d is %s',
	      $nexus, $name, $ctrl, $state;
	}
        report('storage', $msg, $status2nagios{$status}, $nexus);
    }
    return;
}


#-----------------------------------------
# STORAGE: Check enclosure fans
#-----------------------------------------
sub check_enclosure_fans {
    return if $#controllers == -1;
    return if blacklisted('encl_fan', 'all');

    my $nexus     = undef;
    my $name      = undef;
    my $state     = undef;
    my $status    = undef;
    my $speed     = undef;
    my $encl_id   = undef;
    my $encl_name = undef;
    my @output    = ();

    if ($snmp) {
	my %fan_oid
	  = (
	     '1.3.6.1.4.1.674.10893.1.20.130.7.1.2'  => 'fanName',
	     '1.3.6.1.4.1.674.10893.1.20.130.7.1.4'  => 'fanState',
	     '1.3.6.1.4.1.674.10893.1.20.130.7.1.11' => 'fanProbeCurrValue',
	     '1.3.6.1.4.1.674.10893.1.20.130.7.1.15' => 'fanComponentStatus',
	     '1.3.6.1.4.1.674.10893.1.20.130.7.1.16' => 'fanNexusID',
	     '1.3.6.1.4.1.674.10893.1.20.130.8.1.4'  => 'fanConnectionEnclosureName',
	     '1.3.6.1.4.1.674.10893.1.20.130.8.1.5'  => 'fanConnectionEnclosureNumber',
	    );
	my $result = undef;
	if ($opt{use_get_table}) {
	    my $fanTable = '1.3.6.1.4.1.674.10893.1.20.130.7';
            my $fanConnectionTable = '1.3.6.1.4.1.674.10893.1.20.130.8';

	    $result = $snmp_session->get_table(-baseoid => $fanTable);
            my $ext = $snmp_session->get_table(-baseoid => $fanConnectionTable);

	    if (defined $result) {
                defined $ext && map { $$result{$_} = $$ext{$_} } keys %{ $ext };
            }
	}
	else {
	    $result = $snmp_session->get_entries(-columns => [keys %fan_oid]);
	}

	# No enclosure fans is OK
	return if !defined $result;

	@output = @{ get_snmp_output($result, \%fan_oid) };
    }
    else {
	foreach my $enc (@enclosures) {
	    push @output, @{ run_omreport("storage enclosure controller=$enc->{ctrl} enclosure=$enc->{id} info=fans") };
	    map_item('ctrl', $enc->{ctrl}, \@output);
	    map_item('encl_id', $enc->{id}, \@output);
	    map_item('encl_name', $enc->{name}, \@output);
	}
    }

    my %fan_state
      = (
	 0  => 'Unknown',
	 1  => 'Ready',
	 2  => 'Failed',
	 3  => 'Online',
	 4  => 'Offline',
	 6  => 'Degraded',
	 21 => 'Missing',
	);

    # Check fans on each of the enclosures
  FAN:
    foreach my $out (@output) {
	if ($snmp) {
	    $name      = $out->{fanName} || 'Unknown fan';
	    $state     = get_hashval($out->{fanState}, \%fan_state);
	    $status    = get_snmp_status($out->{fanComponentStatus});
	    $speed     = $out->{fanProbeCurrValue} || 'N/A';
	    $encl_name = $out->{fanConnectionEnclosureName} || 'Unknown enclosure';
	    $encl_id   = $snmp_enclosure{$out->{fanConnectionEnclosureNumber}}{nexus};
	    $nexus     = convert_nexus(($out->{fanNexusID} || 9999));
	}
	else {
	    $name      = get_nonempty_string('Name', $out, 'Unknown fan');
	    $state     = get_nonempty_string('State', $out, 'Unknown state');
	    $status    = get_nonempty_string('Status', $out, 'Unknown');
	    $speed     = get_nonempty_string('Speed', $out, 'N/A');
	    $encl_id   = join q{:}, $out->{ctrl}, $out->{'encl_id'};
	    $encl_name = $out->{encl_name};
	    $nexus     = join q{:}, $out->{ctrl}, $out->{'encl_id'}, get_nonempty_string('ID', $out, '9999');
	}

	next FAN if blacklisted('encl_fan', $nexus);

	# Default
	if ($status ne 'Ok') {
	    my $msg = sprintf '%s in enclosure %s [%s] needs attention: %s',
	      $name, $encl_id, $encl_name, $state;
	    report('storage', $msg, $status2nagios{$status}, $nexus);
	}
	# Ok
	else {
	    my $msg = sprintf '%s in enclosure %s [%s] is %s (speed=%s)',
	      $name, $encl_id, $encl_name, $state, $speed;
	    report('storage', $msg, $E_OK, $nexus);
	}
    }
    return;
}


#-----------------------------------------
# STORAGE: Check enclosure power supplies
#-----------------------------------------
sub check_enclosure_pwr {
    return if $#controllers == -1;
    return if blacklisted('encl_ps', 'all');

    my $nexus     = undef;
    my $name      = undef;
    my $state     = undef;
    my $status    = undef;
    my $encl_id   = undef;
    my $encl_name = undef;
    my @output    = ();

    if ($snmp) {
	my %ps_oid
	  = (
	     '1.3.6.1.4.1.674.10893.1.20.130.9.1.2'  => 'powerSupplyName',
	     '1.3.6.1.4.1.674.10893.1.20.130.9.1.4'  => 'powerSupplyState',
	     '1.3.6.1.4.1.674.10893.1.20.130.9.1.9'  => 'powerSupplyComponentStatus',
	     '1.3.6.1.4.1.674.10893.1.20.130.9.1.10' => 'powerSupplyNexusID',
	     '1.3.6.1.4.1.674.10893.1.20.130.10.1.4' => 'powerSupplyConnectionEnclosureName',
	     '1.3.6.1.4.1.674.10893.1.20.130.10.1.5' => 'powerSupplyConnectionEnclosureNumber',
	    );
	my $result = undef;
	if ($opt{use_get_table}) {
	    my $powerSupplyTable = '1.3.6.1.4.1.674.10893.1.20.130.9';
            my $powerSupplyConnectionTable = '1.3.6.1.4.1.674.10893.1.20.130.10';

	    $result = $snmp_session->get_table(-baseoid => $powerSupplyTable);
            my $ext = $snmp_session->get_table(-baseoid => $powerSupplyConnectionTable);

	    if (defined $result) {
                defined $ext && map { $$result{$_} = $$ext{$_} } keys %{ $ext };
            }
	}
	else {
	    $result = $snmp_session->get_entries(-columns => [keys %ps_oid]);
	}

	# No enclosure power supplies is OK
	return if !defined $result;

	@output = @{ get_snmp_output($result, \%ps_oid) };
    }
    else {
	foreach my $enc (@enclosures) {
	    push @output, @{ run_omreport("storage enclosure controller=$enc->{ctrl} enclosure=$enc->{id} info=pwrsupplies") };
	    map_item('ctrl', $enc->{ctrl}, \@output);
	    map_item('encl_id', $enc->{id}, \@output);
	    map_item('encl_name', $enc->{name}, \@output);
	}
    }

    my %ps_state
      = (
	 0  => 'Unknown',
	 1  => 'Ready',
	 2  => 'Failed',
	 5  => 'Not Installed',
	 6  => 'Degraded',
	 11 => 'Removed',
	 21 => 'Missing',
	);

    # Check power supplies on each of the enclosures
  PS:
    foreach my $out (@output) {
	if ($snmp) {
	    $name      = $out->{powerSupplyName} || 'Unknown PSU';
	    $state     = get_hashval($out->{powerSupplyState}, \%ps_state);
	    $status    = get_snmp_status($out->{powerSupplyComponentStatus});
	    $encl_id   = $snmp_enclosure{$out->{powerSupplyConnectionEnclosureNumber}}{nexus};
	    $encl_name = $out->{powerSupplyConnectionEnclosureName} || 'Unknown enclosure';
	    $nexus     = convert_nexus(($out->{powerSupplyNexusID} || 9999));
	}
	else {
	    $name      = get_nonempty_string('Name', $out, 'Unknown PSU');
	    $state     = get_nonempty_string('State', $out, 'Unknown state');
	    $status    = get_nonempty_string('Status', $out, 'Unknown');
	    $encl_id   = join q{:}, $out->{ctrl}, $out->{'encl_id'};
	    $encl_name = $out->{encl_name};
	    $nexus     = join q{:}, $out->{ctrl}, $out->{'encl_id'}, get_nonempty_string('ID', $out, '9999');
	}

	next PS if blacklisted('encl_ps', $nexus);

	# Default
	if ($status ne 'Ok') {
	    my $msg = sprintf '%s in enclosure %s [%s] needs attention: %s',
	      $name, $encl_id, $encl_name, $state;
	    report('storage', $msg, $status2nagios{$status}, $nexus);
	}
	# Ok
	else {
	    my $msg = sprintf '%s in enclosure %s [%s] is %s',
	      $name, $encl_id, $encl_name, $state;
	    report('storage', $msg, $E_OK, $nexus);
	}
    }
    return;
}


#-----------------------------------------
# STORAGE: Check enclosure temperatures
#-----------------------------------------
sub check_enclosure_temp {
    return if $#controllers == -1;
    return if blacklisted('encl_temp', 'all');

    my $nexus     = undef;
    my $name      = undef;
    my $state     = undef;
    my $status    = undef;
    my $reading   = undef;
    my $unit      = undef;
    my $max_warn  = undef;
    my $max_crit  = undef;
    my $min_warn  = undef;
    my $min_crit  = undef;
    my $encl_id   = undef;
    my $encl_name = undef;
    my @output    = ();

    if ($snmp) {
	my %temp_oid
	  = (
	     '1.3.6.1.4.1.674.10893.1.20.130.11.1.2'  => 'temperatureProbeName',
	     '1.3.6.1.4.1.674.10893.1.20.130.11.1.4'  => 'temperatureProbeState',
	     '1.3.6.1.4.1.674.10893.1.20.130.11.1.6'  => 'temperatureProbeUnit',
	     '1.3.6.1.4.1.674.10893.1.20.130.11.1.7'  => 'temperatureProbeMinWarning',
	     '1.3.6.1.4.1.674.10893.1.20.130.11.1.8'  => 'temperatureProbeMinCritical',
	     '1.3.6.1.4.1.674.10893.1.20.130.11.1.9'  => 'temperatureProbeMaxWarning',
	     '1.3.6.1.4.1.674.10893.1.20.130.11.1.10' => 'temperatureProbeMaxCritical',
	     '1.3.6.1.4.1.674.10893.1.20.130.11.1.11' => 'temperatureProbeCurValue',
	     '1.3.6.1.4.1.674.10893.1.20.130.11.1.13' => 'temperatureProbeComponentStatus',
	     '1.3.6.1.4.1.674.10893.1.20.130.11.1.14' => 'temperatureProbeNexusID',
	     '1.3.6.1.4.1.674.10893.1.20.130.12.1.4'  => 'temperatureConnectionEnclosureName',
	     '1.3.6.1.4.1.674.10893.1.20.130.12.1.5'  => 'temperatureConnectionEnclosureNumber',
	    );
	my $result = undef;
	if ($opt{use_get_table}) {
	    my $temperatureProbeTable = '1.3.6.1.4.1.674.10893.1.20.130.11';
            my $temperatureConnectionTable = '1.3.6.1.4.1.674.10893.1.20.130.12';

	    $result = $snmp_session->get_table(-baseoid => $temperatureProbeTable);
            my $ext = $snmp_session->get_table(-baseoid => $temperatureConnectionTable);

	    if (defined $result) {
                defined $ext && map { $$result{$_} = $$ext{$_} } keys %{ $ext };
            }
	}
	else {
	    $result = $snmp_session->get_entries(-columns => [keys %temp_oid]);
	}

	# No enclosure temperature probes is OK
	return if !defined $result;

	@output = @{ get_snmp_output($result, \%temp_oid) };
    }
    else {
	foreach my $enc (@enclosures) {
	    push @output, @{ run_omreport("storage enclosure controller=$enc->{ctrl} enclosure=$enc->{id} info=temps") };
	    map_item('ctrl', $enc->{ctrl}, \@output);
	    map_item('encl_id', $enc->{id}, \@output);
	    map_item('encl_name', $enc->{name}, \@output);
	}
    }

    my %temp_state
      = (
	 0  => 'Unknown',
	 1  => 'Ready',
	 2  => 'Failed',
	 4  => 'Offline',
	 6  => 'Degraded',
	 9  => 'Inactive',
	 21 => 'Missing',
	);

    # Check temperature probes on each of the enclosures
  TEMP:
    foreach my $out (@output) {
	if ($snmp) {
	    $name      = $out->{temperatureProbeName} || 'Unknown temp probe';
	    $state     = get_hashval($out->{temperatureProbeState}, \%temp_state);
	    $status    = get_snmp_probestatus($out->{temperatureProbeComponentStatus});
	    $unit      = $out->{temperatureProbeUnit} || 'Unknown unit';
	    $reading   = $out->{temperatureProbeCurValue} || '[N/A]';
	    $max_warn  = $out->{temperatureProbeMaxWarning} || '[N/A]';
	    $max_crit  = $out->{temperatureProbeMaxCritical} || '[N/A]';
	    $min_warn  = $out->{temperatureProbeMinWarning} || '[N/A]';
	    $min_crit  = $out->{temperatureProbeMinCritical} || '[N/A]';
	    $encl_id   = $snmp_enclosure{$out->{temperatureConnectionEnclosureNumber}}{nexus};
	    $encl_name = $out->{temperatureConnectionEnclosureName} || 'Unknown enclosure';
	    $nexus     = convert_nexus(($out->{temperatureProbeNexusID} || 9999));
	}
	else {
	    $name      = get_nonempty_string('Name', $out, 'Unknown temp probe');
	    $state     = get_nonempty_string('State', $out, 'Unknown state');
	    $status    = get_nonempty_string('Status', $out, 'Unknown');
	    $unit      = 'FIXME';
	    $reading   = get_nonempty_string('Reading', $out, '[N/A]');
	    $max_warn  = get_nonempty_string('Maximum Warning Threshold', $out, '[N/A]');
	    $max_crit  = get_nonempty_string('Maximum Failure Threshold', $out, '[N/A]');
	    $min_warn  = get_nonempty_string('Minimum Warning Threshold', $out, '[N/A]');
	    $min_crit  = get_nonempty_string('Minimum Failure Threshold', $out, '[N/A]');
	    $encl_id   = join q{:}, $out->{ctrl}, $out->{'encl_id'};
	    $encl_name = $out->{encl_name};
	    $nexus     = join q{:}, $out->{ctrl}, $out->{'encl_id'}, get_nonempty_string('ID', $out, '9999');
	}

	next TEMP if blacklisted('encl_temp', $nexus);

	# Make sure these values are integers
	$reading  =~ s{\A \s* (-?\d+) \s* C? \s* \z}{$1}xms or $reading  = '[N/A]';
	$max_warn =~ s{\A \s* (-?\d+) \s* C? \s* \z}{$1}xms or $max_warn = '[N/A]';
	$max_crit =~ s{\A \s* (-?\d+) \s* C? \s* \z}{$1}xms or $max_crit = '[N/A]';
	$min_warn =~ s{\A \s* (-?\d+) \s* C? \s* \z}{$1}xms or $min_warn = '[N/A]';
	$min_crit =~ s{\A \s* (-?\d+) \s* C? \s* \z}{$1}xms or $min_crit = '[N/A]';

	# Inactive temp probes
	if ($status eq 'Unknown' and $state eq 'Inactive') {
	    my $msg = sprintf '%s in enclosure %s [%s] is %s',
	      $name, $encl_id, $encl_name, $state;
	    report('storage', $msg, $E_OK, $nexus);
	}
	elsif ($status ne 'Ok' and $max_crit ne '[N/A]' and $reading > $max_crit) {
	    my $msg = sprintf '%s in enclosure %s [%s] is critically high at %d C',
	      $name, $encl_id, $encl_name, $reading;
	    my $err = $snmp ? $probestatus2nagios{$status} : $status2nagios{$status};
	    report('chassis', $msg, $err, $nexus);
	}
	elsif ($status ne 'Ok' and $max_warn ne '[N/A]' and $reading > $max_warn) {
	    my $msg = sprintf '%s in enclosure %s [%s] is too high at %d C',
	      $name, $encl_id, $encl_name, $reading;
	    my $err = $snmp ? $probestatus2nagios{$status} : $status2nagios{$status};
	    report('chassis', $msg, $err, $nexus);
	}
	elsif ($status ne 'Ok' and $min_crit ne '[N/A]' and $reading < $min_crit) {
	    my $msg = sprintf '%s in enclosure %s [%s] is critically low at %d C',
	      $name, $encl_id, $encl_name, $reading;
	    my $err = $snmp ? $probestatus2nagios{$status} : $status2nagios{$status};
	    report('chassis', $msg, $err, $nexus);
	}
	elsif ($status ne 'Ok' and $min_warn ne '[N/A]' and $reading < $min_warn) {
	    my $msg = sprintf '%s in enclosure %s [%s] is too low at %d C',
	      $name, $encl_id, $encl_name, $reading;
	    my $err = $snmp ? $probestatus2nagios{$status} : $status2nagios{$status};
	    report('chassis', $msg, $err, $nexus);
	}
	# Default
	elsif ($status ne 'Ok') {
	    my $msg = sprintf '%s in enclosure %s [%s] is %s',
	      $name, $encl_id, $encl_name, $state;
	    if (defined $reading && $reading =~ m{\A -?\d+ \z}xms) {
		# take into account that with certain states the
		# reading doesn't exist or is not an integer
		$msg .= sprintf ' at %s C', $reading;
		if ($min_warn eq '[N/A]' or $min_crit eq '[N/A]') {
		    $msg .= sprintf ' (max=%s/%s)', $max_warn, $max_crit;
		}
		else {
		    $msg .= sprintf ' (min=%s/%s, max=%s/%s)',
		      $min_warn, $min_crit, $max_warn, $max_crit;
		}
	    }
	    my $err = $snmp ? $probestatus2nagios{$status} : $status2nagios{$status};
	    report('storage', $msg, $err, $nexus);
	}
	# Ok
	else {
	    my $msg = sprintf '%s in enclosure %s [%s]',
	      $name, $encl_id, $encl_name;
	    if (defined $reading && $reading ne '[N/A]') {
		# take into account that with certain states the
		# reading doesn't exist or is not an integer
		$msg .= sprintf ' reads %d C', $reading;
		if ($min_warn eq '[N/A]' or $min_crit eq '[N/A]') {
		    $msg .= sprintf ' (max=%s/%s)', $max_warn, $max_crit;
		}
		else {
		    $msg .= sprintf ' (min=%s/%s, max=%s/%s)',
		      $min_warn, $min_crit, $max_warn, $max_crit;
		}
	    }
	    else {
		$msg .= sprintf ' is %s', $state;
	    }
	    report('storage', $msg, $E_OK, $nexus);
	}

	# Collect performance data
	if (defined $opt{perfdata} && $reading ne '[N/A]') {
	    $name =~ s{\A Temperature\sProbe\s(\d+) \z}{temp_$1}gxms;
	    my $label = "enclosure_${encl_id}_${name}";
	    my $mini = $label;
	    $mini =~ s{enclosure_(.+?)_temp_(.+?)}{e$1t$2}xms;
	    push @perfdata, {
			     label => $label,
			     mini  => $mini,
			     value => $reading,
			     warn  => $max_warn,
			     crit  => $max_crit,
			    };
	}
    }
    return;
}


#-----------------------------------------
# STORAGE: Check enclosure management modules (EMM)
#-----------------------------------------
sub check_enclosure_emms {
    return if $#controllers == -1;
    return if blacklisted('encl_emm', 'all');

    my $nexus     = undef;
    my $name      = undef;
    my $state     = undef;
    my $status    = undef;
    my $encl_id   = undef;
    my $encl_name = undef;
    my @output    = ();

    if ($snmp) {
	my %emms_oid
	  = (
	     '1.3.6.1.4.1.674.10893.1.20.130.13.1.2'  => 'enclosureManagementModuleName',
	     '1.3.6.1.4.1.674.10893.1.20.130.13.1.4'  => 'enclosureManagementModuleState',
	     '1.3.6.1.4.1.674.10893.1.20.130.13.1.11' => 'enclosureManagementModuleComponentStatus',
	     '1.3.6.1.4.1.674.10893.1.20.130.13.1.12' => 'enclosureManagementModuleNexusID',
	     '1.3.6.1.4.1.674.10893.1.20.130.14.1.4'  => 'enclosureManagementModuleConnectionEnclosureName',
	     '1.3.6.1.4.1.674.10893.1.20.130.14.1.5'  => 'enclosureManagementModuleConnectionEnclosureNumber',
	    );
	my $result = undef;
	if ($opt{use_get_table}) {
	    my $enclosureManagementModuleTable = '1.3.6.1.4.1.674.10893.1.20.130.13';
            my $enclosureManagementModuleConnectionTable = '1.3.6.1.4.1.674.10893.1.20.130.14';

	    $result = $snmp_session->get_table(-baseoid => $enclosureManagementModuleTable);
            my $ext = $snmp_session->get_table(-baseoid => $enclosureManagementModuleConnectionTable);

	    if (defined $result) {
                defined $ext && map { $$result{$_} = $$ext{$_} } keys %{ $ext };
            }
	}
	else {
	    $result = $snmp_session->get_entries(-columns => [keys %emms_oid]);
	}

	# No enclosure EMMs is OK
	return if !defined $result;

	@output = @{ get_snmp_output($result, \%emms_oid) };
    }
    else {
	foreach my $enc (@enclosures) {
	    push @output, @{ run_omreport("storage enclosure controller=$enc->{ctrl} enclosure=$enc->{id} info=emms") };
	    map_item('ctrl', $enc->{ctrl}, \@output);
	    map_item('encl_id', $enc->{id}, \@output);
	    map_item('encl_name', $enc->{name}, \@output);
	}
    }

    my %emms_state
      = (
	 0  => 'Unknown',
	 1  => 'Ready',
	 2  => 'Failed',
	 3  => 'Online',
	 4  => 'Offline',
	 5  => 'Not Installed',
	 6  => 'Degraded',
	 21 => 'Missing',
	);

    # Check EMMs on each of the enclosures
  EMM:
    foreach my $out (@output) {
	if ($snmp) {
	    $name      = $out->{enclosureManagementModuleName} || 'Unknown EMM';
	    $state     = get_hashval($out->{enclosureManagementModuleState}, \%emms_state);
	    $status    = get_snmp_status($out->{enclosureManagementModuleComponentStatus});
	    $encl_id   = $snmp_enclosure{$out->{enclosureManagementModuleConnectionEnclosureNumber}}{nexus};
	    $encl_name = $out->{enclosureManagementModuleConnectionEnclosureName} || 'Unknown enclosure';
	    $nexus     = convert_nexus(($out->{enclosureManagementModuleNexusID} || 9999));
	}
	else {
	    $name      = get_nonempty_string('Name', $out, 'Unknown EMM');
	    $state     = get_nonempty_string('State', $out, 'Unknown state');
	    $status    = get_nonempty_string('Status', $out, 'Unknown');
	    $encl_id   = join q{:}, $out->{ctrl}, $out->{'encl_id'};
	    $encl_name = $out->{encl_name};
	    $nexus     = join q{:}, $out->{ctrl}, $out->{'encl_id'}, get_nonempty_string('ID', $out, '9999');
	}

	next EMM if blacklisted('encl_emm', $nexus);

	# Not installed
	if ($status =~ m{\A Other|Unknown \z}xms and $state eq 'Not Installed') {
	    my $msg = sprintf '%s in enclosure %s [%s] is %s',
	      $name, $encl_id, $encl_name, $state;
	    report('storage', $msg, $E_OK, $nexus);
	}
	# Default
	elsif ($status ne 'Ok') {
	    my $msg = sprintf '%s in enclosure %s [%s] needs attention: %s',
	      $name, $encl_id, $encl_name, $state;
	    report('storage', $msg, $status2nagios{$status}, $nexus);
	}
	# Ok
	else {
	    my $msg = sprintf '%s in enclosure %s [%s] is %s',
	      $name, $encl_id, $encl_name, $state;
	    report('storage', $msg, $E_OK, $nexus);
	}
    }
    return;
}


#-----------------------------------------
# CHASSIS: Check memory modules
#-----------------------------------------
sub check_memory {
    return if blacklisted('dimm', 'all');

    my $index    = undef;
    my $status   = undef;
    my $location = undef;
    my $size     = undef;
    my $modes    = undef;
    my @failures = ();
    my @output   = ();

    if ($snmp) {
	my %dimm_oid
	  = (
	     '1.3.6.1.4.1.674.10892.1.1100.50.1.2.1'  => 'memoryDeviceIndex',
	     '1.3.6.1.4.1.674.10892.1.1100.50.1.5.1'  => 'memoryDeviceStatus',
	     '1.3.6.1.4.1.674.10892.1.1100.50.1.8.1'  => 'memoryDeviceLocationName',
	     '1.3.6.1.4.1.674.10892.1.1100.50.1.14.1' => 'memoryDeviceSize',
	     '1.3.6.1.4.1.674.10892.1.1100.50.1.20.1' => 'memoryDeviceFailureModes',
	    );
	my $result = undef;
	if ($opt{use_get_table}) {
	    my $memoryDeviceTable = '1.3.6.1.4.1.674.10892.1.1100.50.1';
	    $result = $snmp_session->get_table(-baseoid => $memoryDeviceTable);
	}
	else {
	    $result = $snmp_session->get_entries(-columns => [keys %dimm_oid]);
	}

	if (!defined $result) {
	    printf "SNMP ERROR [memory]: %s.\n", $snmp_session->error;
	    $snmp_session->close;
	    exit $E_UNKNOWN;
	}

	@output = @{ get_snmp_output($result, \%dimm_oid) };
    }
    else {
	@output = @{ run_omreport("$omopt_chassis memory") };
    }

    # Note: These values are bit masks, so combination values are
    # possible. If value is 0 (zero), memory device has no faults.
    my %failure_mode
      = (
	 1  => 'ECC single bit correction warning rate exceeded',
	 2  => 'ECC single bit correction failure rate exceeded',
	 4  => 'ECC multibit fault encountered',
	 8  => 'ECC single bit correction logging disabled',
	 16 => 'device disabled because of spare activation',
	);

  DIMM:
    foreach my $out (@output) {
	@failures = ();  # Initialize
	if ($snmp) {
	    $index    = ($out->{memoryDeviceIndex} || 10000) - 1;
	    $status   = get_snmp_status($out->{memoryDeviceStatus});
	    $location = $out->{memoryDeviceLocationName} || 'Unknown location';
	    $size     = sprintf '%d MB', ($out->{memoryDeviceSize} || 0)/1024;
	    $modes    = $out->{memoryDeviceFailureModes} || -9999;
	    if ($modes > 0) {
		foreach my $mask (sort keys %failure_mode) {
		    if (($modes & $mask) != 0) { push @failures, $failure_mode{$mask}; }
		}
	    }
	    elsif ($modes == -9999) {
		push @failures, q{ERROR: Failure modes not available via SNMP};
	    }
	}
	else {
	    my $type  = get_nonempty_string('Type', $out, q{});
	    $index    = $type eq '[Not Occupied]' ? undef : get_nonempty_string('Index', $out, 9999);
	    $status   = get_nonempty_string('Status', $out, 'Unknown');
	    $location = get_nonempty_string('Connector Name', $out, 'Unknown location');
	    $size     = get_nonempty_string('Size', $out, 0);
	    if (defined $size) {
		$size =~ s{\s\s}{ }gxms;
	    }
	    # Run 'omreport chassis memory index=X' to get the failures
	    if ($status ne 'Ok' && defined $index) {
		foreach (@{ run_command("$omreport $omopt_chassis memory index=$index -fmt ssv") }) {
		    if (m/\A Failures; (.+?) \z/xms) {
			chop(my $fail = $1);
			push @failures, split m{\.}xms, $fail;
		    }
		}
	    }
	}
	$location =~ s{\A \s*(.*?)\s* \z}{$1}xms;

	# calculate total memory
	my $msize = defined $size ? $size : 0;
	$msize =~ s{\A (\d+) \s MB}{$1}xms;
	$count{mem} += $msize;

	next DIMM if blacklisted('dimm', $index);

	# Ignore empty memory slots
	next DIMM if !defined $index;
	$count{dimm}++;

	if ($status ne 'Ok') {
	    my $msg = undef;
	    if (scalar @failures == 0) {
		$msg = sprintf 'Memory module %d [%s, %s] needs attention (%s)',
		  $index, $location, $size, $status;
	    }
	    else {
		$msg = sprintf 'Memory module %d [%s, %s] needs attention: %s',
		  $index, $location, $size, (join q{, }, @failures);
	    }

	    report('chassis', $msg, $status2nagios{$status}, $index);
	}
	# Ok
	else {
	    my $msg = sprintf 'Memory module %d [%s, %s] is %s',
	      $index, $location, $size, $status;
	    report('chassis', $msg, $E_OK, $index);
	}
    }
    return;
}


#-----------------------------------------
# CHASSIS: Check fans
#-----------------------------------------
sub check_fans {
    return if blacklisted('fan', 'all');

    my $index    = undef;
    my $status   = undef;
    my $reading  = undef;
    my $location = undef;
    my $max_crit = undef;
    my $max_warn = undef;
    my @output   = ();

    if ($snmp) {
	my %cool_oid
	  = (
	     '1.3.6.1.4.1.674.10892.1.700.12.1.2.1'  => 'coolingDeviceIndex',
	     '1.3.6.1.4.1.674.10892.1.700.12.1.5.1'  => 'coolingDeviceStatus',
	     '1.3.6.1.4.1.674.10892.1.700.12.1.6.1'  => 'coolingDeviceReading',
	     '1.3.6.1.4.1.674.10892.1.700.12.1.8.1'  => 'coolingDeviceLocationName',
	     '1.3.6.1.4.1.674.10892.1.700.12.1.10.1' => 'coolingDeviceUpperCriticalThreshold',
	     '1.3.6.1.4.1.674.10892.1.700.12.1.11.1' => 'coolingDeviceUpperNonCriticalThreshold',
	    );
	my $result = undef;
	if ($opt{use_get_table}) {
	    my $coolingDeviceTable = '1.3.6.1.4.1.674.10892.1.700.12.1';
	    $result = $snmp_session->get_table(-baseoid => $coolingDeviceTable);
	}
	else {
	    $result = $snmp_session->get_entries(-columns => [keys %cool_oid]);
	}

	if ($blade && !defined $result) {
	    return 0;
	}
	elsif (!$blade && !defined $result) {
	    printf "SNMP ERROR [cooling]: %s.\n", $snmp_session->error;
	    $snmp_session->close;
	    exit $E_UNKNOWN;
	}

	@output = @{ get_snmp_output($result, \%cool_oid) };
    }
    else {
	@output = @{ run_omreport("$omopt_chassis fans") };
    }

  FAN:
    foreach my $out (@output) {
	if ($snmp) {
	    $index    = ($out->{coolingDeviceIndex} || 10000) - 1;
	    $status   = get_snmp_probestatus($out->{coolingDeviceStatus});
	    $reading  = $out->{coolingDeviceReading} || 0;
	    $location = $out->{coolingDeviceLocationName} || 'Unknown location';
	    $max_crit = $out->{coolingDeviceUpperCriticalThreshold} || 0;
	    $max_warn = $out->{coolingDeviceUpperNonCriticalThreshold} || 0;
	}
	else {
	    $index    = get_nonempty_string('Index', $out, 9999);
	    $status   = get_nonempty_string('Status', $out, 'Unknown');
	    $reading  = get_nonempty_string('Reading', $out, 0);
	    $location = get_nonempty_string('Probe Name', $out, 'Unknown location');
	    $max_crit = get_nonempty_string('Maximum Failure Threshold', $out, 0);
	    $max_warn = get_nonempty_string('Maximum Warning Threshold', $out, 0);
	    if ($max_crit eq '[N/A]') { $max_crit = 0; }
	    if ($max_warn eq '[N/A]') { $max_warn = 0; }
	    $reading  =~ s{\A (\d+).* \z}{$1}xms;
	    $max_warn =~ s{\A (\d+).* \z}{$1}xms;
	    $max_crit =~ s{\A (\d+).* \z}{$1}xms;
	}

	next FAN if blacklisted('fan', $index);
	$count{fan}++;

	if ($status ne 'Ok') {
	    my $msg = sprintf 'Chassis fan %d [%s] needs attention: %s',
	      $index, $location, $status;
	    my $err = $snmp ? $probestatus2nagios{$status} : $status2nagios{$status};
	    report('chassis', $msg, $err, $index);
	}
	else {
	    my $msg = sprintf 'Chassis fan %d [%s]: %s',
	      $index, $location, $reading;
	    report('chassis', $msg, $E_OK, $index);
	}

	# Collect performance data
	if (defined $opt{perfdata}) {
	    my $pname = lc $location;
	    $pname =~ s{\s}{_}gxms;
	    $pname =~ s{proc_}{cpu#}xms;
	    push @perfdata, {
			     label => "fan_${index}_${pname}",
			     mini  => "f$index",
			     value => $reading,
			     warn  => $max_warn,
			     crit  => $max_crit,
			    };
	}
    }
    return;
}


#-----------------------------------------
# CHASSIS: Check power supplies
#-----------------------------------------
sub check_powersupplies {
    return if blacklisted('ps', 'all');

    my $index    = undef;
    my $status   = undef;
    my $type     = undef;
    my $err_type = undef;
    my $state    = undef;
    my @states   = ();
    my @output   = ();

    if ($snmp) {
	my %ps_oid
	  = (
	     '1.3.6.1.4.1.674.10892.1.600.12.1.2.1'  => 'powerSupplyIndex',
	     '1.3.6.1.4.1.674.10892.1.600.12.1.5.1'  => 'powerSupplyStatus',
	     '1.3.6.1.4.1.674.10892.1.600.12.1.7.1'  => 'powerSupplyType',
	     '1.3.6.1.4.1.674.10892.1.600.12.1.11.1' => 'powerSupplySensorState',
	     '1.3.6.1.4.1.674.10892.1.600.12.1.12.1' => 'powerSupplyConfigurationErrorType',
	    );
	my $result = undef;
	if ($opt{use_get_table}) {
	    my $powerDeviceTable = '1.3.6.1.4.1.674.10892.1.600.12.1';
	    $result = $snmp_session->get_table(-baseoid => $powerDeviceTable);
	}
	else {
	    $result = $snmp_session->get_entries(-columns => [keys %ps_oid]);
	}

	# No instrumented PSU is OK (blades, low-end servers)
	return 0 if !defined $result;

	@output = @{ get_snmp_output($result, \%ps_oid) };
    }
    else {
	@output = @{ run_omreport("$omopt_chassis pwrsupplies") };
    }

    my %ps_type
      = (
	 1  => 'Other',
	 2  => 'Unknown',
	 3  => 'Linear',
	 4  => 'Switching',
	 5  => 'Battery',
	 6  => 'Uninterruptible Power Supply',
	 7  => 'Converter',
	 8  => 'Regulator',
	 9  => 'AC',
	 10 => 'DC',
	 11 => 'VRM',
	);

    my %ps_state
      = (
	 1  => 'Presence detected',
	 2  => 'Failure detected',
	 4  => 'Predictive Failure',
	 8  => 'AC lost',
	 16 => 'AC lost or out-of-range',
	 32 => 'AC out-of-range but present',
	 64 => 'Configuration error',
	);

    my %ps_config_error_type
      = (
	 1 => 'Vendor mismatch',
	 2 => 'Revision mismatch',
	 3 => 'Processor missing',
	);

  PS:
    foreach my $out (@output) {
	if ($snmp) {
	    @states = ();  # contains states for the PS

	    $index    = ($out->{powerSupplyIndex} || 10000) - 1;
	    $status   = get_snmp_status($out->{powerSupplyStatus});
	    $type     = get_hashval($out->{powerSupplyType}, \%ps_type);
	    $err_type = defined $out->{powerSupplyConfigurationErrorType}
	      ? get_hashval($out->{powerSupplyConfigurationErrorType}, \%ps_config_error_type) : undef;

	    # get the combined state from the StatusReading OID
	    my $raw_state = $out->{powerSupplySensorState} || 0;
	    foreach my $mask (sort keys %ps_state) {
		if (($raw_state & $mask) != 0) {
		    push @states, $ps_state{$mask};
		}
	    }

	    # If configuration error, also include the error type
	    if (defined $err_type) {
		push @states, $err_type;
	    }

	    # Finally, construct the state string
	    $state = join q{, }, @states;
	}
	else {
	    $index  = get_nonempty_string('Index', $out, 9999);
	    $status = get_nonempty_string('Status', $out, 'Unknown');
	    $type   = get_nonempty_string('Type', $out, 'Unknown type');
	    $state  = get_nonempty_string('Online Status', $out, 'Unknown state');
	}

	next PS if blacklisted('ps', $index);
	$count{power}++;

	if ($status ne 'Ok') {
	    my $msg = sprintf 'Power Supply %d [%s] needs attention: %s',
	      $index, $type, $state;
	    report('chassis', $msg, $status2nagios{$status}, $index);
	}
	else {
	    my $msg = sprintf 'Power Supply %d [%s]: %s',
	      $index, $type, $state;
	    report('chassis', $msg, $E_OK, $index);
	}
    }
    return;
}


#-----------------------------------------
# CHASSIS: Check temperatures
#-----------------------------------------
sub check_temperatures {
    return if blacklisted('temp', 'all');

    my $index    = undef;
    my $status   = undef;
    my $reading  = undef;
    my $location = undef;
    my $max_crit = undef;
    my $max_warn = undef;
    my $min_warn = undef;
    my $min_crit = undef;
    my $type     = undef;
    my $discrete = undef;
    my @output = ();

    # Getting custom temperature thresholds (user option)
    my %warn_threshold = %{ custom_temperature_thresholds('w') };
    my %crit_threshold = %{ custom_temperature_thresholds('c') };

    if ($snmp) {
	my %temp_oid
	  = (
	     '1.3.6.1.4.1.674.10892.1.700.20.1.2.1'  => 'temperatureProbeIndex',
	     '1.3.6.1.4.1.674.10892.1.700.20.1.5.1'  => 'temperatureProbeStatus',
	     '1.3.6.1.4.1.674.10892.1.700.20.1.6.1'  => 'temperatureProbeReading',
	     '1.3.6.1.4.1.674.10892.1.700.20.1.7.1'  => 'temperatureProbeType',
	     '1.3.6.1.4.1.674.10892.1.700.20.1.8.1'  => 'temperatureProbeLocationName',
	     '1.3.6.1.4.1.674.10892.1.700.20.1.10.1' => 'temperatureProbeUpperCriticalThreshold',
	     '1.3.6.1.4.1.674.10892.1.700.20.1.11.1' => 'temperatureProbeUpperNonCriticalThreshold',
	     '1.3.6.1.4.1.674.10892.1.700.20.1.12.1' => 'temperatureProbeLowerNonCriticalThreshold',
	     '1.3.6.1.4.1.674.10892.1.700.20.1.13.1' => 'temperatureProbeLowerCriticalThreshold',
	     '1.3.6.1.4.1.674.10892.1.700.20.1.16.1' => 'temperatureProbeDiscreteReading',
	    );
	# this didn't work well for some reason
	#my $result = $snmp_session->get_entries(-columns => [keys %temp_oid]);

	# Getting values using the table
	my $temperatureProbeTable = '1.3.6.1.4.1.674.10892.1.700.20';
	my $result = $snmp_session->get_table(-baseoid => $temperatureProbeTable);

	if (!defined $result) {
	    printf "SNMP ERROR [temperatures]: %s.\n", $snmp_session->error;
	    $snmp_session->close;
	    exit $E_UNKNOWN;
	}

	@output = @{ get_snmp_output($result, \%temp_oid) };
    }
    else {
	@output = @{ run_omreport("$omopt_chassis temps") };
    }

    my %probe_type
      = (
	 1  => 'Other',      # type is other than following values
	 2  => 'Unknown',    # type is unknown
	 3  => 'AmbientESM', # type is Ambient Embedded Systems Management temperature probe
	 16 => 'Discrete',   # type is temperature probe with discrete reading
	);

  TEMP:
    foreach my $out (@output) {
	if ($snmp) {
	    $index    = ($out->{temperatureProbeIndex} || 10000) - 1;
	    $status   = get_snmp_probestatus($out->{temperatureProbeStatus});
	    $location = $out->{temperatureProbeLocationName} || 'Unknown location';
	    $type     = get_hashval($out->{temperatureProbeType}, \%probe_type);
	    $reading  = $out->{temperatureProbeReading} || '[N/A]';
	    $max_crit = $out->{temperatureProbeUpperCriticalThreshold} || '[N/A]';
	    $max_warn = $out->{temperatureProbeUpperNonCriticalThreshold} || '[N/A]';
	    $min_crit = $out->{temperatureProbeLowerCriticalThreshold} || '[N/A]';
	    $min_warn = $out->{temperatureProbeLowerNonCriticalThreshold} || '[N/A]';
	    $discrete = $out->{temperatureProbeDiscreteReading} || '[N/A]';

	    # If numeric values, i.e. not discrete
	    $reading  /= 10 if $reading  =~ m{\A \d+ \z}xms;
	    $max_crit /= 10 if $max_crit =~ m{\A \d+ \z}xms;
	    $max_warn /= 10 if $max_warn =~ m{\A \d+ \z}xms;
	    $min_crit /= 10 if $min_crit =~ m{\A \d+ \z}xms;
	    $min_warn /= 10 if $min_warn =~ m{\A \d+ \z}xms;

	    # workaround for bad temp probes
	    if ($type eq 'AmbientESM' and $reading !~ m{\A \d+(\.\d+)? \z}xms) {
		$type = 'Discrete';
	    }
	}
	else {
	    $index    = get_nonempty_string('Index', $out, 9999);
	    $status   = get_nonempty_string('Status', $out, 'Unknown');
	    $location = get_nonempty_string('Probe Name', $out, 'Unknown location');
	    $reading  = get_nonempty_string('Reading', $out, '[N/A]');
	    $max_crit = get_nonempty_string('Maximum Failure Threshold', $out, '[N/A]');
	    $max_warn = get_nonempty_string('Maximum Warning Threshold', $out, '[N/A]');
	    $min_crit = get_nonempty_string('Minimum Failure Threshold', $out, '[N/A]');
	    $min_warn = get_nonempty_string('Minimum Warning Threshold', $out, '[N/A]');

	    # Cleaning the temp readings
	    $reading =~ s{\.0\s+C}{}xms;
	    $max_crit =~ s{\.0\s+C}{}xms;
	    $max_warn =~ s{\.0\s+C}{}xms;
	    $min_crit =~ s{\.0\s+C}{}xms;
	    $min_warn =~ s{\.0\s+C}{}xms;

	    $type     = $reading =~ m{\A\d+\z}xms ? 'AmbientESM' : 'Discrete';
	    $discrete = $reading;
	}

	next TEMP if blacklisted('temp', $index);
	$count{temp}++;

	if ($type eq 'Discrete') {
	    my $msg = sprintf 'Temperature probe %d [%s] is %s',
	      $index, $location, $discrete;
	    my $err = $snmp ? $probestatus2nagios{$status} : $status2nagios{$status};
	    report('chassis', $msg, $err, $index);
	}
	else {
	    # First check according to custom thresholds
	    if (exists $crit_threshold{$index}{max} and $reading > $crit_threshold{$index}{max}) {
		# Custom critical MAX
		my $msg = sprintf 'Temperature Probe %d [%s] reads %d C (custom max=%d)',
		  $index, $location, $reading, $crit_threshold{$index}{max};
		report('chassis', $msg, $E_CRITICAL, $index);
	    }
	    elsif (exists $warn_threshold{$index}{max} and $reading > $warn_threshold{$index}{max}) {
		# Custom warning MAX
		my $msg = sprintf 'Temperature Probe %d [%s] reads %d C (custom max=%d)',
		  $index, $location, $reading, $warn_threshold{$index}{max};
		report('chassis', $msg, $E_WARNING, $index);
	    }
	    elsif (exists $crit_threshold{$index}{min} and $reading < $crit_threshold{$index}{min}) {
		# Custom critical MIN
		my $msg = sprintf 'Temperature Probe %d [%s] reads %d C (custom min=%d)',
		  $index, $location, $reading, $crit_threshold{$index}{min};
		report('chassis', $msg, $E_CRITICAL, $index);
	    }
	    elsif (exists $warn_threshold{$index}{min} and $reading < $warn_threshold{$index}{min}) {
		# Custom warning MIN
		my $msg = sprintf 'Temperature Probe %d [%s] reads %d C (custom min=%d)',
		  $index, $location, $reading, $warn_threshold{$index}{min};
		report('chassis', $msg, $E_WARNING, $index);
	    }
	    elsif ($status ne 'Ok' and $max_crit ne '[N/A]' and $reading > $max_crit) {
		my $msg = sprintf 'Temperature Probe %d [%s] is critically high at %d C',
		  $index, $location, $reading;
		my $err = $snmp ? $probestatus2nagios{$status} : $status2nagios{$status};
		report('chassis', $msg, $err, $index);
	    }
	    elsif ($status ne 'Ok' and $max_warn ne '[N/A]' and $reading > $max_warn) {
		my $msg = sprintf 'Temperature Probe %d [%s] is too high at %d C',
		  $index, $location, $reading;
		my $err = $snmp ? $probestatus2nagios{$status} : $status2nagios{$status};
		report('chassis', $msg, $err, $index);
	    }
	    elsif ($status ne 'Ok' and $min_crit ne '[N/A]' and $reading < $min_crit) {
		my $msg = sprintf 'Temperature Probe %d [%s] is critically low at %d C',
		  $index, $location, $reading;
		my $err = $snmp ? $probestatus2nagios{$status} : $status2nagios{$status};
		report('chassis', $msg, $err, $index);
	    }
	    elsif ($status ne 'Ok' and $min_warn ne '[N/A]' and $reading < $min_warn) {
		my $msg = sprintf 'Temperature Probe %d [%s] is too low at %d C',
		  $index, $location, $reading;
		my $err = $snmp ? $probestatus2nagios{$status} : $status2nagios{$status};
		report('chassis', $msg, $err, $index);
	    }
	    # Ok
	    else {
		my $msg = sprintf 'Temperature Probe %d [%s] reads %d C',
		  $index, $location, $reading;
		if ($min_warn eq '[N/A]' and $min_crit eq '[N/A]') {
		    $msg .= sprintf ' (max=%s/%s)', $max_warn, $max_crit;
		}
		else {
		    $msg .= sprintf ' (min=%s/%s, max=%s/%s)',
		      $min_warn, $min_crit, $max_warn, $max_crit;
		}
		my $err = $snmp ? $probestatus2nagios{$status} : $status2nagios{$status};
		report('chassis', $msg, $err, $index);
	    }

	    # Collect performance data
	    if (defined $opt{perfdata}) {
		my $pname = lc $location;
		$pname =~ s{\s}{_}gxms;
		$pname =~ s{_temp\z}{}xms;
		$pname =~ s{proc_}{cpu#}xms;
		push @perfdata, {
				 label => "temp_${index}_${pname}",
				 mini  => "t$index",
				 value => $reading,
				 warn  => $max_warn,
				 crit  => $max_crit,
				};
	    }
	}
    }
    return;
}


#-----------------------------------------
# CHASSIS: Check processors
#-----------------------------------------
sub check_processors {
    return if blacklisted('cpu', 'all');

    my $index   = undef;
    my $status  = undef;
    my $state   = undef;
    my $brand   = undef;
    my $family  = undef;
    my $man     = undef;
    my $speed   = undef;
    my @output = ();

    if ($snmp) {

	# NOTE: For some reason, older models don't have the
	# "Processor Device Status" OIDs. We check both the newer
	# (preferred) OIDs and the old ones.

	my %cpu_oid
	  = (
             '1.3.6.1.4.1.674.10892.1.1100.30.1.2.1'  => 'processorDeviceIndex',
             '1.3.6.1.4.1.674.10892.1.1100.30.1.5.1'  => 'processorDeviceStatus',
             '1.3.6.1.4.1.674.10892.1.1100.30.1.8.1'  => 'processorDeviceManufacturerName',
             '1.3.6.1.4.1.674.10892.1.1100.30.1.9.1'  => 'processorDeviceStatusState',
             '1.3.6.1.4.1.674.10892.1.1100.30.1.10.1' => 'processorDeviceFamily',
             '1.3.6.1.4.1.674.10892.1.1100.30.1.12.1' => 'processorDeviceCurrentSpeed',
             '1.3.6.1.4.1.674.10892.1.1100.30.1.23.1' => 'processorDeviceBrandName',
	     '1.3.6.1.4.1.674.10892.1.1100.32.1.2.1'  => 'processorDeviceStatusIndex',
	     '1.3.6.1.4.1.674.10892.1.1100.32.1.5.1'  => 'processorDeviceStatusStatus',
	     '1.3.6.1.4.1.674.10892.1.1100.32.1.6.1'  => 'processorDeviceStatusReading',
	    );
	my $result = undef;
	if ($opt{use_get_table}) {
	    my $processorDeviceTable = '1.3.6.1.4.1.674.10892.1.1100.30.1';
	    my $processorDeviceStatusTable = '1.3.6.1.4.1.674.10892.1.1100.32.1';

	    $result = $snmp_session->get_table(-baseoid => $processorDeviceTable);
	    my $ext = $snmp_session->get_table(-baseoid => $processorDeviceStatusTable);

            defined $ext && map { $$result{$_} = $$ext{$_} } keys %{ $ext };
	}
	else {
	    $result = $snmp_session->get_entries(-columns => [keys %cpu_oid]);
	}

	if (!defined $result) {
	    printf "SNMP ERROR [processors]: %s.\n", $snmp_session->error;
	    $snmp_session->close;
	    exit $E_UNKNOWN;
	}

	@output = @{ get_snmp_output($result, \%cpu_oid) };
    }
    else {
	@output = @{ run_omreport("$omopt_chassis processors") };
    }

    my %cpu_state
      = (
         1 => 'Other',         # other than following values
         2 => 'Unknown',       # unknown
         3 => 'Enabled',       # enabled
         4 => 'User Disabled', # disabled by user via BIOS setup
         5 => 'BIOS Disabled', # disabled by BIOS (POST error)
         6 => 'Idle',          # idle
        );

    my %cpu_reading
      = (
	 1    => 'Internal Error',      # Internal Error
	 2    => 'Thermal Trip',        # Thermal Trip
	 32   => 'Configuration Error', # Configuration Error
	 128  => 'Present',             # Processor Present
	 256  => 'Disabled',            # Processor Disabled
	 512  => 'Terminator Present',  # Terminator Present
	 1024 => 'Throttled',           # Processor Throttled
	);

    # Mapping between family numbers from SNMP and actual CPU family
    my %cpu_family
      = (
	 1   => 'Other',                                2   => 'Unknown',
         3   => '8086',                                 4   => '80286',
         5   => '386',                                  6   => '486',
	 7   => '8087',                                 8   => '80287',
         9   => '80387',                                10  => '80487',
         11  => 'Pentium',                              12  => 'Pentium Pro',
	 13  => 'Pentium II',                           14  => 'Pentium with MMX',
         15  => 'Celeron',                              16  => 'Pentium II Xeon',
         17  => 'Pentium III',                          18  => 'Pentium III Xeon',
	 19  => 'Pentium III',                          20  => 'Itanium',
         21  => 'Xeon',                                 22  => 'Pentium 4',
         23  => 'Xeon MP',                              24  => 'Itanium 2',
	 25  => 'K5',                                   26  => 'K6',
         27  => 'K6-2',                                 28  => 'K6-3',
         29  => 'Athlon',                               30  => 'AMD2900',
	 31  => 'K6-2+',                                32  => 'Power PC',
         33  => 'Power PC 601',                         34  => 'Power PC 603',
         35  => 'Power PC 603+',                        36  => 'Power PC 604',
	 37  => 'Power PC 620',                         38  => 'Power PC x704',
         39  => 'Power PC 750',                         40  => 'Core Duo',
         41  => 'Core Duo mobile',                      42  => 'Core Solo mobile',
         43  => 'Intel Atom',                           44  => undef,
         45  => undef,                                  46  => undef,
         47  => undef,                                  48  => 'Alpha',
         49  => 'Alpha 21064',                          50  => 'Alpha 21066',
	 51  => 'Alpha 21164',                          52  => 'Alpha 21164PC',
         53  => 'Alpha 21164a',                         54  => 'Alpha 21264',
         55  => 'Alpha 21364',                          56  => 'Turion II Ultra Dual-Core Mobile M',
         57  => 'Turion II Dual-Core Mobile M',         58  => 'Athlon II Dual-Core Mobile M ',
         59  => 'Opteron 6100',                         60  => 'Opteron 4100',
         61  => undef,                                  62  => undef,
         63  => undef,                                  64  => 'MIPS',
	 65  => 'MIPS R4000',                           66  => 'MIPS R4200',
         67  => 'MIPS R4400',                           68  => 'MIPS R4600',
         69  => 'MIPS R10000',                          70  => undef,
         71  => undef,                                  72  => undef,
         73  => undef,                                  74  => undef,
         75  => undef,                                  76  => undef,
         77  => undef,                                  78  => undef,
         79  => undef,                                  80  => 'SPARC',
	 81  => 'SuperSPARC',                           82  => 'microSPARC II',
         83  => 'microSPARC IIep',                      84  => 'UltraSPARC',
         85  => 'UltraSPARC II',                        86  => 'UltraSPARC IIi',
	 87  => 'UltraSPARC III',                       88  => 'UltraSPARC IIIi',
         89  => undef,                                  90  => undef,
         91  => undef,                                  92  => undef,
         93  => undef,                                  94  => undef,
         95  => undef,                                  96  => '68040',
         97  => '68xxx',                                98  => '68000',
         99  => '68010',                                100 => '68020',
         101 => '68030',                                102 => undef,
         103 => undef,                                  104 => undef,
         105 => undef,                                  106 => undef,
         107 => undef,                                  108 => undef,
         109 => undef,                                  110 => undef,
         111 => undef,                                  112 => 'Hobbit',
         113 => undef,                                  114 => undef,
         115 => undef,                                  116 => undef,
         117 => undef,                                  118 => undef,
         119 => undef,                                  120 => 'Crusoe TM5000',
         121 => 'Crusoe TM3000',                        122 => 'Efficeon TM8000',
         123 => undef,                                  124 => undef,
         125 => undef,                                  126 => undef,
         127 => undef,                                  128 => 'Weitek',
         129 => undef,                                  130 => 'Celeron M',
         131 => 'Athlon 64',                            132 => 'Opteron',
         133 => 'Sempron',                              134 => 'Turion 64 Mobile',
         135 => 'Dual-Core Opteron',                    136 => 'Athlon 64 X2 DC',
         137 => 'Turion 64 X2 M',                       138 => 'Quad-Core Opteron',
         139 => '3rd gen Opteron',                      140 => 'AMD Phenom FX Quad-Core',
         141 => 'AMD Phenom X4 Quad-Core',              142 => 'AMD Phenom X2 Dual-Core',
         143 => 'AMD Athlon X2 Dual-Core',              144 => 'PA-RISC',
         145 => 'PA-RISC 8500',	                        146 => 'PA-RISC 8000',
         147 => 'PA-RISC 7300LC',                       148 => 'PA-RISC 7200',
         149 => 'PA-RISC 7100LC',                       150 => 'PA-RISC 7100',
         151 => undef,                                  152 => undef,
         153 => undef,                                  154 => undef,
         155 => undef,                                  156 => undef,
         157 => undef,                                  158 => undef,
         159 => undef,                                  160 => 'V30',
         161 => 'Quad-Core Xeon 3200',                  162 => 'Dual-Core Xeon 3000',
         163 => 'Quad-Core Xeon 5300',                  164 => 'Dual-Core Xeon 5100',
         165 => 'Dual-Core Xeon 5000',                  166 => 'Dual-Core Xeon LV',
         167 => 'Dual-Core Xeon ULV',                   168 => 'Dual-Core Xeon 7100',
         169 => 'Quad-Core Xeon 5400',                  170 => 'Quad-Core Xeon',
         171 => 'Dual-Core Xeon 5200',                  172 => 'Dual-Core Xeon 7200',
         173 => 'Quad-Core Xeon 7300',                  174 => 'Quad-Core Xeon 7400',
         175 => 'Multi-Core Xeon 7400',                 176 => 'M1',
         177 => 'M2',                                   178 => undef,
         179 => 'Pentium 4 HT',                         180 => 'AS400',
         181 => undef,                                  182 => 'Athlon XP',
	 183 => 'Athlon MP',                            184 => 'Duron',
         185 => 'Pentium M',              	        186 => 'Celeron D',
         187 => 'Pentium D',                            188 => 'Pentium Extreme',
	 189 => 'Core Solo',                            190 => 'Core2',
         191 => 'Core2 Duo',                            192 => 'Core2 Solo',
         193 => 'Core2 Extreme',                        194 => 'Core2 Quad',
         195 => 'Core2 Extreme mobile',                 196 => 'Core2 Duo mobile',
         197 => 'Core2 Solo mobile',                    198 => 'Core i7',
         199 => 'Dual-Core Celeron',                    200 => 'IBM390',
	 201 => 'G4',                                   202 => 'G5',
         203 => 'ESA/390 G6',                           204 => 'z/Architectur',
         205 => 'Core i5',                              206 => 'Core i3',
         207 => undef,                                  208 => undef,
         209 => undef,                                  210 => 'C7-M',
         211 => 'C7-D',                                 212 => 'C7',
         213 => 'Eden',                                 214 => 'Multi-Core Xeon',
         215 => 'Dual-Core Xeon 3xxx',                  216 => 'Quad-Core Xeon 3xxx',
         217 => 'VIA Nano',                             218 => 'Dual-Core Xeon 5xxx',
	 219 => 'Quad-Core Xeon 5xxx',                  220 => undef,
         221 => 'Dual-Core Xeon 7xxx',                  222 => 'Quad-Core Xeon 7xxx',
         223 => 'Multi-Core Xeon 7xxx',                 224 => 'Multi-Core Xeon 3400',
         225 => undef,                                  226 => undef,
         227 => undef,                                  228 => undef,
         229 => undef,                                  230 => 'Embedded AMD Opteron Quad-Core',
         231 => 'AMD Phenom Triple-Core',               232 => 'AMD Turion Ultra Dual-Core Mobile',
         233 => 'AMD Turion Dual-Core Mobile',          234 => 'AMD Athlon Dual-Core',
         235 => 'AMD Sempron SI',                       236 => 'AMD Phenom II',
         237 => 'AMD Athlon II',                        238 => 'Six-Core AMD Opteron',
         239 => 'AMD Sempron M',                        240 => undef,
         241 => undef,                                  242 => undef,
         243 => undef,                                  244 => undef,
         245 => undef,                                  246 => undef,
         247 => undef,                                  248 => undef,
         249 => undef,                                  250 => 'i860',
         251 => 'i960',
	);

  CPU:
    foreach my $out (@output) {
	if ($snmp) {
	    $index  = exists $out->{processorDeviceStatusIndex}
	      ? ($out->{processorDeviceStatusIndex} || 10000) - 1
		: ($out->{processorDeviceIndex} || 10000) - 1;
	    $status = exists $out->{processorDeviceStatusStatus}
	      ? get_snmp_status($out->{processorDeviceStatusStatus})
		: get_snmp_status($out->{processorDeviceStatus});
	    if (defined $out->{processorDeviceStatusReading}) {
		my @states  = ();  # contains states for the CPU

		# get the combined state from the StatusReading OID
		foreach my $mask (sort keys %cpu_reading) {
		    if (($out->{processorDeviceStatusReading} & $mask) != 0) {
			push @states, $cpu_reading{$mask};
		    }
		}

		# Finally, create the state string
		$state = join q{, }, @states;
	    }
	    else {
		$state  = get_hashval($out->{processorDeviceStatusState}, \%cpu_state);
	    }
	    $man    = $out->{processorDeviceManufacturerName} || undef;
	    $family = (defined $out->{processorDeviceFamily}
		       and defined $cpu_family{$out->{processorDeviceFamily}})
	      ? $cpu_family{$out->{processorDeviceFamily}} : undef;
	    $speed  = $out->{processorDeviceCurrentSpeed} || undef;
	    $brand  = $out->{processorDeviceBrandName} || undef;
	}
	else {
	    $index  = get_nonempty_string('Index', $out, 9999);
	    $status = get_nonempty_string('Status', $out, 'Unknown');
	    $state  = get_nonempty_string('State', $out, 'Unknown state');
	    $brand  = get_nonempty_string('Processor Brand', $out, undef);
	    $family = get_nonempty_string('Processor Family',  $out, undef);
	    $man    = get_nonempty_string('Processor Manufacturer', $out, undef);
	    $speed  = get_nonempty_string('Current Speed', $out, undef);
	}

	next CPU if blacklisted('cpu', $index);

	# Ignore unoccupied CPU slots (omreport)
	next CPU if (defined $out->{'Processor Manufacturer'}
		     and $out->{'Processor Manufacturer'} eq '[Not Occupied]')
	  or (defined $out->{'Processor Brand'} and $out->{'Processor Brand'} eq '[Not Occupied]');

	# Ignore unoccupied CPU slots (snmp)
	if ($snmp and defined $out->{processorDeviceStatusReading}
	    and $out->{processorDeviceStatusReading} == 0) {
	    next CPU;
	}

	$count{cpu}++;

	if (defined $brand) {
	    $brand =~ s{\s\s+}{ }gxms;
	    $brand =~ s{\((R|tm)\)}{}gxms;
	    $brand =~ s{\s(CPU|Processor)}{}xms;
	    $brand =~ s{\s\@}{}xms;
	}
	elsif (defined $family and defined $man and defined $speed) {
	    $speed =~ s{\A (\d+) .*}{$1}xms;
	    $brand = sprintf '%s %s %.2fGHz', $man, $family, $speed / 1000;
	}
	else {
	    $brand = "unknown";
	}

	# Default
	if ($status ne 'Ok') {
	    my $msg = sprintf 'Processor %d [%s] needs attention: %s',
	      $index, $brand, $state;
	    report('chassis', $msg, $status2nagios{$status}, $index);
	}
	# Ok
	else {
	    my $msg = sprintf 'Processor %d [%s] is %s',
	      $index, $brand, $state;
	    report('chassis', $msg, $E_OK, $index);
	}
    }
    return;
}


#-----------------------------------------
# CHASSIS: Check voltage probes
#-----------------------------------------
sub check_volts {
    return if blacklisted('volt', 'all');

    my $index    = undef;
    my $status   = undef;
    my $reading  = undef;
    my $location = undef;
    my @output = ();

    if ($snmp) {
	my %volt_oid
	  = (
	     '1.3.6.1.4.1.674.10892.1.600.20.1.2.1'  => 'voltageProbeIndex',
	     '1.3.6.1.4.1.674.10892.1.600.20.1.5.1'  => 'voltageProbeStatus',
	     '1.3.6.1.4.1.674.10892.1.600.20.1.6.1'  => 'voltageProbeReading',
	     '1.3.6.1.4.1.674.10892.1.600.20.1.8.1'  => 'voltageProbeLocationName',
	     '1.3.6.1.4.1.674.10892.1.600.20.1.16.1' => 'voltageProbeDiscreteReading',
	    );

	my $voltageProbeTable = '1.3.6.1.4.1.674.10892.1.600.20.1';
        my $result = $snmp_session->get_table(-baseoid => $voltageProbeTable);

	if (!defined $result) {
	    printf "SNMP ERROR [voltage]: %s.\n", $snmp_session->error;
	    $snmp_session->close;
	    exit $E_UNKNOWN;
	}

	@output = @{ get_snmp_output($result, \%volt_oid) };
    }
    else {
	@output = @{ run_omreport("$omopt_chassis volts") };
    }

    my %volt_discrete_reading
      = (
	 1 => 'Good',
	 2 => 'Bad',
	);

  VOLT:
    foreach my $out (@output) {
	if ($snmp) {
	    $index    = ($out->{voltageProbeIndex} || 10000) - 1;
	    $status   = get_snmp_probestatus($out->{voltageProbeStatus});
	    $reading  = defined $out->{voltageProbeReading}
	      ? sprintf('%.3f V', $out->{voltageProbeReading}/1000)
                : get_hashval($out->{voltageProbeDiscreteReading}, \%volt_discrete_reading);
	    $location = $out->{voltageProbeLocationName} || 'Unknown location';
	}
	else {
	    $index    = get_nonempty_string('Index', $out, 9999);
	    $status   = get_nonempty_string('Status', $out, 'Unknown');
	    $reading  = get_nonempty_string('Reading', $out, 'Unknown reading');
	    $location = get_nonempty_string('Probe Name', $out, 'Unknown location');
	}

	next VOLT if blacklisted('volt', $index);
	$count{volt}++;

	my $msg = sprintf 'Voltage sensor %d [%s] is %s',
	  $index, $location, $reading;
	my $err = $snmp ? $probestatus2nagios{$status} : $status2nagios{$status};
	report('chassis', $msg, $err, $index);
    }
    return;
}


#-----------------------------------------
# CHASSIS: Check batteries
#-----------------------------------------
sub check_batteries {
    return if blacklisted('bp', 'all');

    my $index    = undef;
    my $status   = undef;
    my $reading  = undef;
    my $location = undef;
    my @output = ();

    if ($snmp) {
	my %bat_oid
	  = (
	     '1.3.6.1.4.1.674.10892.1.600.50.1.2.1' => 'batteryIndex',
	     '1.3.6.1.4.1.674.10892.1.600.50.1.5.1' => 'batteryStatus',
	     '1.3.6.1.4.1.674.10892.1.600.50.1.6.1' => 'batteryReading',
	     '1.3.6.1.4.1.674.10892.1.600.50.1.7.1' => 'batteryLocationName',
	    );
	my $result = undef;
	if ($opt{use_get_table}) {
	    my $batteryTable = '1.3.6.1.4.1.674.10892.1.600.50.1';
	    $result = $snmp_session->get_table(-baseoid => $batteryTable);
	}
	else {
	    $result = $snmp_session->get_entries(-columns => [keys %bat_oid]);
	}

	# No batteries is OK
	return 0 if !defined $result;

	@output = @{ get_snmp_output($result, \%bat_oid) };
    }
    else {
	@output = @{ run_omreport("$omopt_chassis batteries") };
    }

    my %bat_reading
      = (
	 1 => 'Predictive Failure',
	 2 => 'Failed',
	 4 => 'Presence Detected',
	);

  BATTERY:
    foreach my $out (@output) {
	if ($snmp) {
	    $index    = ($out->{batteryIndex} || 10000) - 1;
	    $status   = get_snmp_status($out->{batteryStatus});
	    $reading  = get_hashval($out->{batteryReading}, \%bat_reading);
	    $location = $out->{batteryLocationName} || 'Unknown location';
	}
	else {
	    $index    = get_nonempty_string('Index', $out, 9999);
	    $status   = get_nonempty_string('Status', $out, 'Unknown');
	    $reading  = get_nonempty_string('Reading', $out, 'Unknown reading');
	    $location = get_nonempty_string('Probe Name', $out, 'Unknown location');
	}

	next BATTERY if blacklisted('bp', $index);
	$count{bat}++;

	my $msg = sprintf 'Battery probe %d [%s] is %s',
	  $index, $location, $reading;
	report('chassis', $msg, $status2nagios{$status}, $index);
    }
    return;
}


#-----------------------------------------
# CHASSIS: Check amperage probes (power monitoring)
#-----------------------------------------
sub check_pwrmonitoring {
    return if blacklisted('amp', 'all');

    my $index    = undef;
    my $status   = undef;
    my $reading  = undef;
    my $location = undef;
    my $max_crit = undef;
    my $max_warn = undef;
    my $unit     = undef;
    my $type     = undef;
    my @output = ();

    if ($snmp) {
	my %amp_oid
	  = (
	     '1.3.6.1.4.1.674.10892.1.600.30.1.2.1'  => 'amperageProbeIndex',
	     '1.3.6.1.4.1.674.10892.1.600.30.1.5.1'  => 'amperageProbeStatus',
	     '1.3.6.1.4.1.674.10892.1.600.30.1.6.1'  => 'amperageProbeReading',
	     '1.3.6.1.4.1.674.10892.1.600.30.1.7.1'  => 'amperageProbeType',
	     '1.3.6.1.4.1.674.10892.1.600.30.1.8.1'  => 'amperageProbeLocationName',
	     '1.3.6.1.4.1.674.10892.1.600.30.1.10.1' => 'amperageProbeUpperCriticalThreshold',
	     '1.3.6.1.4.1.674.10892.1.600.30.1.11.1' => 'amperageProbeUpperNonCriticalThreshold',
	     '1.3.6.1.4.1.674.10892.1.600.30.1.16.1' => 'amperageProbeDiscreteReading',
	    );
	my $result = undef;
	if ($opt{use_get_table}) {
	    my $amperageProbeTable = '1.3.6.1.4.1.674.10892.1.600.30.1';
	    $result = $snmp_session->get_table(-baseoid => $amperageProbeTable);
	}
	else {
	    $result = $snmp_session->get_entries(-columns => [keys %amp_oid]);
	}

	# No pwrmonitoring is OK
	return 0 if !defined $result;

	@output = @{ get_snmp_output($result, \%amp_oid) };
    }
    else {
	@output = @{ run_omreport("$omopt_chassis pwrmonitoring") };
    }

    my %amp_type   # Amperage probe types
      = (
	 1  => 'amperageProbeTypeIsOther',            # other than following values
	 2  => 'amperageProbeTypeIsUnknown',          # unknown
	 3  => 'amperageProbeTypeIs1Point5Volt',      # 1.5 amperage probe
	 4  => 'amperageProbeTypeIs3Point3volt',      # 3.3 amperage probe
	 5  => 'amperageProbeTypeIs5Volt',            # 5 amperage probe
	 6  => 'amperageProbeTypeIsMinus5Volt',       # -5 amperage probe
	 7  => 'amperageProbeTypeIs12Volt',           # 12 amperage probe
	 8  => 'amperageProbeTypeIsMinus12Volt',      # -12 amperage probe
	 9  => 'amperageProbeTypeIsIO',               # I/O probe
	 10 => 'amperageProbeTypeIsCore',             # Core probe
	 11 => 'amperageProbeTypeIsFLEA',             # FLEA (standby) probe
	 12 => 'amperageProbeTypeIsBattery',          # Battery probe
	 13 => 'amperageProbeTypeIsTerminator',       # SCSI Termination probe
	 14 => 'amperageProbeTypeIs2Point5Volt',      # 2.5 amperage probe
	 15 => 'amperageProbeTypeIsGTL',              # GTL (ground termination logic) probe
	 16 => 'amperageProbeTypeIsDiscrete',         # amperage probe with discrete reading
	 23 => 'amperageProbeTypeIsPowerSupplyAmps',  # Power Supply probe with reading in Amps
	 24 => 'amperageProbeTypeIsPowerSupplyWatts', # Power Supply probe with reading in Watts
	 25 => 'amperageProbeTypeIsSystemAmps',       # System probe with reading in Amps
	 26 => 'amperageProbeTypeIsSystemWatts',      # System probe with reading in Watts
	);

    my %amp_discrete
      = (
	 1 => 'Good',
	 2 => 'Bad',
	);

    my %amp_unit
      = (
	 'amperageProbeTypeIsPowerSupplyAmps'  => 'hA',  # tenths of Amps
	 'amperageProbeTypeIsSystemAmps'       => 'hA',  # tenths of Amps
	 'amperageProbeTypeIsPowerSupplyWatts' => 'W',   # Watts
	 'amperageProbeTypeIsSystemWatts'      => 'W',   # Watts
	 'amperageProbeTypeIsDiscrete'         => q{},   # discrete reading, no unit
	);

  AMP:
    foreach my $out (@output) {
	if ($snmp) {
	    $index    = ($out->{amperageProbeIndex} || 10000) - 1;
	    $status   = get_snmp_probestatus($out->{amperageProbeStatus});
	    $type     = get_hashval($out->{amperageProbeType}, \%amp_type);
	    $reading  = $type eq 'amperageProbeTypeIsDiscrete'
	      ? get_hashval($out->{amperageProbeDiscreteReading}, \%amp_discrete)
		: ($out->{amperageProbeReading} || 0);
	    $location = $out->{amperageProbeLocationName} || 'Unknown location';
	    $max_crit = $out->{amperageProbeUpperCriticalThreshold} || 0;
	    $max_warn = $out->{amperageProbeUpperNonCriticalThreshold} || 0;
	    $unit     = exists $amp_unit{$amp_type{$out->{amperageProbeType}}}
	      ? $amp_unit{$amp_type{$out->{amperageProbeType}}} : 'mA';

	    # calculate proper values and set unit for ampere probes
	    if ($unit eq 'hA' and $type ne 'amperageProbeTypeIsDiscrete') {
		$reading  /= 10;
		$max_crit /= 10;
		$max_warn /= 10;
		$unit      = 'A';
	    }
	}
	else {
	    $index    = get_nonempty_string('Index', $out, 9999);
	    $status   = get_nonempty_string('Status', $out, 'Unknown');
	    $reading  = get_nonempty_string('Reading', $out, 'Unknown reading');
	    $location = get_nonempty_string('Probe Name', $out, 'Unknown location');
	    $max_crit = get_nonempty_string('Failure Threshold', $out, 0);
	    $max_warn = get_nonempty_string('Warning Threshold', $out, 0);

	    $max_crit = 0 if $max_crit eq '[N/A]';
	    $max_warn = 0 if $max_warn eq '[N/A]';

	    $reading  =~ s{\A (\d+.*?)\s+([a-zA-Z]+) \s*\z}{$1}xms;
	    $unit     = $2 || 'unknown';
	    $max_warn =~ s{\A (\d+.*?)\s+[a-zA-Z]+ \s*\z}{$1}xms;
	    $max_crit =~ s{\A (\d+.*?)\s+[a-zA-Z]+ \s*\z}{$1}xms;
	}

	next AMP if blacklisted('amp', $index);
	next AMP if $index !~ m{\A \d+ \z}xms;

	# Special case: Probe is present but unknown. This happens via
	# SNMP on some systems where power monitoring capability is
	# disabled due to non-redundant and/or non-instrumented power
	# supplies.
        # E.g. R410 with newer BMC firmware and 1 power supply
	if ($snmp && $status eq 'Unknown' && $reading eq '[N/A]') {
	    next AMP;
	}

	$count{amp}++;

	# Special case: Discrete reading
	if (defined $type and $type eq 'amperageProbeTypeIsDiscrete') {
	    my $msg = sprintf 'Amperage probe %d [%s] is %s',
	      $index, $location, $reading;
	    report('chassis', $msg, $status2nagios{$status}, $index);
	}
	# Default
	else {
	    my $msg = sprintf 'Amperage probe %d [%s] reads %s %s',
	      $index, $location, $reading, $unit;
	    report('chassis', $msg, $status2nagios{$status}, $index);
	}

	# Collect performance data
	if (defined $opt{perfdata}) {
	    next AMP if $reading !~ m{\A \d+(\.\d+)? \z}xms; # discrete reading (not number)
	    my $label = join q{_}, 'pwr_mon', $index, lc $location;
	    $label =~ s{\s}{_}gxms;
	    push @perfdata, {
			     label => $label,
			     mini  => "p${index}" . lc $unit,
			     value => $reading,
			     warn  => $max_warn,
			     crit  => $max_crit,
			    };
	}
    }

    # Collect EXTRA performance data not found at first run. This is a
    # rather ugly hack
    if (defined $opt{perfdata} && !$snmp) {
	my $found = 0;
	my $index = 0;
	my %used  = ();

	# find used indexes
	foreach (@perfdata) {
	    if ($_->{label} =~ m/\A pwr_mon_(\d+)/xms) {
		$used{$1} = 1;
	    }
	}

      AMP2:
	foreach my $line (@{ run_command("$omreport $omopt_chassis pwrmonitoring -fmt ssv") }) {
	    chop $line;
	    if ($line eq 'Location;Reading') {
		$found = 1;
		next AMP2;
	    }
	    if ($line eq q{}) {
		$found = 0;
		next AMP2;
	    }
	    if ($found and $line =~ m/\A ([^;]+?) ; (\d*\.\d+) \s [AW] \z/xms) {
		my $aname = lc $1;
		my $aval = $2;
		$aname =~ s{\s}{_}gxms;

		# don't use an existing index
		while (exists $used{$index}) { ++$index; }

		push @perfdata, {
				 label => "pwr_mon_${index}_${aname}",
				 mini  => "p${index}a",
				 value => $aval,
				 warn  => 0,
				 crit  => 0,
				};
		++$index;
	    }
	}
    }

    return;
}


#-----------------------------------------
# CHASSIS: Check intrusion
#-----------------------------------------
sub check_intrusion {
    return if blacklisted('intr', 'all');

    my $index    = undef;
    my $status   = undef;
    my $reading  = undef;
    my @output = ();

    if ($snmp) {
	my %int_oid
	  = (
	     '1.3.6.1.4.1.674.10892.1.300.70.1.2.1' => 'intrusionIndex',
	     '1.3.6.1.4.1.674.10892.1.300.70.1.5.1' => 'intrusionStatus',
	     '1.3.6.1.4.1.674.10892.1.300.70.1.6.1' => 'intrusionReading',
	    );
	my $result = undef;
	if ($opt{use_get_table}) {
	    my $intrusionTable = '1.3.6.1.4.1.674.10892.1.300.70.1';
	    $result = $snmp_session->get_table(-baseoid => $intrusionTable);
	}
	else {
	    $result = $snmp_session->get_entries(-columns => [keys %int_oid]);
	}

	# No intrusion is OK
	return 0 if !defined $result;

	@output = @{ get_snmp_output($result, \%int_oid) };
    }
    else {
	@output = @{ run_omreport("$omopt_chassis intrusion") };
    }

    my %int_reading
      = (
	 1 => 'Not Breached',          # chassis not breached and no uncleared breaches
	 2 => 'Breached',              # chassis currently breached
	 3 => 'Breached Prior',        # chassis breached prior to boot and has not been cleared
	 4 => 'Breach Sensor Failure', # intrusion sensor has failed
	);

  INTRUSION:
    foreach my $out (@output) {
	if ($snmp) {
	    $index    = ($out->{intrusionIndex} || 10000) - 1;
	    $status   = get_snmp_status($out->{intrusionStatus});
	    $reading  = get_hashval($out->{intrusionReading}, \%int_reading);
	}
	else {
	    $index    = get_nonempty_string('Index', $out, 9999);
	    $status   = get_nonempty_string('Status', $out, 'Unknown');
	    $reading  = get_nonempty_string('State', $out, 'Unknown state');
	}

	next INTRUSION if blacklisted('intr', $index);
	$count{intr}++;

	if ($status ne 'Ok') {
	    my $msg = sprintf 'Chassis intrusion %d detected: %s',
	      $index, $reading;
	    report('chassis', $msg, $E_WARNING, $index);
	}
	# Ok
	else {
	    my $msg = sprintf 'Chassis intrusion %d detection: %s (%s)',
	      $index, $status, $reading;
	    report('chassis', $msg, $E_OK, $index);
	}
    }
    return;
}


#-----------------------------------------
# CHASSIS: Check SD Card Device
#-----------------------------------------
sub check_sdcard {
    return if blacklisted('sdcard', 'all');

    my $index    = undef;
    my $status   = undef;
    my $state    = undef;
    my $location = undef;
    my $capacity = undef;
    my $setting  = undef;
    my @output = ();

    if ($snmp) {
	my %sd_oid
	  = (
	     '1.3.6.1.4.1.674.10892.1.1100.112.1.2.1'  => 'sdCardDeviceIndex',
	     '1.3.6.1.4.1.674.10892.1.1100.112.1.3.1'  => 'sdCardDeviceStatus',
	     '1.3.6.1.4.1.674.10892.1.1100.112.1.4.1'  => 'sdCardDeviceType',
	     '1.3.6.1.4.1.674.10892.1.1100.112.1.7.1'  => 'sdCardDeviceLocationName',
	     '1.3.6.1.4.1.674.10892.1.1100.112.1.8.1'  => 'sdCardDeviceCardPresent',
	     '1.3.6.1.4.1.674.10892.1.1100.112.1.9.1'  => 'sdCardDeviceCardState',
	     '1.3.6.1.4.1.674.10892.1.1100.112.1.10.1' => 'sdCardDeviceCardStorageSize',
	    );
	my $result = undef;
	if ($opt{use_get_table}) {
	    my $sdCardDeviceTable = '1.3.6.1.4.1.674.10892.1.1100.112.1';
	    $result = $snmp_session->get_table(-baseoid => $sdCardDeviceTable);
	}
	else {
	    $result = $snmp_session->get_entries(-columns => [keys %sd_oid]);
	}

	# No SD cards is OK
	return 0 if !defined $result;

	@output = @{ get_snmp_output($result, \%sd_oid) };
    }
    else {
	@output = @{ run_omreport("$omopt_chassis removableflashmedia") };
    }

    # Note: These values are bit fields, so combination values are possible.
    my %sd_state
      = (
	 0   => 'None',            # state is none of the following:
	 1   => 'Present',         # device is present
	 2   => 'IPMI-ready',      # device is IPMI ready
	 4   => 'Full-ready',      # device is full ready
	 8   => 'Offline',         # device is offline
	 16  => 'Failed',          # device is failed
	 32  => 'Active',          # device is active
	 64  => 'Bootable',        # device is bootable
	 128 => 'Write-protected', # device is write-protected
	 256 => 'Standby',         # device is in standby mode
	);

    my $c = 0;
  SDCARD:
    foreach my $out (@output) {
	if ($snmp) {
	    $index    = ($out->{sdCardDeviceIndex} || 10000) - 1;
	    $status   = get_snmp_status($out->{sdCardDeviceStatus});

	    if (defined $out->{sdCardDeviceCardState}) {
		my @states  = ();  # contains states SD card

		# get the combined state from the Device Status OID
		foreach my $mask (sort keys %sd_state) {
		    if (($out->{sdCardDeviceCardState} & $mask) != 0) {
			push @states, $sd_state{$mask};
		    }
		}

		# Finally, create the state string
		$state = join q{, }, @states;

		# special case: absent
		if ($out->{sdCardDeviceCardState} % 2 == 0) {
		    $state = 'Absent';
		}
	    }

	    $location = $out->{sdCardDeviceLocationName} || 'Unknown location';
	    $capacity = sprintf '%s MB', ($out->{sdCardDeviceCardStorageSize} || 'Unknown size');
	}
	else {
	    $index    = $c++;
	    $status   = get_nonempty_string('Status', $out, 'Ok');
	    $state    = get_nonempty_string('State', $out, 'Unknown state');
	    $location = get_nonempty_string('Connector Name', $out, 'Unknown location');
	    $capacity = get_nonempty_string('Storage Size', $out, 'Unknown size');

	    $capacity =~ s{\[Not Available\]}{Unknown Size};
	}

	next SDCARD if blacklisted('sd', $index);
	$count{sd}++ if $state ne 'Absent';

	if ($status ne 'Ok') {
	    my $msg = sprintf 'SD Card %d needs attention: %s',
	      $index, $state;
	    report('chassis', $msg, $E_WARNING, $index);
	}
	# Special case: Not Present
	elsif ($status eq 'Ok' and $state eq 'Absent') {
	    my $msg = sprintf 'SD Card %d [%s] is %s',
	      $index, $location, $state;
	    report('chassis', $msg, $E_OK, $index);
	}
	# Ok
	else {
	    my $msg = sprintf 'SD Card %d [%s, %s] is %s',
	      $index, $location, $capacity, $state;
	    report('chassis', $msg, $E_OK, $index);
	}
    }
    return;
}


#-----------------------------------------
# CHASSIS: Check alert log
#-----------------------------------------
sub check_alertlog {
    return if $snmp; # Not supported with SNMP

    my @output = @{ run_omreport("$omopt_system alertlog") };
    foreach my $out (@output) {
	++$count{alert}{$out->{Severity}};
    }

    # Create error messages and set exit value if appropriate
    my $err = 0;
    if ($count{alert}{'Critical'} > 0)        { $err = $E_CRITICAL; }
    elsif ($count{alert}{'Non-Critical'} > 0) { $err = $E_WARNING;  }

    my $msg = sprintf 'Alert log content: %d critical, %d non-critical, %d ok',
      $count{alert}{'Critical'}, $count{alert}{'Non-Critical'}, $count{alert}{'Ok'};
    report('other', $msg, $err);

    return;
}

#-----------------------------------------
# CHASSIS: Check ESM log overall health
#-----------------------------------------
sub check_esmlog_health {
    my $health = 'Ok';

    if ($snmp) {
	my $systemStateEventLogStatus = '1.3.6.1.4.1.674.10892.1.200.10.1.41.1';
	my $result = $snmp_session->get_request(-varbindlist => [$systemStateEventLogStatus]);
	if (!defined $result) {
	    my $msg = sprintf 'SNMP ERROR [esmhealth]: %s',
	      $snmp_session->error;
	    report('other', $msg, $E_UNKNOWN);
	}
	$health = get_snmp_status($result->{$systemStateEventLogStatus});
    }
    else {
	foreach (@{ run_command("$omreport $omopt_system esmlog -fmt ssv") }) {
	    if (m/\A Health;(.+) \z/xms) {
		$health = $1;
		chop $health;
		last;
	    }
	}
    }

    # If the overall health of the ESM log is other than "Ok", the
    # fill grade of the log is more than 80% and the log should be
    # cleared
    if ($health eq 'Ok') {
	my $msg = sprintf 'ESM log health is Ok (less than 80%% full)';
	report('other', $msg, $E_OK);
    }
    elsif ($health eq 'Critical') {
	my $msg = sprintf 'ESM log is 100%% full';
	report('other', $msg, $status2nagios{$health});
    }
    else {
	my $msg = sprintf 'ESM log is more than 80%% full';
	report('other', $msg, $status2nagios{$health});
    }

    return;
}

#-----------------------------------------
# CHASSIS: Check ESM log
#-----------------------------------------
sub check_esmlog {
    my @output = ();

    if ($snmp) {
	my %esm_oid
	  = (
	     '1.3.6.1.4.1.674.10892.1.300.40.1.7.1'  => 'eventLogSeverityStatus',
	    );
	my $result = $snmp_session->get_entries(-columns => [keys %esm_oid]);

	# No entries is OK
	return if !defined $result;

	@output = @{ get_snmp_output($result, \%esm_oid) };
	foreach my $out (@output) {
	    ++$count{esm}{$snmp_status{$out->{eventLogSeverityStatus}}};
	}
    }
    else {
	@output = @{ run_omreport("$omopt_system esmlog") };
	foreach my $out (@output) {
	    ++$count{esm}{$out->{Severity}};
	}
    }

    # Create error messages and set exit value if appropriate
    my $err = 0;
    if ($count{esm}{'Critical'} > 0)        { $err = $E_CRITICAL; }
    elsif ($count{esm}{'Non-Critical'} > 0) { $err = $E_WARNING;  }

    my $msg = sprintf 'ESM log content: %d critical, %d non-critical, %d ok',
      $count{esm}{'Critical'}, $count{esm}{'Non-Critical'}, $count{esm}{'Ok'};
    report('other', $msg, $err);

    return;
}

#
# Handy function for checking all storage components
#
sub check_storage {
    check_controllers();
    check_physical_disks();
    check_virtual_disks();
    check_cache_battery();
    check_connectors();
    check_enclosures();
    check_enclosure_fans();
    check_enclosure_pwr();
    check_enclosure_temp();
    check_enclosure_emms();
    return;
}



#---------------------------------------------------------------------
# Info functions
#---------------------------------------------------------------------

#
# Fetch output from 'omreport chassis info', put in sysinfo hash
#
sub get_omreport_chassis_info {
    if (open my $INFO, '-|', "$omreport $omopt_chassis info -fmt ssv") {
	my @lines = <$INFO>;
	close $INFO;
	foreach (@lines) {
	    next if !m/\A (Chassis\sModel|Chassis\sService\sTag|Model|Service\sTag|System\sRevision)/xms;
	    my ($key, $val) = split /;/xms;
	    $key =~ s{\s+\z}{}xms; # remove trailing whitespace
	    $val =~ s{\s+\z}{}xms; # remove trailing whitespace
	    if ($key eq 'Chassis Model' or $key eq 'Model') {
		$sysinfo{model}  = $val;
	    }
	    if ($key eq 'Chassis Service Tag' or $key eq 'Service Tag') {
		$sysinfo{serial} = $val;
	    }
	    if ($key eq 'System Revision') {
		$sysinfo{rev} = q{ } . $val;
	    }
	}
    }
    return;
}

#
# Fetch output from 'omreport chassis bios', put in sysinfo hash
#
sub get_omreport_chassis_bios {
    if (open my $BIOS, '-|', "$omreport $omopt_chassis bios -fmt ssv") {
	my @lines = <$BIOS>;
	close $BIOS;
	foreach (@lines) {
	    next if !m/;/xms;
	    my ($key, $val) = split /;/xms;
	    $key =~ s{\s+\z}{}xms; # remove trailing whitespace
	    $val =~ s{\s+\z}{}xms; # remove trailing whitespace
	    $sysinfo{bios}     = $val if $key eq 'Version';
	    $sysinfo{biosdate} = $val if $key eq 'Release Date';
	}
    }
    return;
}

#
# Fetch output from 'omreport system operatingsystem', put in sysinfo hash
#
sub get_omreport_system_operatingsystem {
    if (open my $VER, '-|', "$omreport $omopt_system operatingsystem -fmt ssv") {
	my @lines = <$VER>;
	close $VER;
	foreach (@lines) {
	    next if !m/;/xms;
	    my ($key, $val) = split /;/xms;
	    $key =~ s{\s+\z}{}xms; # remove trailing whitespace
	    $val =~ s{\s+\z}{}xms; # remove trailing whitespace
	    if ($key eq 'Operating System') {
		$sysinfo{osname} = $val;
	    }
	    elsif ($key eq 'Operating System Version') {
		$sysinfo{osver}  = $val;
	    }
	}
    }
    return;
}

#
# Fetch output from 'omreport about', put in sysinfo hash
#
sub get_omreport_about {
    if (open my $OM, '-|', "$omreport about -fmt ssv") {
	my @lines = <$OM>;
	close $OM;
	foreach (@lines) {
	    if (m/\A Version;(.+) \z/xms) {
		$sysinfo{om} = $1;
		chomp $sysinfo{om};
	    }
	}
    }
    return;
}

#
# Fetch chassis info via SNMP, put in sysinfo hash
#
sub get_snmp_chassis_info {
    my %chassis_oid
      = (
	 '1.3.6.1.4.1.674.10892.1.300.10.1.9.1'  => 'chassisModelName',
	 '1.3.6.1.4.1.674.10892.1.300.10.1.11.1' => 'chassisServiceTagName',
	 '1.3.6.1.4.1.674.10892.1.300.10.1.48.1' => 'chassisSystemRevisionName',
	);

    my $chassisInformationTable = '1.3.6.1.4.1.674.10892.1.300.10.1';
    my $result = $snmp_session->get_table(-baseoid => $chassisInformationTable);

    if (defined $result) {
	foreach my $oid (keys %{ $result }) {
	    if (exists $chassis_oid{$oid} and $chassis_oid{$oid} eq 'chassisModelName') {
		$sysinfo{model} = $result->{$oid};
		$sysinfo{model} =~ s{\s+\z}{}xms; # remove trailing whitespace
	    }
	    elsif (exists $chassis_oid{$oid} and $chassis_oid{$oid} eq 'chassisServiceTagName') {
		$sysinfo{serial} = $result->{$oid};
	    }
	    elsif (exists $chassis_oid{$oid} and $chassis_oid{$oid} eq 'chassisSystemRevisionName') {
		$sysinfo{rev} = q{ } . $result->{$oid};
	    }
	}
    }
    else {
	my $msg = sprintf 'SNMP ERROR getting chassis info: %s',
	  $snmp_session->error;
	report('other', $msg, $E_UNKNOWN);
    }
    return;
}

#
# Fetch BIOS info via SNMP, put in sysinfo hash
#
sub get_snmp_chassis_bios {
    my %bios_oid
      = (
	 '1.3.6.1.4.1.674.10892.1.300.50.1.7.1.1' => 'systemBIOSReleaseDateName',
	 '1.3.6.1.4.1.674.10892.1.300.50.1.8.1.1' => 'systemBIOSVersionName',
	);

    my $systemBIOSTable = '1.3.6.1.4.1.674.10892.1.300.50.1';
    my $result = $snmp_session->get_table(-baseoid => $systemBIOSTable);

    if (defined $result) {
	foreach my $oid (keys %{ $result }) {
	    if (exists $bios_oid{$oid} and $bios_oid{$oid} eq 'systemBIOSReleaseDateName') {
		$sysinfo{biosdate} = $result->{$oid};
		$sysinfo{biosdate} =~ s{\A (\d{4})(\d{2})(\d{2}).*}{$2/$3/$1}xms;
	    }
	    elsif (exists $bios_oid{$oid} and $bios_oid{$oid} eq 'systemBIOSVersionName') {
		$sysinfo{bios} = $result->{$oid};
	    }
	}
    }
    else {
	my $msg = sprintf 'SNMP ERROR getting BIOS info: %s',
	  $snmp_session->error;
	report('other', $msg, $E_UNKNOWN);
    }
    return;
}

#
# Fetch OS info via SNMP, put in sysinfo hash
#
sub get_snmp_system_operatingsystem {
    my %os_oid
      = (
	 '1.3.6.1.4.1.674.10892.1.400.10.1.6.1' => 'operatingSystemOperatingSystemName',
	 '1.3.6.1.4.1.674.10892.1.400.10.1.7.1' => 'operatingSystemOperatingSystemVersionName',
	);

    my $operatingSystemTable = '1.3.6.1.4.1.674.10892.1.400.10.1';
    my $result = $snmp_session->get_table(-baseoid => $operatingSystemTable);

    if (defined $result) {
	foreach my $oid (keys %{ $result }) {
	    if (exists $os_oid{$oid} and $os_oid{$oid} eq 'operatingSystemOperatingSystemName') {
		$sysinfo{osname} = ($result->{$oid});
	    }
	    elsif (exists $os_oid{$oid} and $os_oid{$oid} eq 'operatingSystemOperatingSystemVersionName') {
		$sysinfo{osver} = $result->{$oid};
	    }
	}
    }
    else {
	my $msg = sprintf 'SNMP ERROR getting OS info: %s',
	  $snmp_session->error;
	report('other', $msg, $E_UNKNOWN);
    }
    return;
}

#
# Fetch OMSA version via SNMP, put in sysinfo hash
#
sub get_snmp_about {
    # systemManagementSoftwareGlobalVersionName
    my $oid = '1.3.6.1.4.1.674.10892.1.100.10.0';
    my $result = $snmp_session->get_request(-varbindlist => [$oid]);

    if (defined $result) {
	$sysinfo{om} = exists $result->{$oid} && $result->{$oid} ne q{}
	  ? $result->{$oid} : 'unknown';
    }
    else {
	my $msg = sprintf 'SNMP ERROR: Getting OMSA version failed: %s', $snmp_session->error;
	report('other', $msg, $E_UNKNOWN);
    }
    return;
}

#
# Collects some information about the system
#
sub get_sysinfo
{
    # Get system model and serial number
    $snmp ? get_snmp_chassis_info() : get_omreport_chassis_info();

    # Get BIOS information. Only if needed
    if ( $opt{okinfo} >= 1
	 or $opt{debug}
	 or (defined $opt{postmsg} and $opt{postmsg} =~ m/[%][bd]/xms) ) {
	$snmp ? get_snmp_chassis_bios() : get_omreport_chassis_bios();
    }

    # Get OMSA information. Only if needed
    if ($opt{okinfo} >= 3 or $opt{debug}) {
	$snmp ? get_snmp_about() : get_omreport_about();
    }

    # Return now if debug
    return if $opt{debug};

    # Get OS information. Only if needed
    if (defined $opt{postmsg} and $opt{postmsg} =~ m/[%][or]/xms) {
	$snmp ? get_snmp_system_operatingsystem() : get_omreport_system_operatingsystem();
    }

    return;
}


# Helper function for running omreport when the results are strictly
# name=value pairs.
sub run_omreport_info {
    my $command = shift;
    my %output  = ();
    my @keys    = ();

    # Run omreport and fetch output
    my $rawtext = slurp_command("$omreport $command -fmt ssv 2>&1");

    # Parse output, store in array
    for ((split /\n/xms, $rawtext)) {
	if (m/\A Error/xms) {
	    my $msg = "Problem running 'omreport $command': $_";
	    report('other', $msg, $E_UNKNOWN);
	}
	next if !m/;/xms;  # ignore lines with less than two fields
	my @vals = split m/;/xms;
	$output{$vals[0]} = $vals[1];
    }

    # Finally, return the collected information
    return \%output;
}

# Get various firmware information (BMC, RAC)
sub get_firmware_info {
    my @snmp_output = ();
    my %nrpe_output = ();

    if ($snmp) {
	my %fw_oid
	  = (
	     '1.3.6.1.4.1.674.10892.1.300.60.1.7.1'  => 'firmwareType',
	     '1.3.6.1.4.1.674.10892.1.300.60.1.8.1'  => 'firmwareTypeName',
	     '1.3.6.1.4.1.674.10892.1.300.60.1.11.1' => 'firmwareVersionName',
	    );

	my $firmwareTable = '1.3.6.1.4.1.674.10892.1.300.60.1';
	my $result = $snmp_session->get_table(-baseoid => $firmwareTable);

	# Some don't have this OID, this is ok
	if (!defined $result) {
	    return;
	}

	@snmp_output = @{ get_snmp_output($result, \%fw_oid) };
    }
    else {
	%nrpe_output = %{ run_omreport_info("$omopt_chassis info") };
    }

    my %fw_type  # Firmware types
      = (
	 1  => 'other',                              # other than following values
	 2  => 'unknown',                            # unknown
	 3  => 'systemBIOS',                         # System BIOS
	 4  => 'embeddedSystemManagementController', # Embedded System Management Controller
	 5  => 'powerSupplyParallelingBoard',        # Power Supply Paralleling Board
	 6  => 'systemBackPlane',                    # System (Primary) Backplane
	 7  => 'powerVault2XXSKernel',               # PowerVault 2XXS Kernel
	 8  => 'powerVault2XXSApplication',          # PowerVault 2XXS Application
	 9  => 'frontPanel',                         # Front Panel Controller
	 10 => 'baseboardManagementController',      # Baseboard Management Controller
	 11 => 'hotPlugPCI',                         # Hot Plug PCI Controller
	 12 => 'sensorData',                         # Sensor Data Records
	 13 => 'peripheralBay',                      # Peripheral Bay Backplane
	 14 => 'secondaryBackPlane',                 # Secondary Backplane for ESM 2 systems
	 15 => 'secondaryBackPlaneESM3And4',         # Secondary Backplane for ESM 3 and 4 systems
	 16 => 'rac',                                # Remote Access Controller
	 17 => 'iDRAC',                              # Integrated Dell Remote Access Controller
	 19 => 'unifiedServerConfigurator',          # Unified Server Configurator
	 20 => 'lifecycleController',                # Lifecycle Controller
	);


    if ($snmp) {
	foreach my $out (@snmp_output) {
	    if ($fw_type{$out->{firmwareType}} eq 'baseboardManagementController') {
		$sysinfo{'bmc'} = 1;
		$sysinfo{'bmc_fw'} = $out->{firmwareVersionName};
	    }
	    elsif ($fw_type{$out->{firmwareType}} =~ m{\A rac|iDRAC \z}xms) {
		my $name = $out->{firmwareTypeName}; $name =~ s/\s//gxms;
		$sysinfo{'rac'} = 1;
		$sysinfo{'rac_name'} = $name;
		$sysinfo{'rac_fw'} = $out->{firmwareVersionName};
	    }
	}
    }
    else {
	foreach my $key (keys %nrpe_output) {
	    next if !defined $nrpe_output{$key};
	    if ($key eq 'BMC Version' or $key eq 'Baseboard Management Controller Version') {
		$sysinfo{'bmc'} = 1;
		$sysinfo{'bmc_fw'} = $nrpe_output{$key};
	    }
	    elsif ($key =~ m{\A (i?DRAC)\s*(\d?)\s+Version}xms) {
		my $name = "$1$2";
		$sysinfo{'rac'} = 1;
		$sysinfo{'rac_fw'} = $nrpe_output{$key};
		$sysinfo{'rac_name'} = $name;
	    }
	}
    }

    return;
}



#=====================================================================
# Main program
#=====================================================================

# Here we do the actual checking of components
# Check global status if applicable
if ($global) {
    $globalstatus = check_global();
}

# Do multiple selected checks
if ($check{storage})     { check_storage();       }
if ($check{memory})      { check_memory();        }
if ($check{fans})        { check_fans();          }
if ($check{power})       { check_powersupplies(); }
if ($check{temp})        { check_temperatures();  }
if ($check{cpu})         { check_processors();    }
if ($check{voltage})     { check_volts();         }
if ($check{batteries})   { check_batteries();     }
if ($check{amperage})    { check_pwrmonitoring(); }
if ($check{intrusion})   { check_intrusion();     }
if ($check{sdcard})      { check_sdcard();        }
if ($check{alertlog})    { check_alertlog();      }
if ($check{esmlog})      { check_esmlog();        }
if ($check{esmhealth})   { check_esmlog_health(); }


#---------------------------------------------------------------------
# Finish up
#---------------------------------------------------------------------

# Counter variable
%nagios_alert_count
  = (
     'OK'       => 0,
     'WARNING'  => 0,
     'CRITICAL' => 0,
     'UNKNOWN'  => 0,
    );

# Get system information
get_sysinfo();

# Get firmware info if requested via option
if ($opt{okinfo} >= 1) {
    get_firmware_info();
}

# Close SNMP session
if ($snmp) {
    $snmp_session->close;
}

# Print messages
if ($opt{debug}) {
    # finding the mode of operation
    my $mode = 'local';
    if ($snmp) {
	# Setting the domain (IP version and transport protocol)
	my $transport = $opt{tcp} ? 'TCP' : 'UDP';
	my $ipversion = $opt{ipv6} ? 'IPv6' : 'IPv4';
	$mode = "SNMPv$opt{protocol} $transport/$ipversion";
    }

    print "   System:      $sysinfo{model}$sysinfo{rev}";
    print q{ } x (25 - length "$sysinfo{model}$sysinfo{rev}"), "OMSA version:    $sysinfo{om}\n";
    print "   ServiceTag:  $sysinfo{serial}";
    print q{ } x (25 - length $sysinfo{serial}), "Plugin version:  $VERSION\n";
    print "   BIOS/date:   $sysinfo{bios} $sysinfo{biosdate}";
    print q{ } x (25 - length "$sysinfo{bios} $sysinfo{biosdate}"), "Operation mode:  $mode\n";
    if ($#report_storage >= 0) {
	print "-----------------------------------------------------------------------------\n";
	print "   Storage Components                                                        \n";
	print "=============================================================================\n";
	print "  STATE  |    ID    |  MESSAGE TEXT                                          \n";
	print "---------+----------+--------------------------------------------------------\n";
	foreach (@report_storage) {
	    my ($msg, $level, $nexus) = @{$_};
	    print q{ } x (8 - length $reverse_exitcode{$level}) . "$reverse_exitcode{$level} | "
	      . q{ } x (8 - length $nexus) . "$nexus | $msg\n";
	    $nagios_alert_count{$reverse_exitcode{$level}}++;
	}
    }
    if ($#report_chassis >= 0) {
	print "-----------------------------------------------------------------------------\n";
	print "   Chassis Components                                                        \n";
	print "=============================================================================\n";
	print "  STATE  |  ID  |  MESSAGE TEXT                                              \n";
	print "---------+------+------------------------------------------------------------\n";
	foreach (@report_chassis) {
	    my ($msg, $level, $nexus) = @{$_};
	    print q{ } x (8 - length $reverse_exitcode{$level}) . "$reverse_exitcode{$level} | "
	      . q{ } x (4 - length $nexus) . "$nexus | $msg\n";
	    $nagios_alert_count{$reverse_exitcode{$level}}++;
	}
    }
    if ($#report_other >= 0) {
	print "-----------------------------------------------------------------------------\n";
	print "   Other messages                                                            \n";
	print "=============================================================================\n";
	print "  STATE  |  MESSAGE TEXT                                                     \n";
	print "---------+-------------------------------------------------------------------\n";
	foreach (@report_other) {
	    my ($msg, $level, $nexus) = @{$_};
	    print q{ } x (8 - length $reverse_exitcode{$level}) . "$reverse_exitcode{$level} | $msg\n";
	    $nagios_alert_count{$reverse_exitcode{$level}}++;
	}
    }
}
else {
    my $c = 0;  # counter to determine linebreaks

    # Run through each message, sorted by severity level
  ALERT:
    foreach (sort {$a->[1] < $b->[1]} (@report_storage, @report_chassis, @report_other)) {
	my ($msg, $level, $nexus) = @{ $_ };
	next ALERT if $level == $E_OK;

	if (defined $opt{only}) {
	    # If user wants only critical alerts
	    next ALERT if ($opt{only} eq 'critical' and $level == $E_WARNING);

	    # If user wants only warning alerts
	    next ALERT if ($opt{only} eq 'warning' and $level == $E_CRITICAL);
	}

	# Prefix with service tag if specified with option '-i|--info'
	if ($opt{info}) {
	    if (defined $opt{htmlinfo}) {
		$msg = '[<a href="' . warranty_url($sysinfo{serial})
		  . "\">$sysinfo{serial}</a>] " . $msg;
	    }
	    else {
		$msg = "[$sysinfo{serial}] " . $msg;
	    }
	}

	# Prefix with nagios level if specified with option '--state'
	$msg = $reverse_exitcode{$level} . ": $msg" if $opt{state};

	# Prefix with one-letter nagios level if specified with option '--short-state'
	$msg = (substr $reverse_exitcode{$level}, 0, 1) . ": $msg" if $opt{shortstate};

	($c++ == 0) ? print $msg : print $linebreak, $msg;

	$nagios_alert_count{$reverse_exitcode{$level}}++;
    }
}

# Determine our exit code
$exit_code = $E_OK;
$exit_code = $E_UNKNOWN  if $nagios_alert_count{'UNKNOWN'} > 0;
$exit_code = $E_WARNING  if $nagios_alert_count{'WARNING'} > 0;
$exit_code = $E_CRITICAL if $nagios_alert_count{'CRITICAL'} > 0;

# Global status via SNMP.. extra safety check
if ($globalstatus != $E_OK && $exit_code == $E_OK && !defined $opt{only}) {
    print "OOPS! Something is wrong with this server, but I don't know what. ";
    print "The global system health status is $reverse_exitcode{$globalstatus}, ";
    print "but every component check is OK. This may be a bug in the Nagios plugin, ";
    print "please file a bug report.\n";
    exit $E_UNKNOWN;
}

# Print OK message
if ($exit_code == $E_OK && defined $opt{only} && $opt{only} !~ m{\A critical|warning|chassis \z}xms && !$opt{debug}) {
    my %okmsg
      = ( 'storage'     => "STORAGE OK - $count{pdisk} physical drives, $count{vdisk} logical drives",
	  'fans'        => $count{fan} == 0 && $blade ? 'OK - blade system with no fan probes' : "FANS OK - $count{fan} fan probes checked",
	  'temp'        => "TEMPERATURES OK - $count{temp} temperature probes checked",
	  'memory'      => "MEMORY OK - $count{dimm} memory modules, $count{mem} MB total memory",
	  'power'       => $count{power} == 0 ? 'OK - no instrumented power supplies found' : "POWER OK - $count{power} power supplies checked",
	  'cpu'         => "PROCESSORS OK - $count{cpu} processors checked",
	  'voltage'     => "VOLTAGE OK - $count{volt} voltage probes checked",
	  'batteries'   => $count{bat} == 0 ? 'OK - no batteries found' : "BATTERIES OK - $count{bat} batteries checked",
	  'amperage'    => $count{amp} == 0 ? 'OK - no power monitoring probes found' : "AMPERAGE OK - $count{amp} amperage (power monitoring) probes checked",
	  'intrusion'   => $count{intr} == 0 ? 'OK - no intrusion detection probes found' : "INTRUSION OK - $count{intr} intrusion detection probes checked",
	  'alertlog'    => $snmp ? 'OK - not supported via snmp' : "OK - Alert Log content: $count{alert}{Ok} ok, $count{alert}{'Non-Critical'} warning and $count{alert}{Critical} critical",
	  'esmlog'      => "OK - ESM Log content: $count{esm}{Ok} ok, $count{esm}{'Non-Critical'} warning and $count{esm}{Critical} critical",
	  'esmhealth'   => "ESM LOG OK - less than 80% used",
          'sdcard'      => "SD CARDS OK - $count{sd} SD cards installed",
	);

    print $okmsg{$opt{only}};
}
elsif ($exit_code == $E_OK && !$opt{debug}) {
    if (defined $opt{htmlinfo}) {
	printf q{OK - System: '<a href="%s">%s%s</a>', SN: '<a href="%s">%s</a>'},
	  documentation_url($sysinfo{model}), $sysinfo{model}, $sysinfo{rev},
	    warranty_url($sysinfo{serial}), $sysinfo{serial};
    }
    else {
	printf q{OK - System: '%s%s', SN: '%s'},
	  $sysinfo{model}, $sysinfo{rev}, $sysinfo{serial};
    }

    if ($check{memory}) {
	my $unit = 'MB';
	if ($count{mem} >= 1024) {
	    $count{mem} /= 1024;
	    $unit = 'GB';
	}
	printf ', %d %s ram (%d dimms)', $count{mem}, $unit, $count{dimm};
    }
    else {
	print ', not checking memory';
    }

    if ($check{storage}) {
	printf ', %d logical drives, %d physical drives',
	  $count{vdisk}, $count{pdisk};
    }
    else {
	print ', not checking storage';
    }

    if ($opt{okinfo} >= 1) {
	print $linebreak;
	printf q{----- BIOS='%s %s'}, $sysinfo{bios}, $sysinfo{biosdate};

	if ($sysinfo{rac}) {
	    printf q{, %s='%s'}, $sysinfo{rac_name}, $sysinfo{rac_fw};
	}
	if ($sysinfo{bmc}) {
	    printf q{, BMC='%s'}, $sysinfo{bmc_fw};
	}
    }

    if ($opt{okinfo} >= 2) {
	if ($check{storage}) {
	    my @storageprint = ();
	    foreach my $id (sort keys %{ $sysinfo{controller} }) {
		chomp $sysinfo{controller}{$id}{driver};
		my $msg = sprintf q{----- Ctrl %s [%s]: Fw='%s', Dr='%s'},
		  $sysinfo{controller}{$id}{id}, $sysinfo{controller}{$id}{name},
		    $sysinfo{controller}{$id}{firmware}, $sysinfo{controller}{$id}{driver};
		if (defined $sysinfo{controller}{$id}{storport}) {
		    $msg .= sprintf q{, Storport: '%s'}, $sysinfo{controller}{$id}{storport};
		}
		push @storageprint, $msg;
	    }
	    foreach my $id (sort keys %{ $sysinfo{enclosure} }) {
		push @storageprint, sprintf q{----- Encl %s [%s]: Fw='%s'},
		  $sysinfo{enclosure}{$id}->{id}, $sysinfo{enclosure}{$id}->{name},
		    $sysinfo{enclosure}{$id}->{firmware};
	    }

	    # print stuff
	    foreach my $line (@storageprint) {
		print $linebreak, $line;
	    }
	}
    }

    if ($opt{okinfo} >= 3) {
	print "$linebreak----- OpenManage Server Administrator (OMSA) version: '$sysinfo{om}'";
    }

}
else {
    if ($opt{extinfo}) {
	print $linebreak;
	if (defined $opt{htmlinfo}) {
	    printf '------ SYSTEM: <a href="%s">%s%s</a>, SN: <a href="%s">%s</a>',
	      documentation_url($sysinfo{model}), $sysinfo{model}, $sysinfo{rev},
		warranty_url($sysinfo{serial}), $sysinfo{serial};
	}
	else {
	    printf '------ SYSTEM: %s%s, SN: %s',
	      $sysinfo{model}, $sysinfo{rev}, $sysinfo{serial};
	}
    }
    if (defined $opt{postmsg}) {
	my $post = undef;
	if (-f $opt{postmsg}) {
	    open my $POST, '<', $opt{postmsg}
	      or ( print $linebreak
		   and print "ERROR: Couldn't open post message file $opt{postmsg}: $!\n"
		   and exit $E_UNKNOWN );
	    $post = <$POST>;
	    close $POST;
	    chomp $post;
	}
	else {
	    $post = $opt{postmsg};
	}
	if (defined $post) {
	    print $linebreak;
	    $post =~ s{[%]s}{$sysinfo{serial}}gxms;
	    $post =~ s{[%]m}{$sysinfo{model}$sysinfo{rev}}gxms;
	    $post =~ s{[%]b}{$sysinfo{bios}}gxms;
	    $post =~ s{[%]d}{$sysinfo{biosdate}}gxms;
	    $post =~ s{[%]o}{$sysinfo{osname}}gxms;
	    $post =~ s{[%]r}{$sysinfo{osver}}gxms;
	    $post =~ s{[%]p}{$count{pdisk}}gxms;
	    $post =~ s{[%]l}{$count{vdisk}}gxms;
	    $post =~ s{[%]n}{$linebreak}gxms;
	    $post =~ s{[%]{2}}{%}gxms;
	    print $post;
	}
    }
}

# Reset the WARN signal
$SIG{__WARN__} = 'DEFAULT';

# Print any perl warnings that have occured
if (@perl_warnings) {
    foreach (@perl_warnings) {
	chop @$_;
	print "${linebreak}INTERNAL ERROR: @$_";
    }
    $exit_code = $E_UNKNOWN;
}

# Print performance data
if (defined $opt{perfdata} && !$opt{debug} && @perfdata) {
    my $lb = $opt{perfdata} eq 'multiline' ? "\n" : q{ };  # line break for perfdata
    print q{|};

    # Sort routine for performance data
    sub perfsort {
	my %order = ( fan => 0, pwr => 1, tem => 2, enc => 3, );
	return ($order{(substr $a->{label}, 0, 3)} cmp $order{(substr $b->{label}, 0, 3)}) ||
	  $a->{label} cmp $b->{label};
    }

    # Print performance data sorted
    my $type = $opt{perfdata} eq 'minimal' ? 'mini' : 'label';
    print join $lb, map { "$_->{$type}=$_->{value};$_->{warn};$_->{crit}" } sort perfsort @perfdata;
}

# Print a linebreak at the end
print "\n" if !$opt{debug};

# Exit with proper exit code
exit $exit_code;

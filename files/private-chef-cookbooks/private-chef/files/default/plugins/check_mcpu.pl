#!/usr/bin/perl -w

#######################################################################
#
# Copyright (c) 2007 Jaime Gascon Romero <jgascon@gmail.com>
#
# License Information:
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>. 
# 
# This plugin depends on the sysstat package:
# http://perso.orange.fr/sebastien.godard/index.html
#
# $Id: check_mcpu,v 1.10 2007/09/20 10:54:42 jgr Exp jgr $
# $Revision: 1.10 $
# Home Site: http://emergeworld.blogspot.com/
#
# Changelog
# =========
# 1.10 Fixed bug when sysstats is installed with nls support (20.09.2007)
# 1.9 Added -u option and some cosmetics changes in the output (17.07.2007)
# 1.2 Initial Public Release  (05.07.2007)
# #####################################################################

use strict;
use Getopt::Long;
use File::Basename;
use vars qw($PROGNAME $VERSION);
use lib "/usr/lib/nagios/plugins";
use utils qw(%ERRORS);

$PROGNAME = basename($0);
$VERSION = '$Revision: 1.10 $';
$ENV{LC_ALL} = 'POSIX';

my ($opt_c, $opt_w, $opt_i, $opt_r, $opt_h, $opt_u);
my ($ccrit, $cwarn, $output, $cont, @cpus_idle, $flag_u, $cpu);

sub print_help ();
sub print_usage ();

GetOptions
  ("h"  =>  \$opt_h, "help" => \$opt_h,
   "w=f" => \$opt_w, "warning=f"  => \$opt_w, 
   "c=f" => \$opt_c, "critical=f" => \$opt_c,
   "r=i" => \$opt_r, "reports=i" => \$opt_r,
   "i=i" => \$opt_i, "interval=i" => \$opt_i,
   "u" => \$opt_u, "used" => \$opt_u,
  ) or exit $ERRORS{'UNKNOWN'};

# default values
unless (defined $opt_w) {
  if (defined $opt_u) {
    $opt_w = 80;
  } else {
    $opt_w = 20;
  }
}

unless ( defined $opt_c ) {
if (defined $opt_u) {
    $opt_c = 90;
  } else {
    $opt_c = 10;
  }
}

unless ( defined $opt_r) {
  $opt_r = 1;
}

unless ( defined $opt_i) {
  $opt_i = 1;
}

if ($opt_h) {print_help(); exit $ERRORS{'OK'};}

if (defined $opt_u) {
  if ($opt_c < $opt_w) {
    print "Error: Warning (-w) cannot be greater than Critical (-c)\n";
    exit $ERRORS{'UNKNOWN'};
  }
}

if ( ! defined $opt_u ) {
  if ($opt_c > $opt_w) {
  print "Error: Critical (-c) cannot be greater than Warning (-w)\n"; 
  exit $ERRORS{'UNKNOWN'};
  }
}
  
$ccrit = 0; # critical counter
$cwarn = 0; # warning counter
$cont = 0;
$output = "";

# Here is te magic!!!
@cpus_idle = `mpstat -P ALL $opt_i $opt_r | awk '\$1 == "Average:" && \$2 ~ /[0-9]+/ {print \$11}'`;

$cpu = 0;

foreach $cpu (@cpus_idle) {
  chomp(@cpus_idle);
  if (defined $opt_u) {
    $cpu = 100 - $cpu;
    $ccrit++ if ($opt_c < $cpu);
    $cwarn++ if ($opt_w < $cpu);
    $cpu = sprintf "%.2f", $cpu;
    $output = "${output}CPU${cont} $cpu% ";
    $cont++
  } else {
    $ccrit++ if ($opt_c > $cpu);
    $cwarn++ if ($opt_w > $cpu);
    $output = "${output}CPU${cont} $cpu% ";
    $cont++
  }
}

# Whether print used or idle
if (defined $opt_u)  {
  $flag_u = "Used" 
  } else {
  $flag_u = "Idle"
}

# On multicpu boxes we require more than half to be loaded
my $cpu_loaded_threshold = $#cpus_idle / 2;

if ($ccrit > $cpu_loaded_threshold) {print "$flag_u CPU CRITICAL: $output\n"; exit $ERRORS{'CRITICAL'};}

if ($cwarn > $cpu_loaded_threshold) {print "$flag_u CPU WARNING: $output\n"; exit $ERRORS{'WARNING'};}

print "$flag_u CPU OK: $output\n";
exit $ERRORS{'OK'};

#######################################################################
# subs

sub print_usage () {
  print "Usage: $PROGNAME [-w | --warning] <value> [-c | --critical] <value> [-r | --reports] <value> [-i | --interval] <value>\n";
  print "       $PROGNAME -u | --used [-w | --warning] <value> [-c | --critical] <value> [-r | --reports] <value> [-i | --interval] <value>\n";
  print "       $PROGNAME -h | --help\n";
}

sub print_help () {
  print "$PROGNAME $VERSION\n\n";
  print "Copyright (c) 2007 Jaime Gascon Romero

This plugin checks the amount of used or idle cpu (default is idle) for multiple cpus. All arguments are optionals.

";
  print_usage();
  print "
-c | --critical
  Exit with CRITICAL status if idle CPU is less than the critical percentage value. 
  In used mode exits with CRITICAL status if the amount of used CPU is more tan the critical percentage value.
  Default value: 10 
  Deafault value in used mode: 90

-w | --warning
  Exit with WARNING status if idle CPU is less than the warning percentage value.
  In used mode exits with WARNING status if the amount of used CPU is more tan the critical percentage value.
  Default value: 20
  Default value in used mode: 80

-i | --interval
  Amount of time in seconds between each report of mpstat.
  Default value: 1

-r | --reports
  Number of reports generated at interval seconds apart for mpstat.
  Default value: 1

-u | --used
  Check the amount of used CPU instead of the idle CPU.

-h | --help
  Show this help.

This plugin depends on the sysstat package:
http://perso.orange.fr/sebastien.godard/index.html

";
}

#############
# Changelog #
#############
# (19/07/07) Uso del modulo File::Basename para determinar el nombre del script => $PROGNAME = basename($0);

# vim:sts=2:sw=2:ts=2:et

#!/usr/bin/perl
#
# DESCRIPTION: Nagios plugin to check the health of a HP ServiceGuard
#              cluster
#
# AUTHOR: Trond H. Amundsen <t.h.amundsen@usit.uio.no>
#
# $Id: check_serviceguard 14512 2009-07-23 11:45:08Z trondham $
#
# Copyright (C) 2009 Trond H. Amundsen
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

use strict;
use warnings;
use POSIX qw(isatty);
use Sys::Hostname;
use Getopt::Long qw(:config no_ignore_case);
use Pod::Usage;

#---------------------------------------------------------------------
# Initialization and global variables
#---------------------------------------------------------------------

# Version and similar info
my $NAME    = 'check_serviceguard';
my $VERSION = '1.2.2';
my $AUTHOR  = 'Trond H. Amundsen';
my $CONTACT = 't.h.amundsen@usit.uio.no';

# Exit codes
my $OK       = 0;
my $WARNING  = 1;
my $CRITICAL = 2;
my $UNKNOWN  = 3;

# Nagios error levels reversed
my %ERRORCODE
  = (
     0 => 'OK',
     1 => 'WARNING',
     2 => 'CRITICAL',
     3 => 'UNKNOWN',
    );

# Options with default values
my %opt
  = ( 'blacklist'   => [],
      'primary'     => 0,
      'autorun'     => 1,
      'timeout'     => 30,  # default timeout is 30 seconds
      'verbose'     => 0,
      'help'        => 0,
      'man'         => 0,
      'version'     => 0,
      'state'       => 0,
      'short-state' => 0,
      'linebreak'   => undef,
    );

# Get options
GetOptions('blacklist=s'  => \@{ $opt{blacklist} },
     'primary!'     => \$opt{primary},
     'autorun!'     => \$opt{autorun},
     't|timeout=i'  => \$opt{timeout},
     'v|verbose'    => \$opt{verbose},
     'h|help'       => \$opt{help},
     'man'          => \$opt{man},
     'V|version'    => \$opt{version},
     'state'        => \$opt{state},
     'short-state'  => \$opt{shortstate},
     'linebreak=s'  => \$opt{linebreak},
    ) or pod2usage(-exitstatus => $UNKNOWN, -verbose => 0);

# If user requested help
if ($opt{'help'}) {
    pod2usage(-exitstatus => $OK, -verbose => 1);
}

# If user requested man page
if ($opt{'man'}) {
    pod2usage(-exitstatus => $OK, -verbose => 2);
}

# If user requested version info
if ($opt{'version'}) {
    print <<"END_VERSION";
$NAME $VERSION
Copyright (C) 2009 $AUTHOR
License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.

Written by $AUTHOR <$CONTACT>
END_VERSION
    exit $OK;
}

# Reports (messages) are gathered in this array
my @report = ();

# Set PATH and LANG environments
$ENV{'PATH'} = join q{:}, '/usr/local/cmcluster/bin', '/usr/bin', $ENV{'PATH'};
$ENV{'LANG'} = 'C';

# Variables holding various information
my %cluster = ();  # cluster info
my %package = ();  # packages
my @nodes   = ();  # list of nodes

# The name of this machine, not FQDN
my $hostname = hostname();
$hostname =~ s{
                \A                # Beginning of line
                ([\w-]+?) [.]     # Capture all up to the first dot
                [^\n]*            # The rest
                \z                # End of line
          }{$1}xms;

# Setting timeout
$SIG{ALRM} = sub {
    print "$NAME timed out after $opt{timeout} seconds\n";
    exit $UNKNOWN;
};
alarm $opt{timeout};

# Default line break
my $linebreak = isatty(*STDOUT) ? "\n" : '<br/>';

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

# The cmviewcl command. Don't sudo if we're root
#my $cmviewcl = $< != 0 ? 'sudo -S </dev/null cmviewcl' : 'cmviewcl';
my $cmviewcl = $< != 0 ? 'sudo cmviewcl' : 'cmviewcl';

# Blacklisted components
my @blacklist = defined $opt{blacklist} ? @{ get_blacklist() } : ();



#=====================================================================
# Functions
#=====================================================================

# Helper function to run cmviewcl with options
sub run_cmviewcl {
    my $options = shift;

    open my $CMD, q{-|}, "$cmviewcl $options 2>&1"
      or ( print "Couldn't run '$cmviewcl $options': $!\n"
     and exit $UNKNOWN );
    my @output = <$CMD>;
    close $CMD
      or push @report, [ "Couldn't close file descriptor for '$cmviewcl $options': $!",
       $UNKNOWN ];

    # Check if we have permission to execute cmviewcl
    if (grep { m/(^password:)|(you\smust\sbe\sroot)/ixms } @output) {
  print "Insufficient rights to call '$cmviewcl $options'\n";
  exit $UNKNOWN;
    }
    elsif (grep { m/must\shave\sa\stty/xms } @output) {
  print "sudo must be configured with requiretty=no (man sudo)\n";
  exit $UNKNOWN;
    }

    return \@output;
}

# Read the blacklist option and return a hash containing the
# blacklisted components
sub get_blacklist {
    my @bl = ();
    my @blacklist = ();

    if (scalar @{ $opt{blacklist} } >= 0) {
  foreach my $black (@{ $opt{blacklist} }) {
      my $tmp = q{};
      if (-f $black) {
    open my $BL, '<', $black
      or ( push @report, [ "Couldn't open blacklist file '$black': $!",
               $UNKNOWN ] and return [] );
    $tmp = <$BL>;
    close $BL
      or ( push @report, [ "Couldn't close blacklist file '$black': $!",
               $UNKNOWN ] and return [] );
    chomp $tmp;
      }
      else {
    $tmp = $black;
      }
      push @bl, $tmp;
  }
    }

    return [] if $#bl < 0;

    # Parse blacklist string, put in hash
    foreach (@bl) {
  push @blacklist, split /,/xms;
    }

    return \@blacklist;
}

# Checks if a component is blacklisted. Returns 1 if the component is
# blacklisted, 0 otherwise.
sub blacklisted {
    my $name = shift;  # package name

    foreach (@blacklist) {
  return 1 if $_ eq $name;
    }

    return 0;
}

#--------------------
# Check cluster
#--------------------
sub check_cluster {
    my $output = run_cmviewcl('-l cluster');

  LINE:
    foreach (@{ $output }) {
  next LINE if m{\A\z}xms;       # empty line
  next LINE if m{\A CLUSTER}xms; # header
  ($cluster{name}, $cluster{status}) = (split /\s+/xms, $_);
    }
    push @report, [ sprintf(q{Cluster is %s}, $cluster{status}),
        $cluster{status} eq 'up' ? $OK : $CRITICAL ];
    return;
}

#--------------------
# Check nodes
#--------------------
sub check_nodes {
    my $output = run_cmviewcl('-l node');

  LINE:
    foreach (@{ $output }) {
  next LINE if m/\A \s+NODE\s+STATUS\s+STATE/xms; # header
  if (m/\A \s+(\S+)\s+(\S+)\s+(\S+)/xms) {
      my ($node, $status, $state) = ($1, $2, $3);
      $state =~ s{\s+$}{}xms; # strip end whitespace
      push @nodes, [ $node, $status, $state ];
      push @report, [ sprintf(q{Node '%s' is %s (%s)},
            $node, $status, $state),
          $status eq 'up' && $state eq 'running' ? $OK : $CRITICAL ];
  }
    }
    return;
}

#--------------------
# Check node parameters
#--------------------
sub check_node_parameters {
  NODE:
    foreach my $n (@nodes) {
  my ($node, $status, $state) = @{ $n };

  next NODE if ($status ne 'up' || $state ne 'running');
  my $output = run_cmviewcl("-v -l node -n $node");

  my $f_quorum  = 0;  # quorum flag
  my $f_network = 0;  # network flag

      LINE:
  foreach (@{ $output }) {
      next LINE if m{\A\z}xms;                                       # empty line
      next LINE if m{\A \s+NAME\s+STATUS\s+STATE}xms;                # quorum header
      next LINE if m{\A \s+$node\s+\w+\s+\w+}xms;                    # node line
      next LINE if m{\A \s+INTERFACE\s+STATUS(\s+PATH)?\s+NAME}xms;  # network header

      if (m/Quorum_Server_Status:/xms) {
    $f_quorum = 1;
    next LINE;
      }
      if (m/Network_Parameters:/xms) {
    $f_network = 1;
    next LINE;
      }
      if ($f_quorum and m{\A \s+(\S+)\s+(\S+)\s+(\S+)\s* \z}xms) {
    $f_quorum = 0;
    my $quorum_name   = $1;
    my $quorum_status = $2;
    my $quorum_state  = $3;
    push @report, [ sprintf(q{Quorum '%s' for node %s is %s (%s)},
          $quorum_name, $node, uc($quorum_status), $quorum_state),
        $quorum_status eq 'up' && $quorum_state eq 'running' ? $OK : $CRITICAL ];
    next LINE;
      }
      if ($f_network and m{\A \s+(\S+)\s+(\S+)\s+(\S+)\s* \z}xms) {
    my $net_type   = $1;
    my $net_status = $2;
    my $net_name   = $3;
    push @report, [ sprintf(q{%s network (%s) on node %s is %s},
          $net_type, $net_name, $node, $net_status),
        $net_status eq 'up' ? $OK : $CRITICAL ];
    next LINE;
      }
      if ($f_network and m{\A \s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s* \z}xms) {
    # alternate network output with PATH
    my $net_type   = $1;
    my $net_status = $2;
    my $net_path   = $3;
    my $net_name   = $4;
    push @report, [ sprintf(q{%s network (%s - %s) on node %s is %s},
          $net_type, $net_name, $net_path, $node, $net_status),
        $net_status eq 'up' ? $OK : $CRITICAL ];
    next LINE;
      }

      $f_quorum  = 0;
      $f_network = 0;
  }
    }
    return;
}

#--------------------
# Check packages
#--------------------
sub check_packages {
    my $output = run_cmviewcl('-v -l package');

    my $f_package = 0;  # flag package info
    my $f_policy  = 0;  # flag policy info
    my $f_script  = 0;  # flag script info
    my $f_node    = 0;  # flag node info
    my $f_dep     = 0;  # flag dependency info

    my $this_package = undef;

    # Regexp for package info
    my $r_package
      = qr{
              \A           # beginning of string
              \s+(\S+)     # package name
              \s+(\S+)     # status
              \s+(\S+)?    # state
              \s+(\S+)?    # auto_run
              \s+(\S+)     # node
              \s*
              \z
      }xms;

    # Regexp for package scripts
    my $r_script
      = qr{
              \A           # beginnning of string
              \s+(\S+)     # item
              \s+(\S+)     # status
              \s+(\S+)     # max restarts
              \s+(\S+)     # restarts
              \s+(\S+)     # name
              \s*
              \z
      }xms;

    # Regexp for package scripts (alternate)
    my $r_script_alt
      = qr{
              \A           # beginnning of string
              \s+(\S+)     # item
              \s+(\S+)     # status
              \s+(\S+)     # name
              \s*
              \z
      }xms;

    # Regexp for node info
    my $r_node
      = qr{
              \A           # beginnning of string
              \s+(\S+)     # node type
              \s+(\S+)     # status
              \s+(\S+)?    # switching
              \s+(.+)      # name
              \z           # end of string
      }xms;

  LINE:
    foreach (@{ $output }) {
  next LINE if m{\A\z}xms;                                                 # empty line
  next LINE if m{\A \s+POLICY_NAME\s+CONFIGURED_VALUE}xms;                 # policy header
  next LINE if m{\A \s+ITEM\s+STATUS\s+MAX_RESTARTS\s+RESTARTS\s+NAME}xms; # script header
  next LINE if m{\A \s+NODE_TYPE\s+STATUS\s+SWITCHING\s+NAME}xms;          # node switch header

  if (m{\A \s+PACKAGE\s+STATUS\s+STATE\s+AUTO_RUN\s+NODE}xms) {
      $f_package = 1;
      $f_node = 0;
      next LINE;
  }
  if (m/Policy_Parameters:/xms) {
      $f_policy = 1;
      $f_package = 0;
      next LINE;
  }
  if (m/Script_Parameters:/xms) {
      $f_script = 1;
      $f_policy = 0;
      next LINE;
  }
  if (m/Node_Switching_Parameters:/xms) {
      $f_node = 1;
      $f_script = 0;
      next LINE;
  }
  if (m/Dependency_Parameters:/xms) {
      $f_dep = 1;
      $f_node = 0;
      next LINE;
  }
  if ($f_package and m/$r_package/xms) {
      my ($package, $status, $state, $auto_run, $node) = ($1, $2, $3, $4, $5);
      $this_package = $package;
      next LINE if blacklisted($this_package);
      push @report, [ sprintf(q{Package '%s' is %s (%s)},
            $package, $status, $state),
          $status eq 'up' && $state eq 'running' ? $OK : $CRITICAL ];
      push @report, [ sprintf(q{AUTO_RUN for package '%s' is %s},
            $package, $auto_run),
          $opt{autorun} && $auto_run ne 'enabled' && $status eq 'up'
          ? $WARNING : $OK ];
      next LINE;
  }
  if ($f_script and m/$r_script/xms) {
      next LINE if blacklisted($this_package);
      my ($item, $status, $max_restarts, $restarts, $name) = ($1, $2, $3, $4, $5);
      push @report, [ sprintf(q{%s '%s' for package '%s' status is %s},
            $item, $name, $this_package, $status),
          $status eq 'up' ? $OK : $WARNING ];
      next LINE;
  }
  if ($f_script and m/$r_script_alt/xms) {
      # alternate (without restarts and max_restarts)
      next LINE if blacklisted($this_package);
      my ($item, $status, $name) = ($1, $2, $3);
      push @report, [ sprintf(q{%s '%s' for package '%s' status is %s},
            $item, $name, $this_package, $status),
          $status eq 'up' ? $OK : $WARNING ];
      next LINE;
  }
  if ($f_node and  m/$r_node/xms) {
      my ($node_type, $status, $switching, $name) = ($1, $2, $3, $4);
      if ($name =~ m/\A \s* \z/xms) {
    $name = $switching;
    $switching = q{};
      }
      $name =~ s{\s+\z}{}xms; # strip end whitespace

      # registering current node
      if ($name =~ m{\A (.+?)\s+\(current\)}xms) {
    $name = $1;
    $package{$this_package}{'current'} = $name;
      }

      # register primary and alternate nodes
      push @{ $package{$this_package}{'nodes'} }, $name;
      next LINE;
  }
    }

  PACKAGE:
    foreach my $p (keys %package) {
  next PACKAGE if blacklisted($p);
  next PACKAGE if !defined $package{$p}{current};
  if ($package{$p}{current} eq $package{$p}{nodes}[0]) {
      push @report, [ sprintf(q{Package '%s' is running on primary node %s},
            $p, $package{$p}{'current'}),
          $OK ];
  }
  else {
      push @report, [ sprintf(q{Package '%s' is running on alternate node %s (primary=%s)},
            $p, $package{$p}{'current'}, $package{$p}{nodes}[0]),
          $opt{primary} ? $WARNING : $OK ];
  }
    }
    return;
}


#=====================================================================
# Main program
#=====================================================================

# check cluster status
check_cluster();

# check nodes and packages
if ($cluster{status} eq 'up') {
    check_nodes();            # check node status
    check_node_parameters();  # check node parameters
    check_packages();         # check packages
}

# Counter variable
my %nagios_level_count
  = (
     'OK'       => 0,
     'WARNING'  => 0,
     'CRITICAL' => 0,
     'UNKNOWN'  => 0,
    );

# Print messages
if ($opt{verbose}) {
    if ($#report >= 0) {
  print "-----------------------------------------------------------------------------\n";
  print "   HP ServiceGuard Status for cluster: $cluster{name}\n";
  print "=============================================================================\n";
  print "  STATE  |   MESSAGE TEXT\n";
  print "---------+-------------------------------------------------------------------\n";
  foreach (@report) {
      my ($msg, $level) = @{$_};
      print q{ } x (8 - length $ERRORCODE{$level}),
        "$ERRORCODE{$level} | $msg\n";
      $nagios_level_count{$ERRORCODE{$level}}++;
  }
    }
}
else {
    my $report_node = $hostname;  # which node should report

    # Find the first node in the node list that is up and running
  NODE:
    foreach (@nodes) {
  if ($_->[1] eq 'up' and $_->[2] eq 'running') {
      $report_node = $_->[0];
      last NODE;
  }
    }

    # We are running on the first node that is up, we should report
    # errors. If running on other node, move on..
    if ($report_node eq $hostname) {
  my $c = 0;
      REPORT:
  foreach (sort {$a->[1] < $b->[1]} @report) {
      my ($msg, $level) = @{ $_ };
      $nagios_level_count{$ERRORCODE{$level}}++;
      next REPORT if $level == 0;

      # Prefix with the cluster name
      $msg = sprintf '[%s] %s', $cluster{name}, $msg;

      # Prefix with nagios level if specified with option '--state'
      $msg = $ERRORCODE{$level} . ": $msg" if $opt{state};

      # Prefix with one-letter nagios level if specified with option '--short-state'
      $msg = (substr $ERRORCODE{$level}, 0, 1) . ": $msg" if $opt{shortstate};

      ($c++ == 0) ? print $msg : print $linebreak, $msg;
  }
    }
}

# Determine our exit code
my $exit_code = $OK;
if ($nagios_level_count{UNKNOWN} > 0)  { $exit_code = $UNKNOWN;  }
if ($nagios_level_count{WARNING} > 0)  { $exit_code = $WARNING;  }
if ($nagios_level_count{CRITICAL} > 0) { $exit_code = $CRITICAL; }

# Print OK message
if ($exit_code == $OK && !$opt{verbose}) {
    printf q{OK - Cluster '%s' is up, %d nodes, %d packages},
      $cluster{name}, scalar @nodes, scalar keys %package;
}
print "\n" if !$opt{verbose};

# Exit with proper exit code
exit $exit_code;


# Man page created with:
#
#  pod2man -s 3pm -r "`./check_serviceguard -V | head -n 1`" -c 'Nagios plugin' check_serviceguard check_serviceguard.3pm
#

__END__

=head1 NAME

check_serviceguard - Nagios plugin that checks HP ServiceGuard cluster

=head1 SYNOPSIS

check_serviceguard [I<OPTION>...]

=head1 DESCRIPTION

This is a plugin for Nagios that checks the overall health and various
aspects of a HP ServiceGuard cluster. The plugin checks the cluster
status, that all nodes and packages are up and running, that package
services are running etc.

The plugin tries to be smart and minimize the output. Only the first
node (in the node list) that are up and running will report any
errors. The other nodes will report that everything is ok.

=head1 OPTIONS

=over 4

=item --primary, --no-primary

Check that packages are running on their primary nodes. This is turned
off by default.

=item --autorun, --no-autorun

Enable or disable the check of auto_run for the packages. This is
turned on by default.

=item -b, --blacklist I<STRING> or I<FILE>

Blacklist one or more packages, e.g. test packages. Blacklisted
packages are completely ignored. The parameter is either the blacklist
string, or a file (that may or may not exist) containing the
string. The blacklist string contains package names separated by comma
(,). This option can be specified multiple times.

=item --state

Prefix each alert with its corresponding Nagios state. This is useful
in case of several alerts from the same cluster.

=item --short-state

Same as the --state option above, except that the state is abbreviated
to a single letter (W=warning, C=critical etc.).

=item -t, --timeout I<SECONDS>

The number of seconds after which the plugin will abort. Default
timeout is 30 seconds if the option is not present.

=item --linebreak I<STRING>

check_serviceguard will sometimes report more than one line, e.g. if
there are several alerts. If the script has a TTY, it will use regular
linebreaks. If not (which is the case with NRPE) it will use HTML
linebreaks. Sometimes it can be useful to control what the plugin uses
as a line separator, and this option provides that control.

The argument is the exact string to be used as the line
separator. There are two exceptions, i.e. two keywords that translates
to the following:

=over 4

=item B<REG>

Regular linebreaks, i.e. "\n".

=item B<HTML>

HTML linebreaks, i.e. "<br/>".

=back

This is a rather special option that is normally not needed. The
default behaviour should be sufficient for most users.

=item -v, --verbose

Verbose output. Will report status on everything, even if status is
ok. Blacklisted packages are ignored (i.e. no output).

=item -h, --help

Display help text.

=item -m, --man

Display man page.

=item -V, --version

Display version info.

=back

=head1 DIAGNOSTICS

The option I<--verbose> can be specified to display all monitored
components and their associated alert states.

=head1 DEPENDENCIES

A regular perl distribution is required to run the script.

=head1 EXIT STATUS

If no errors are discovered, a value of 0 (OK) is returned. An exit
value of 1 (WARNING) signifies one or more non-critical errors, while
2 (CRITICAL) signifies one or more critical errors.

The exit value 3 (UNKNOWN) is reserved for errors within the script,
or errors getting values from HP ServiceGuard.

=head1 AUTHOR

Written by Trond H. Amundsen <t.h.amundsen@usit.uio.no>

=head1 BUGS AND LIMITATIONS

Only tested on RHEL4/SG11.16, RHEL5/SG11.18 and HP-UX11/SG11.17. The
plugin assumes that only one cluster is configured on the node,
i.e. that a node is member of exactly one cluster.

=head1 REPORTING BUGS

Report bugs to <t.h.amundsen@usit.uio.no>

=head1 LICENSE AND COPYRIGHT

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see L<http://www.gnu.org/licenses/>.

=head1 SEE ALSO

L<http://folk.uio.no/trondham/software/check_serviceguard.html>

=cut


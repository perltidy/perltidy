# Created with: ./make_t.pl

# Contents:
#1 else1.def
#2 else2.def

# To locate test #13 you can search for its name or the string '#13'

use strict;
use Test;
use Carp;
use Perl::Tidy;
my $rparams;
my $rsources;
my $rtests;

BEGIN {

    ###########################################
    # BEGIN SECTION 1: Parameter combinations #
    ###########################################
    $rparams = { 'def' => "", };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

        'else1' => <<'----------',
# pad after 'if' when followed by 'elsif'
if    ( not defined $dir or not length $dir ) { $rslt = ''; }
elsif ( $dir =~ /^\$\([^\)]+\)\Z(?!\n)/s )    { $rslt = $dir; }
else                                          { $rslt = vmspath($dir); }
----------

        'else2' => <<'----------',
	# no pad after 'if' when followed by 'else'
        if ( $m = $g[$x][$y] ) { print $$m{v}; $$m{i}->() }
        else                   { print " " }
----------
    };

    ####################################
    # BEGIN SECTION 3: Expected output #
    ####################################
    $rtests = {

        'else1.def' => {
            source => "else1",
            params => "def",
            expect => <<'#1...........',
# pad after 'if' when followed by 'elsif'
if    ( not defined $dir or not length $dir ) { $rslt = ''; }
elsif ( $dir =~ /^\$\([^\)]+\)\Z(?!\n)/s )    { $rslt = $dir; }
else                                          { $rslt = vmspath($dir); }
#1...........
        },

        'else2.def' => {
            source => "else2",
            params => "def",
            expect => <<'#2...........',
        # no pad after 'if' when followed by 'else'
        if ( $m = $g[$x][$y] ) { print $$m{v}; $$m{i}->() }
        else                   { print " " }
#2...........
        },
    };

    my $ntests = 0 + keys %{$rtests};
    plan tests => $ntests;
}

###############
# EXECUTE TESTS
###############

foreach my $key ( sort keys %{$rtests} ) {
    my $output;
    my $sname  = $rtests->{$key}->{source};
    my $expect = $rtests->{$key}->{expect};
    my $pname  = $rtests->{$key}->{params};
    my $source = $rsources->{$sname};
    my $params = defined($pname) ? $rparams->{$pname} : "";
    my $stderr_string;
    my $errorfile_string;
    my $err = Perl::Tidy::perltidy(
        source      => \$source,
        destination => \$output,
        perltidyrc  => \$params,
        argv        => '',             # for safety; hide any ARGV from perltidy
        stderr      => \$stderr_string,
        errorfile => \$errorfile_string,    # not used when -se flag is set
    );
    if ( $err || $stderr_string || $errorfile_string ) {
        if ($err) {
            print STDERR
"This error received calling Perl::Tidy with '$sname' + '$pname'\n";
            ok( !$err );
        }
        if ($stderr_string) {
            print STDERR "---------------------\n";
            print STDERR "<<STDERR>>\n$stderr_string\n";
            print STDERR "---------------------\n";
            print STDERR
"This error received calling Perl::Tidy with '$sname' + '$pname'\n";
            ok( !$stderr_string );
        }
        if ($errorfile_string) {
            print STDERR "---------------------\n";
            print STDERR "<<.ERR file>>\n$errorfile_string\n";
            print STDERR "---------------------\n";
            print STDERR
"This error received calling Perl::Tidy with '$sname' + '$pname'\n";
            ok( !$errorfile_string );
        }
    }
    else {
        ok( $output, $expect );
    }
}

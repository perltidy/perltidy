# Created with: ./make_t.pl

# Contents:
#1 spp.spp1
#2 spp.spp2
#3 git16.def
#4 git10.def
#5 git10.git10
#6 multiple_equals.def

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
    $rparams = {
        'def'   => "",
        'git10' => "-wn -ce -cbl=sort,map,grep",
        'spp1'  => "-spp=1",
        'spp2'  => "-spp=2",
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

        'git10' => <<'----------',
# perltidy -wn -ce -cbl=sort,map,grep
@sorted = map {
    $_->[0]
} sort {
    $a->[1] <=> $b->[1] or $a->[0] cmp $b->[0]
} map {
    [ $_, length($_) ]
} @unsorted;
----------

        'git16' => <<'----------',
# git#16, two equality lines with fat commas on the right
my $Package = $Self->RepositoryGet( %Param, Result => 'SCALAR' );
my %Structure = $Self->PackageParse( String => $Package );
----------

        'multiple_equals' => <<'----------',
# ignore second '=' here
$|          = $debug = 1 if $opt_d;
$full_index = 1          if $opt_i;
$query_all  = $opt_A     if $opt_A;

# align multiple '='s here
$start   = $end     = $len = $ismut = $number = $allele_ori = $allele_mut =
  $proof = $xxxxreg = $reg = $dist  = '';
----------

        'spp' => <<'----------',
sub get_val() { }

sub get_Val  () { }

sub Get_val		() { }
----------
    };

    ####################################
    # BEGIN SECTION 3: Expected output #
    ####################################
    $rtests = {

        'spp.spp1' => {
            source => "spp",
            params => "spp1",
            expect => <<'#1...........',
sub get_val() { }

sub get_Val () { }

sub Get_val () { }
#1...........
        },

        'spp.spp2' => {
            source => "spp",
            params => "spp2",
            expect => <<'#2...........',
sub get_val () { }

sub get_Val () { }

sub Get_val () { }
#2...........
        },

        'git16.def' => {
            source => "git16",
            params => "def",
            expect => <<'#3...........',
# git#16, two equality lines with fat commas on the right
my $Package   = $Self->RepositoryGet( %Param, Result => 'SCALAR' );
my %Structure = $Self->PackageParse( String => $Package );
#3...........
        },

        'git10.def' => {
            source => "git10",
            params => "def",
            expect => <<'#4...........',
# perltidy -wn -ce -cbl=sort,map,grep
@sorted =
  map  { $_->[0] }
  sort { $a->[1] <=> $b->[1] or $a->[0] cmp $b->[0] }
  map  { [ $_, length($_) ] } @unsorted;
#4...........
        },

        'git10.git10' => {
            source => "git10",
            params => "git10",
            expect => <<'#5...........',
# perltidy -wn -ce -cbl=sort,map,grep
@sorted = map {
    $_->[0]
} sort {
    $a->[1] <=> $b->[1] or $a->[0] cmp $b->[0]
} map {
    [ $_, length($_) ]
} @unsorted;
#5...........
        },

        'multiple_equals.def' => {
            source => "multiple_equals",
            params => "def",
            expect => <<'#6...........',
# ignore second '=' here
$|          = $debug = 1 if $opt_d;
$full_index = 1          if $opt_i;
$query_all  = $opt_A     if $opt_A;

# align multiple '='s here
$start   = $end     = $len = $ismut = $number = $allele_ori = $allele_mut =
  $proof = $xxxxreg = $reg = $dist  = '';
#6...........
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

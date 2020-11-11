# Created with: ./make_t.pl

# Contents:
#1 boa.def
#2 bol.bol
#3 bol.def
#4 bot.bot
#5 bot.def
#6 hash_bang.def
#7 hash_bang.hash_bang
#8 listop1.listop1
#9 sbcp.def
#10 sbcp.sbcp1

# To locate test #13 you can search for its name or the string '#13'

use strict;
use Test::More;
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
        'bol' => <<'----------',
# -bol is default, so test -nbol
-nbol
----------
        'bot' => <<'----------',
# -bot is default so we test -nbot
-nbot
----------
        'def'       => "",
        'hash_bang' => "-x",
        'listop1'   => <<'----------',
# -bok is default so we test nbok
-nbok
----------
        'sbcp1' => <<'----------',
-sbc -sbcp='#x#'
----------
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

        'boa' => <<'----------',
my @field
  : field
  : Default(1)
  : Get('Name' => 'foo') 
  : Set('Name');
----------

        'bol' => <<'----------',
return unless $cmd = $cmd || ($dot 
          && $Last_Shell) || &prompt('|');
----------

        'bot' => <<'----------',
$foo =
  $condition
  ? undef
  : 1;
----------

        'hash_bang' => <<'----------',




# above spaces will be retained with -x but not by default
#!/usr/bin/perl
my $date = localtime();
----------

        'listop1' => <<'----------',
my @sorted = map { $_->[0] }
  sort { $a->[1] <=> $b->[1] }
  map { [ $_, rand ] } @list;
----------

        'sbcp' => <<'----------',
@month_of_year = (
    'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct',
#x# 'Dec', 'Nov'
## 'Dec', 'Nov'
    'Nov', 'Dec'
);
----------
    };

    ####################################
    # BEGIN SECTION 3: Expected output #
    ####################################
    $rtests = {

        'boa.def' => {
            source => "boa",
            params => "def",
            expect => <<'#1...........',
my @field
  : field
  : Default(1)
  : Get('Name' => 'foo')
  : Set('Name');
#1...........
        },

        'bol.bol' => {
            source => "bol",
            params => "bol",
            expect => <<'#2...........',
return unless $cmd = $cmd || ( $dot && $Last_Shell ) || &prompt('|');
#2...........
        },

        'bol.def' => {
            source => "bol",
            params => "def",
            expect => <<'#3...........',
return
  unless $cmd = $cmd
  || ( $dot
    && $Last_Shell )
  || &prompt('|');
#3...........
        },

        'bot.bot' => {
            source => "bot",
            params => "bot",
            expect => <<'#4...........',
$foo = $condition ? undef : 1;
#4...........
        },

        'bot.def' => {
            source => "bot",
            params => "def",
            expect => <<'#5...........',
$foo =
  $condition
  ? undef
  : 1;
#5...........
        },

        'hash_bang.def' => {
            source => "hash_bang",
            params => "def",
            expect => <<'#6...........',

# above spaces will be retained with -x but not by default
#!/usr/bin/perl
my $date = localtime();
#6...........
        },

        'hash_bang.hash_bang' => {
            source => "hash_bang",
            params => "hash_bang",
            expect => <<'#7...........',




# above spaces will be retained with -x but not by default
#!/usr/bin/perl
my $date = localtime();
#7...........
        },

        'listop1.listop1' => {
            source => "listop1",
            params => "listop1",
            expect => <<'#8...........',
my @sorted =
  map { $_->[0] } sort { $a->[1] <=> $b->[1] } map { [ $_, rand ] } @list;
#8...........
        },

        'sbcp.def' => {
            source => "sbcp",
            params => "def",
            expect => <<'#9...........',
@month_of_year = (
    'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct',

    #x# 'Dec', 'Nov'
## 'Dec', 'Nov'
    'Nov', 'Dec'
);
#9...........
        },

        'sbcp.sbcp1' => {
            source => "sbcp",
            params => "sbcp1",
            expect => <<'#10...........',
@month_of_year = (
    'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct',
#x# 'Dec', 'Nov'
    ## 'Dec', 'Nov'
    'Nov', 'Dec'
);
#10...........
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
        errorfile   => \$errorfile_string,    # not used when -se flag is set
    );
    if ( $err || $stderr_string || $errorfile_string ) {
        print STDERR "Error output received for test '$key'\n";
        if ($err) {
            print STDERR "An error flag '$err' was returned\n";
            ok( !$err );
        }
        if ($stderr_string) {
            print STDERR "---------------------\n";
            print STDERR "<<STDERR>>\n$stderr_string\n";
            print STDERR "---------------------\n";
            ok( !$stderr_string );
        }
        if ($errorfile_string) {
            print STDERR "---------------------\n";
            print STDERR "<<.ERR file>>\n$errorfile_string\n";
            print STDERR "---------------------\n";
            ok( !$errorfile_string );
        }
    }
    else {
        if ( !is( $output, $expect, $key ) ) {
            my $leno = length($output);
            my $lene = length($expect);
            if ( $leno == $lene ) {
                print STDERR
"#> Test '$key' gave unexpected output.  Strings differ but both have length $leno\n";
            }
            else {
                print STDERR
"#> Test '$key' gave unexpected output.  String lengths differ: output=$leno, expected=$lene\n";
            }
        }
    }
}

# Created with: ./make_t.pl

# Contents:
#1 rt123749.rt123749
#2 rt123774.def
#3 rt124114.def
#4 rt124354.def
#5 rt124354.rt124354
#6 rt125012.def
#7 rt125012.rt125012
#8 rt125506.def
#9 rt125506.rt125506
#10 rt126965.def
#11 rt15735.def
#12 rt18318.def
#13 rt18318.rt18318
#14 rt27000.def
#15 rt31741.def
#16 rt49289.def
#17 rt50702.def
#18 rt50702.rt50702
#19 rt68870.def
#20 rt70747.def

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
        'def'      => "",
        'rt123749' => "-wn",
        'rt124354' => "-io",
        'rt125012' => <<'----------',
-mangle
-dac
----------
        'rt125506' => "-io",
        'rt18318'  => <<'----------',
-nwrs='A'
----------
        'rt50702' => <<'----------',
-wbb='='
----------
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

        'rt123749' => <<'----------',
get('http://mojolicious.org')->then(
    sub {
        my $mojo = shift;
        say $mojo->res->code;
        return get('http://metacpan.org');
    }
)->then(
    sub {
        my $cpan = shift;
        say $cpan->res->code;
    }
)->catch(
    sub {
        my $err = shift;
        warn "Something went wrong: $err";
    }
)->wait;
----------

        'rt123774' => <<'----------',
# retain any space between backslash and quote to avoid fooling html formatters
my $var1 = \ "bubba";
my $var2 = \"bubba";
my $var3 = \ 'bubba';
my $var4 = \'bubba';
my $var5 = \            "bubba";
----------

        'rt124114' => <<'----------',
#!/usr/bin/perl 
my %h = {
    a    => 2 > 3 ? 1 : 0,
    bbbb => sub { my $y = "1" },
    c    => sub { my $z = "2" },
    d    => 2 > 3 ? 1 : 0,
};
----------

        'rt124354' => <<'----------',
package Foo;

use Moose;

has a => ( is => 'ro', isa => 'Int' );
has b => ( is => 'ro', isa => 'Int' );
has c => ( is => 'ro', isa => 'Int' );

__PACKAGE__->meta->make_immutable;
----------

        'rt125012' => <<'----------',
++$_ for
#one space before eol:
values %_;
system
#one space before eol:
qq{};
----------

        'rt125506' => <<'----------',
my $t = '
        un
        deux
        trois
	';
----------

        'rt126965' => <<'----------',
my $restrict_customer = shift ? 1 : 0;
----------

        'rt15735' => <<'----------',
my $user_prefs = $ref_type eq 'SCALAR' ? _load_from_string( $profile ) : $ref_type eq 'ARRAY' ? _load_from_array( $profile ) : $ref_type eq 'HASH' ? _load_from_hash( $profile ) : _load_from_file( $profile );
----------

        'rt18318' => <<'----------',
# Class::Std attribute list
# The token type of the first colon is 'A' so use -nwrs='A' to avoid space
# after it
my %rank_of : ATTR( :init_arg<starting_rank>  :get<rank>  :set<rank> );
----------

        'rt27000' => <<'----------',
print add( 3, 4 ), "\n";
print add( 4, 3 ), "\n";

sub add {
    my ( $term1, $term2 ) = @_;
# line 1234
    die "$term1 > $term2" if $term1 > $term2;
    return $term1 + $term2;
}
----------

        'rt31741' => <<'----------',
$msg //= 'World';
----------

        'rt49289' => <<'----------',
use constant qw{ DEBUG 0 };
----------

        'rt50702' => <<'----------',
if (1) { my $uid = $ENV{ 'ORIG_LOGNAME' } || $ENV{ 'LOGNAME' } || $ENV{ 'REMOTE_USER' } || 'foobar'; } if (2) { my $uid = ($ENV{ 'ORIG_LOGNAME' } || $ENV{ 'LOGNAME' } || $ENV{ 'REMOTE_USER' } || 'foobar'); }
----------

        'rt68870' => <<'----------',
s///r;
----------

        'rt70747' => <<'----------',
coerce Q2RawStatGroupArray, from ArrayRef [Q2StatGroup], via {
  [ map {
      my $g = $_->as_hash;
      $g->{stats} = [ map { scalar $_->as_array } @{ $g->{stats} } ]; $g;
    } @$_
  ]
};
----------
    };

    ####################################
    # BEGIN SECTION 3: Expected output #
    ####################################
    $rtests = {

        'rt123749.rt123749' => {
            source => "rt123749",
            params => "rt123749",
            expect => <<'#1...........',
get('http://mojolicious.org')->then( sub {
    my $mojo = shift;
    say $mojo->res->code;
    return get('http://metacpan.org');
} )->then( sub {
    my $cpan = shift;
    say $cpan->res->code;
} )->catch( sub {
    my $err = shift;
    warn "Something went wrong: $err";
} )->wait;
#1...........
        },

        'rt123774.def' => {
            source => "rt123774",
            params => "def",
            expect => <<'#2...........',
# retain any space between backslash and quote to avoid fooling html formatters
my $var1 = \ "bubba";
my $var2 = \"bubba";
my $var3 = \ 'bubba';
my $var4 = \'bubba';
my $var5 = \ "bubba";
#2...........
        },

        'rt124114.def' => {
            source => "rt124114",
            params => "def",
            expect => <<'#3...........',
#!/usr/bin/perl 
my %h = {
    a    => 2 > 3 ? 1 : 0,
    bbbb => sub { my $y = "1" },
    c    => sub { my $z = "2" },
    d    => 2 > 3 ? 1 : 0,
};
#3...........
        },

        'rt124354.def' => {
            source => "rt124354",
            params => "def",
            expect => <<'#4...........',
package Foo;

use Moose;

has a => ( is => 'ro', isa => 'Int' );
has b => ( is => 'ro', isa => 'Int' );
has c => ( is => 'ro', isa => 'Int' );

__PACKAGE__->meta->make_immutable;
#4...........
        },

        'rt124354.rt124354' => {
            source => "rt124354",
            params => "rt124354",
            expect => <<'#5...........',
package Foo;

use Moose;

has a => ( is => 'ro', isa => 'Int' );
has b => ( is => 'ro', isa => 'Int' );
has c => ( is => 'ro', isa => 'Int' );

__PACKAGE__->meta->make_immutable;
#5...........
        },

        'rt125012.def' => {
            source => "rt125012",
            params => "def",
            expect => <<'#6...........',
++$_ for

  #one space before eol:
  values %_;
system

  #one space before eol:
  qq{};
#6...........
        },

        'rt125012.rt125012' => {
            source => "rt125012",
            params => "rt125012",
            expect => <<'#7...........',
++$_ for
  values%_;
system
  qq{};
#7...........
        },

        'rt125506.def' => {
            source => "rt125506",
            params => "def",
            expect => <<'#8...........',
my $t = '
        un
        deux
        trois
	';
#8...........
        },

        'rt125506.rt125506' => {
            source => "rt125506",
            params => "rt125506",
            expect => <<'#9...........',
my $t = '
        un
        deux
        trois
	';
#9...........
        },

        'rt126965.def' => {
            source => "rt126965",
            params => "def",
            expect => <<'#10...........',
my $restrict_customer = shift ? 1 : 0;
#10...........
        },

        'rt15735.def' => {
            source => "rt15735",
            params => "def",
            expect => <<'#11...........',
my $user_prefs =
    $ref_type eq 'SCALAR' ? _load_from_string($profile)
  : $ref_type eq 'ARRAY'  ? _load_from_array($profile)
  : $ref_type eq 'HASH'   ? _load_from_hash($profile)
  :                         _load_from_file($profile);
#11...........
        },

        'rt18318.def' => {
            source => "rt18318",
            params => "def",
            expect => <<'#12...........',
# Class::Std attribute list
# The token type of the first colon is 'A' so use -nwrs='A' to avoid space
# after it
my %rank_of : ATTR( :init_arg<starting_rank>  :get<rank>  :set<rank> );
#12...........
        },

        'rt18318.rt18318' => {
            source => "rt18318",
            params => "rt18318",
            expect => <<'#13...........',
# Class::Std attribute list
# The token type of the first colon is 'A' so use -nwrs='A' to avoid space
# after it
my %rank_of :ATTR( :init_arg<starting_rank>  :get<rank>  :set<rank> );
#13...........
        },

        'rt27000.def' => {
            source => "rt27000",
            params => "def",
            expect => <<'#14...........',
print add( 3, 4 ), "\n";
print add( 4, 3 ), "\n";

sub add {
    my ( $term1, $term2 ) = @_;
# line 1234
    die "$term1 > $term2" if $term1 > $term2;
    return $term1 + $term2;
}
#14...........
        },

        'rt31741.def' => {
            source => "rt31741",
            params => "def",
            expect => <<'#15...........',
$msg //= 'World';
#15...........
        },

        'rt49289.def' => {
            source => "rt49289",
            params => "def",
            expect => <<'#16...........',
use constant qw{ DEBUG 0 };
#16...........
        },

        'rt50702.def' => {
            source => "rt50702",
            params => "def",
            expect => <<'#17...........',
if (1) {
    my $uid =
         $ENV{'ORIG_LOGNAME'}
      || $ENV{'LOGNAME'}
      || $ENV{'REMOTE_USER'}
      || 'foobar';
}
if (2) {
    my $uid =
      (      $ENV{'ORIG_LOGNAME'}
          || $ENV{'LOGNAME'}
          || $ENV{'REMOTE_USER'}
          || 'foobar' );
}
#17...........
        },

        'rt50702.rt50702' => {
            source => "rt50702",
            params => "rt50702",
            expect => <<'#18...........',
if (1) {
    my $uid
      = $ENV{'ORIG_LOGNAME'}
      || $ENV{'LOGNAME'}
      || $ENV{'REMOTE_USER'}
      || 'foobar';
}
if (2) {
    my $uid
      = (    $ENV{'ORIG_LOGNAME'}
          || $ENV{'LOGNAME'}
          || $ENV{'REMOTE_USER'}
          || 'foobar' );
}
#18...........
        },

        'rt68870.def' => {
            source => "rt68870",
            params => "def",
            expect => <<'#19...........',
s///r;
#19...........
        },

        'rt70747.def' => {
            source => "rt70747",
            params => "def",
            expect => <<'#20...........',
coerce Q2RawStatGroupArray, from ArrayRef [Q2StatGroup], via {
    [
        map {
            my $g = $_->as_hash;
            $g->{stats} = [ map { scalar $_->as_array } @{ $g->{stats} } ];
            $g;
        } @$_
    ]
};
#20...........
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

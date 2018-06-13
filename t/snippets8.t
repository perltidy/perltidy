# **This script was automatically generated**
# Created with: ./make_t.pl
# Tue Jun 12 19:09:24 2018

# To locate test #13 for example, search for the string '#13'

use strict;
use Test;
use Carp;
use Perl::Tidy;
my $rparams;
my $rsources;
my $rtests;

BEGIN {

    #####################################
    # SECTION 1: Parameter combinations #
    #####################################
    $rparams = {
        'def'      => "",
        'rt123749' => "-wn",
        'rt124354' => "-io",
        'rt125012' => <<'----------',
-mangle
-dac
----------
        'rt125506' => "-io",
        'rt50702'  => <<'----------',
-wbb='='
----------
        'rt70747' => "-i=2",
    };

    ######################
    # SECTION 2: Sources #
    ######################
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

        'rt15735' => <<'----------',
my $user_prefs = $ref_type eq 'SCALAR' ? _load_from_string( $profile ) : $ref_type eq 'ARRAY' ? _load_from_array( $profile ) : $ref_type eq 'HASH' ? _load_from_hash( $profile ) : _load_from_file( $profile );
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
    } @$_;
  ]
};
----------

        'rt74856' => <<'----------',
{
my $foo = '1';
#<<< 
my $bar = (test())
 ? 'some value'
 : undef;
#>>> 
my $baz = 'something else';
}
----------

        'rt78156' => <<'----------',
package Some::Class 2.012;
----------
    };

    ##############################
    # SECTION 3: Expected output #
    ##############################
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
++$_ for values%_;
system qq{};
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

        'rt15735.def' => {
            source => "rt15735",
            params => "def",
            expect => <<'#10...........',
my $user_prefs =
    $ref_type eq 'SCALAR' ? _load_from_string($profile)
  : $ref_type eq 'ARRAY'  ? _load_from_array($profile)
  : $ref_type eq 'HASH'   ? _load_from_hash($profile)
  :                         _load_from_file($profile);
#10...........
        },

        'rt27000.def' => {
            source => "rt27000",
            params => "def",
            expect => <<'#11...........',
print add( 3, 4 ), "\n";
print add( 4, 3 ), "\n";

sub add {
    my ( $term1, $term2 ) = @_;
# line 1234
    die "$term1 > $term2" if $term1 > $term2;
    return $term1 + $term2;
}
#11...........
        },

        'rt31741.def' => {
            source => "rt31741",
            params => "def",
            expect => <<'#12...........',
$msg //= 'World';
#12...........
        },

        'rt49289.def' => {
            source => "rt49289",
            params => "def",
            expect => <<'#13...........',
use constant qw{ DEBUG 0 };
#13...........
        },

        'rt50702.def' => {
            source => "rt50702",
            params => "def",
            expect => <<'#14...........',
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
#14...........
        },

        'rt50702.rt50702' => {
            source => "rt50702",
            params => "rt50702",
            expect => <<'#15...........',
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
#15...........
        },

        'rt68870.def' => {
            source => "rt68870",
            params => "def",
            expect => <<'#16...........',
s///r;
#16...........
        },

        'rt70747.def' => {
            source => "rt70747",
            params => "def",
            expect => <<'#17...........',
coerce Q2RawStatGroupArray, from ArrayRef [Q2StatGroup], via {
    [
        map {
            my $g = $_->as_hash;
            $g->{stats} = [ map { scalar $_->as_array } @{ $g->{stats} } ];
            $g;
        } @$_;
    ]
};
#17...........
        },

        'rt70747.rt70747' => {
            source => "rt70747",
            params => "rt70747",
            expect => <<'#18...........',
coerce Q2RawStatGroupArray, from ArrayRef [Q2StatGroup], via {
  [
    map {
      my $g = $_->as_hash;
      $g->{stats} = [ map { scalar $_->as_array } @{ $g->{stats} } ];
      $g;
    } @$_;
  ]
};
#18...........
        },

        'rt74856.def' => {
            source => "rt74856",
            params => "def",
            expect => <<'#19...........',
{
    my $foo = '1';
#<<< 
my $bar = (test())
 ? 'some value'
 : undef;
#>>> 
    my $baz = 'something else';
}
#19...........
        },

        'rt78156.def' => {
            source => "rt78156",
            params => "def",
            expect => <<'#20...........',
package Some::Class 2.012;
#20...........
        },
    };

    my $ntests = 0 + keys %{$rtests};
    plan tests => $ntests;
}

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

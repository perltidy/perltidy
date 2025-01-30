# Created with: ./make_t.pl

# Contents:
#1 rt70747.rt70747
#2 rt74856.def
#3 rt78156.def
#4 rt78764.def
#5 rt79813.def
#6 rt79947.def
#7 rt80645.def
#8 rt81852.def
#9 rt81852.rt81852
#10 rt81854.def
#11 rt87502.def
#12 rt93197.def
#13 rt94338.def
#14 rt95419.def
#15 rt95708.def
#16 rt96021.def
#17 rt96101.def
#18 rt98902.def
#19 rt98902.rt98902
#20 rt99961.def

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
        'def'     => "",
        'rt70747' => "-i=2",
        'rt81852' => <<'----------',
-wn
-act=2
----------
        'rt98902' => "-boc",
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

        'rt70747' => <<'----------',
coerce Q2RawStatGroupArray, from ArrayRef [Q2StatGroup], via {
  [ map {
      my $g = $_->as_hash;
      $g->{stats} = [ map { scalar $_->as_array } @{ $g->{stats} } ]; $g;
    } @$_
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

        'rt78764' => <<'----------',
qr/3/ ~~ ['1234'] ? 1 : 0;
map { $_ ~~ [ '0', '1' ] ? 'x' : 'o' } @a;
----------

        'rt79813' => <<'----------',
my %hash = ( a => { bbbbbbbbb => {
            cccccccccc => 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx',
        }, },);
----------

        'rt79947' => <<'----------',
try { croak "An Error!"; }
catch ($error) {
    print STDERR $error . "\n";
}
----------

        'rt80645' => <<'----------',
BEGIN { $^W = 1; }
use warnings;
use strict;
@$ = 'test';
print $#{$};
----------

        'rt81852' => <<'----------',
do {
    {
        next if ( $n % 2 );
        print $n, "\n";
    }
} while ( $n++ < 10 );
----------

        'rt81854' => <<'----------',
return "this is a descriptive error message"
  if $res->is_error or not length $data;
----------

        'rt87502' => <<'----------',
if ( @ARGV ~~ { map { $_ => 1 } qw(re restart reload) } ) { 
    # CODE
}
----------

        'rt93197' => <<'----------',
$to = $to->{$_} ||= {} for @key; if (1) {2;} else {3;}
----------

        'rt94338' => <<'----------',
# for-loop in a parenthesized block-map triggered an error message
map( { foreach my $item ( '0', '1' ) { print $item} } qw(a b c) );
----------

        'rt95419' => <<'----------',
case "blah" => sub {
    { a => 1 }
};
----------

        'rt95708' => <<'----------',
use strict;
use JSON;
my $ref = { 
when => time(), message => 'abc' };
my $json  = encode_json   { 
when => time(), message => 'abc' };
my $json2 = encode_json + { 
when => time(), message => 'abc' };
----------

        'rt96021' => <<'----------',
$a->@*;
$a->**;
$a->$*;
$a->&*;
$a->%*;
$a->$#*
----------

        'rt96101' => <<'----------',
# Example for rt.cpan.org #96101; Perltidy not properly formatting subroutine
# references inside subroutine execution.

# closing brace of second sub should get outdented here
sub startup {
    my $self = shift;
    $self->plugin(
        'authentication' => {
            'autoload_user' => 1,
            'session_key'   => rand(),
            'load_user'     => sub {
                return HaloVP::Users->load(@_);
            },
            'validate_user' => sub {
                return HaloVP::Users->login(@_);
            }
        }
    );
}

----------

        'rt98902' => <<'----------',
my %foo = ( 
   alpha => 1, 
beta => 2, gamma => 3, 
);

my @bar = map { { 
number => $_, 
character => chr $_, 
padding => ( ' ' x $_ ), 
} } ( 0 .. 32 );
----------

        'rt99961' => <<'----------',
%thing = %{ print qq[blah1\n]; $b; };
----------
    };

    ####################################
    # BEGIN SECTION 3: Expected output #
    ####################################
    $rtests = {

        'rt70747.rt70747' => {
            source => "rt70747",
            params => "rt70747",
            expect => <<'#1...........',
coerce Q2RawStatGroupArray, from ArrayRef [Q2StatGroup], via {
  [
    map {
      my $g = $_->as_hash;
      $g->{stats} = [ map { scalar $_->as_array } @{ $g->{stats} } ];
      $g;
    } @$_
  ]
};
#1...........
        },

        'rt74856.def' => {
            source => "rt74856",
            params => "def",
            expect => <<'#2...........',
{
    my $foo = '1';
#<<< 
my $bar = (test())
 ? 'some value'
 : undef;
#>>> 
    my $baz = 'something else';
}
#2...........
        },

        'rt78156.def' => {
            source => "rt78156",
            params => "def",
            expect => <<'#3...........',
package Some::Class 2.012;
#3...........
        },

        'rt78764.def' => {
            source => "rt78764",
            params => "def",
            expect => <<'#4...........',
qr/3/ ~~ ['1234'] ? 1 : 0;
map { $_ ~~ [ '0', '1' ] ? 'x' : 'o' } @a;
#4...........
        },

        'rt79813.def' => {
            source => "rt79813",
            params => "def",
            expect => <<'#5...........',
my %hash = (
    a => {
        bbbbbbbbb => {
            cccccccccc => 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx',
        },
    },
);
#5...........
        },

        'rt79947.def' => {
            source => "rt79947",
            params => "def",
            expect => <<'#6...........',
try { croak "An Error!"; }
catch ($error) {
    print STDERR $error . "\n";
}
#6...........
        },

        'rt80645.def' => {
            source => "rt80645",
            params => "def",
            expect => <<'#7...........',
BEGIN { $^W = 1; }
use warnings;
use strict;
@$ = 'test';
print $#{$};
#7...........
        },

        'rt81852.def' => {
            source => "rt81852",
            params => "def",
            expect => <<'#8...........',
do {
    {
        next if ( $n % 2 );
        print $n, "\n";
    }
} while ( $n++ < 10 );
#8...........
        },

        'rt81852.rt81852' => {
            source => "rt81852",
            params => "rt81852",
            expect => <<'#9...........',
do {{
    next if ($n % 2);
    print $n, "\n";
}} while ($n++ < 10);
#9...........
        },

        'rt81854.def' => {
            source => "rt81854",
            params => "def",
            expect => <<'#10...........',
return "this is a descriptive error message"
  if $res->is_error or not length $data;
#10...........
        },

        'rt87502.def' => {
            source => "rt87502",
            params => "def",
            expect => <<'#11...........',
if ( @ARGV ~~ { map { $_ => 1 } qw(re restart reload) } ) {

    # CODE
}
#11...........
        },

        'rt93197.def' => {
            source => "rt93197",
            params => "def",
            expect => <<'#12...........',
$to = $to->{$_} ||= {} for @key;
if   (1) { 2; }
else     { 3; }
#12...........
        },

        'rt94338.def' => {
            source => "rt94338",
            params => "def",
            expect => <<'#13...........',
# for-loop in a parenthesized block-map triggered an error message
map( {
        foreach my $item ( '0', '1' ) {
            print $item;
        }
} qw(a b c) );
#13...........
        },

        'rt95419.def' => {
            source => "rt95419",
            params => "def",
            expect => <<'#14...........',
case "blah" => sub {
    { a => 1 }
};
#14...........
        },

        'rt95708.def' => {
            source => "rt95708",
            params => "def",
            expect => <<'#15...........',
use strict;
use JSON;
my $ref = {
    when    => time(),
    message => 'abc'
};
my $json = encode_json {
    when    => time(),
    message => 'abc'
};
my $json2 = encode_json +{
    when    => time(),
    message => 'abc'
};
#15...........
        },

        'rt96021.def' => {
            source => "rt96021",
            params => "def",
            expect => <<'#16...........',
$a->@*;
$a->**;
$a->$*;
$a->&*;
$a->%*;
$a->$#*
#16...........
        },

        'rt96101.def' => {
            source => "rt96101",
            params => "def",
            expect => <<'#17...........',
# Example for rt.cpan.org #96101; Perltidy not properly formatting subroutine
# references inside subroutine execution.

# closing brace of second sub should get outdented here
sub startup {
    my $self = shift;
    $self->plugin(
        'authentication' => {
            'autoload_user' => 1,
            'session_key'   => rand(),
            'load_user'     => sub {
                return HaloVP::Users->load(@_);
            },
            'validate_user' => sub {
                return HaloVP::Users->login(@_);
            }
        }
    );
}

#17...........
        },

        'rt98902.def' => {
            source => "rt98902",
            params => "def",
            expect => <<'#18...........',
my %foo = (
    alpha => 1,
    beta  => 2,
    gamma => 3,
);

my @bar =
  map { { number => $_, character => chr $_, padding => ( ' ' x $_ ), } }
  ( 0 .. 32 );
#18...........
        },

        'rt98902.rt98902' => {
            source => "rt98902",
            params => "rt98902",
            expect => <<'#19...........',
my %foo = (
    alpha => 1,
    beta  => 2, gamma => 3,
);

my @bar = map {
    {
        number    => $_,
        character => chr $_,
        padding   => ( ' ' x $_ ),
    }
} ( 0 .. 32 );
#19...........
        },

        'rt99961.def' => {
            source => "rt99961",
            params => "def",
            expect => <<'#20...........',
%thing = %{
    print qq[blah1\n];
    $b;
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

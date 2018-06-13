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
        'def'     => "",
        'rt81852' => <<'----------',
-wn
-act=2
----------
        'rt98902' => "-boc",
        'scl'     => "-scl=12",
    };

    ######################
    # SECTION 2: Sources #
    ######################
    $rsources = {

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

        'scl' => <<'----------',
    # try -scl=12 to see '$returns' joined with the previous line
    $format = "format STDOUT =\n" . &format_line('Function:       @') . '$name' . "\n" . &format_line('Arguments:      @') . '$args' . "\n" . &format_line('Returns:        @') . '$returns' . "\n" . &format_line('             ~~ ^') . '$desc' . "\n.\n";
----------

        'semicolon2' => <<'----------',
	# will not add semicolon for this block type
        $highest = List::Util::reduce { Sort::Versions::versioncmp( $a, $b ) > 0 ? $a : $b }
----------
    };

    ##############################
    # SECTION 3: Expected output #
    ##############################
    $rtests = {

        'rt78764.def' => {
            source => "rt78764",
            params => "def",
            expect => <<'#1...........',
qr/3/ ~~ ['1234'] ? 1 : 0;
map { $_ ~~ [ '0', '1' ] ? 'x' : 'o' } @a;
#1...........
        },

        'rt79813.def' => {
            source => "rt79813",
            params => "def",
            expect => <<'#2...........',
my %hash = (
    a => {
        bbbbbbbbb => {
            cccccccccc => 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx',
        },
    },
);
#2...........
        },

        'rt79947.def' => {
            source => "rt79947",
            params => "def",
            expect => <<'#3...........',
try { croak "An Error!"; }
catch ($error) {
    print STDERR $error . "\n";
}
#3...........
        },

        'rt80645.def' => {
            source => "rt80645",
            params => "def",
            expect => <<'#4...........',
BEGIN { $^W = 1; }
use warnings;
use strict;
@$ = 'test';
print $#{$};
#4...........
        },

        'rt81852.def' => {
            source => "rt81852",
            params => "def",
            expect => <<'#5...........',
do {
    {
        next if ( $n % 2 );
        print $n, "\n";
    }
} while ( $n++ < 10 );
#5...........
        },

        'rt81852.rt81852' => {
            source => "rt81852",
            params => "rt81852",
            expect => <<'#6...........',
do {{
    next if ($n % 2);
    print $n, "\n";
}} while ($n++ < 10);
#6...........
        },

        'rt81854.def' => {
            source => "rt81854",
            params => "def",
            expect => <<'#7...........',
return "this is a descriptive error message"
  if $res->is_error or not length $data;
#7...........
        },

        'rt87502.def' => {
            source => "rt87502",
            params => "def",
            expect => <<'#8...........',
if ( @ARGV ~~ { map { $_ => 1 } qw(re restart reload) } ) {

    # CODE
}
#8...........
        },

        'rt93197.def' => {
            source => "rt93197",
            params => "def",
            expect => <<'#9...........',
$to = $to->{$_} ||= {} for @key;
if   (1) { 2; }
else     { 3; }
#9...........
        },

        'rt94338.def' => {
            source => "rt94338",
            params => "def",
            expect => <<'#10...........',
# for-loop in a parenthesized block-map triggered an error message
map( {
        foreach my $item ( '0', '1' ) {
            print $item;
        }
} qw(a b c) );
#10...........
        },

        'rt95419.def' => {
            source => "rt95419",
            params => "def",
            expect => <<'#11...........',
case "blah" => sub {
    { a => 1 }
};
#11...........
        },

        'rt95708.def' => {
            source => "rt95708",
            params => "def",
            expect => <<'#12...........',
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
my $json2 = encode_json + {
    when    => time(),
    message => 'abc'
};
#12...........
        },

        'rt96021.def' => {
            source => "rt96021",
            params => "def",
            expect => <<'#13...........',
$a->@*;
$a->**;
$a->$*;
$a->&*;
$a->%*;
$a->$#*
#13...........
        },

        'rt96101.def' => {
            source => "rt96101",
            params => "def",
            expect => <<'#14...........',
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

#14...........
        },

        'rt98902.def' => {
            source => "rt98902",
            params => "def",
            expect => <<'#15...........',
my %foo = (
    alpha => 1,
    beta  => 2,
    gamma => 3,
);

my @bar =
  map { { number => $_, character => chr $_, padding => ( ' ' x $_ ), } }
  ( 0 .. 32 );
#15...........
        },

        'rt98902.rt98902' => {
            source => "rt98902",
            params => "rt98902",
            expect => <<'#16...........',
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
#16...........
        },

        'rt99961.def' => {
            source => "rt99961",
            params => "def",
            expect => <<'#17...........',
%thing = %{
    print qq[blah1\n];
    $b;
};
#17...........
        },

        'scl.def' => {
            source => "scl",
            params => "def",
            expect => <<'#18...........',
    # try -scl=12 to see '$returns' joined with the previous line
    $format =
        "format STDOUT =\n"
      . &format_line('Function:       @') . '$name' . "\n"
      . &format_line('Arguments:      @') . '$args' . "\n"
      . &format_line('Returns:        @')
      . '$returns' . "\n"
      . &format_line('             ~~ ^') . '$desc' . "\n.\n";
#18...........
        },

        'scl.scl' => {
            source => "scl",
            params => "scl",
            expect => <<'#19...........',
    # try -scl=12 to see '$returns' joined with the previous line
    $format =
        "format STDOUT =\n"
      . &format_line('Function:       @') . '$name' . "\n"
      . &format_line('Arguments:      @') . '$args' . "\n"
      . &format_line('Returns:        @') . '$returns' . "\n"
      . &format_line('             ~~ ^') . '$desc' . "\n.\n";
#19...........
        },

        'semicolon2.def' => {
            source => "semicolon2",
            params => "def",
            expect => <<'#20...........',
        # will not add semicolon for this block type
        $highest = List::Util::reduce {
            Sort::Versions::versioncmp( $a, $b ) > 0 ? $a : $b
        }
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

# Created with: ./make_t.pl

# Contents:
#1 align10.def
#2 align11.def
#3 align12.def
#4 align13.def
#5 rt127633.def
#6 rt127633.rt127633
#7 align14.def
#8 align15.def
#9 align16.def
#10 break5.def
#11 align19.def
#12 align20.def
#13 align21.def
#14 align22.def
#15 align23.def
#16 align24.def
#17 align25.def
#18 align26.def
#19 align27.def

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
        'rt127633' => "-baao",
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

        'align10' => <<'----------',
$message =~ &rhs_wordwrap( $message, $width );
$message_len =~ split( /^/, $message );
----------

        'align11' => <<'----------',
my $accountno = getnextacctno( $env, $bornum, $dbh );
my $item = getiteminformation( $env, $itemno );
my $account = "Insert into accountlines
 bla bla";
----------

        'align12' => <<'----------',
    my $type = shift || "o";
    my $fname  = ( $type eq 'oo'               ? 'orte_city' : 'orte' );
    my $suffix = ( $coord_system eq 'standard' ? ''          : '-orig' );
----------

        'align13' => <<'----------',
# symbols =~ and !~ are equivalent in alignment
ok( $out !~ /EXACT <fop>/, "No 'baz'" );
ok( $out =~ /<liz>/,       "Got 'liz'" );    # liz
ok( $out =~ /<zoo>/,       "Got 'zoo'" );    # zoo
ok( $out !~ /<zap>/,       "Got 'zap'" );    # zap 
----------

        'align14' => <<'----------',
# align the =
my($apple)=new Fruit("Apple1",.1,.30);
my($grapefruit)=new Grapefruit("Grapefruit1",.3);
my($redgrapefruit)=new RedGrapefruit("Grapefruit2",.3);
----------

        'align15' => <<'----------',
# align both = and //
my$color=$opts{'-color'}//'black';
my$background=$opts{'-background'}//'none';
my$linewidth=$opts{'-linewidth'}//1;
my$radius=$opts{'-radius'}//0;
----------

        'align16' => <<'----------',
# align all at first =>
use constant {
    PHFAM => [ { John => 1, Jane => 2, Sally => 3 }, 33, 28, 3 ],
    FAMILY => [qw( John Jane Sally )],
    AGES   => { John => 33, Jane => 28, Sally => 3 },
    RFAM => [ [qw( John Jane Sally )] ],
    THREE => 3,
    SPIT  => sub { shift },
};

----------

        'align19' => <<'----------',
# different lhs patterns, do not align the '='
@_                                       = qw(sort grep map do eval);
@is_not_zero_continuation_block_type{@_} = (1) x scalar(@_);
----------

        'align20' => <<'----------',
# marginal two-line match; different lhs patterns; do not align
$w[$i] = $t;
$t = 1000000;
----------

        'align21' => <<'----------',
# two lines with large gap but same lhs pattern so align equals
local (@pieces)            = split( /\./, $filename, 2 );
local ($just_dir_and_base) = $pieces[0];

# two lines with 3 alignment tokens
$expect = "1$expect" if $expect =~ /^e/i;
$p = "1$p" if defined $p and $p =~ /^e/i;

# two lines where alignment causes a large gap
is( eval { sysopen( my $ro, $foo, &O_RDONLY | $TAINT0 ) }, undef );
is( $@, '' );
----------

        'align22' => <<'----------',
# two equality lines with different patterns to left of equals do not align
$signame{$_} = ++$signal;
$signum[$signal] = $_;
----------

        'align23' => <<'----------',
# two equality lines with same pattern on left of equals will align
my $orig = my $format = "^<<<<< ~~\n";
my $abc = "abc";
----------

        'align24' => <<'----------',
# Do not align interior fat commas here; different container types
my $p    = TAP::Parser::SubclassTest->new(
    {
        exec    => [ $cat            => $file ],
        sources => { MySourceHandler => { accept_all => 1 } },
    }
);
----------

        'align25' => <<'----------',
# do not align internal commas here; different container types
is_deeply( [ $a,        $a ], [ $b,               $c ] );
is_deeply( { foo => $a, bar => $a }, { foo => $b, bar => $c } );
is_deeply( [ \$a,       \$a ], [ \$b,             \$c ] );

----------

        'align26' => <<'----------',
#  align first of multiple equals
$SIG{PIPE}=sub{die"writingtoaclosedpipe"};
$SIG{BREAK}=$SIG{INT}=$SIG{TERM};
$SIG{HUP}=\&some_handler;
----------

        'align27' => <<'----------',
# do not align first equals here (unmatched commas on left side of =)
my ( $self, $name, $type ) = @_;
my $html_toc_fh            = $self->{_html_toc_fh};
my $html_prelim_fh            = $self->{_html_prelim_fh};
----------

        'break5' => <<'----------',
# do not break at .'s after the ?
return (
     $pod eq $pod2 
  )
  ? "\n&lt;A NAME=\""
  . $value
  . "\"&gt;\n$text&lt;/A&gt;\n"
  : "\n$type$pod2.html\#" . $value . "\"&gt;$text&lt;\/A&gt;\n";
----------

        'rt127633' => <<'----------',
# keep lines long; do not break after 'return' and '.' with -baoo
return $ref eq 'SCALAR' ? $self->encode_scalar( $object, $name, $type, $attr ) : $ref eq 'ARRAY';
my $s = 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa' .  'bbbbbbbbbbbbbbbbbbbbbbbbb';
----------
    };

    ####################################
    # BEGIN SECTION 3: Expected output #
    ####################################
    $rtests = {

        'align10.def' => {
            source => "align10",
            params => "def",
            expect => <<'#1...........',
$message     =~ &rhs_wordwrap( $message, $width );
$message_len =~ split( /^/, $message );
#1...........
        },

        'align11.def' => {
            source => "align11",
            params => "def",
            expect => <<'#2...........',
my $accountno = getnextacctno( $env, $bornum, $dbh );
my $item      = getiteminformation( $env, $itemno );
my $account   = "Insert into accountlines
 bla bla";
#2...........
        },

        'align12.def' => {
            source => "align12",
            params => "def",
            expect => <<'#3...........',
    my $type   = shift || "o";
    my $fname  = ( $type eq 'oo'               ? 'orte_city' : 'orte' );
    my $suffix = ( $coord_system eq 'standard' ? ''          : '-orig' );
#3...........
        },

        'align13.def' => {
            source => "align13",
            params => "def",
            expect => <<'#4...........',
# symbols =~ and !~ are equivalent in alignment
ok( $out !~ /EXACT <fop>/, "No 'baz'" );
ok( $out =~ /<liz>/,       "Got 'liz'" );    # liz
ok( $out =~ /<zoo>/,       "Got 'zoo'" );    # zoo
ok( $out !~ /<zap>/,       "Got 'zap'" );    # zap
#4...........
        },

        'rt127633.def' => {
            source => "rt127633",
            params => "def",
            expect => <<'#5...........',
# keep lines long; do not break after 'return' and '.' with -baoo
return $ref eq 'SCALAR'
  ? $self->encode_scalar( $object, $name, $type, $attr )
  : $ref eq 'ARRAY';
my $s = 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
  . 'bbbbbbbbbbbbbbbbbbbbbbbbb';
#5...........
        },

        'rt127633.rt127633' => {
            source => "rt127633",
            params => "rt127633",
            expect => <<'#6...........',
# keep lines long; do not break after 'return' and '.' with -baoo
return $ref eq 'SCALAR' ? $self->encode_scalar( $object, $name, $type, $attr ) :
  $ref eq 'ARRAY';
my $s = 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa' .
  'bbbbbbbbbbbbbbbbbbbbbbbbb';
#6...........
        },

        'align14.def' => {
            source => "align14",
            params => "def",
            expect => <<'#7...........',
# align the =
my ($apple)         = new Fruit( "Apple1", .1, .30 );
my ($grapefruit)    = new Grapefruit( "Grapefruit1", .3 );
my ($redgrapefruit) = new RedGrapefruit( "Grapefruit2", .3 );
#7...........
        },

        'align15.def' => {
            source => "align15",
            params => "def",
            expect => <<'#8...........',
# align both = and //
my $color      = $opts{'-color'}      // 'black';
my $background = $opts{'-background'} // 'none';
my $linewidth  = $opts{'-linewidth'}  // 1;
my $radius     = $opts{'-radius'}     // 0;
#8...........
        },

        'align16.def' => {
            source => "align16",
            params => "def",
            expect => <<'#9...........',
# align all at first =>
use constant {
    PHFAM  => [ { John => 1, Jane => 2, Sally => 3 }, 33, 28, 3 ],
    FAMILY => [qw( John Jane Sally )],
    AGES   => { John => 33, Jane => 28, Sally => 3 },
    RFAM   => [ [qw( John Jane Sally )] ],
    THREE  => 3,
    SPIT   => sub { shift },
};

#9...........
        },

        'break5.def' => {
            source => "break5",
            params => "def",
            expect => <<'#10...........',
# do not break at .'s after the ?
return ( $pod eq $pod2 )
  ? "\n&lt;A NAME=\"" . $value . "\"&gt;\n$text&lt;/A&gt;\n"
  : "\n$type$pod2.html\#" . $value . "\"&gt;$text&lt;\/A&gt;\n";
#10...........
        },

        'align19.def' => {
            source => "align19",
            params => "def",
            expect => <<'#11...........',
# different lhs patterns, do not align the '='
@_ = qw(sort grep map do eval);
@is_not_zero_continuation_block_type{@_} = (1) x scalar(@_);
#11...........
        },

        'align20.def' => {
            source => "align20",
            params => "def",
            expect => <<'#12...........',
# marginal two-line match; different lhs patterns; do not align
$w[$i] = $t;
$t = 1000000;
#12...........
        },

        'align21.def' => {
            source => "align21",
            params => "def",
            expect => <<'#13...........',
# two lines with large gap but same lhs pattern so align equals
local (@pieces)            = split( /\./, $filename, 2 );
local ($just_dir_and_base) = $pieces[0];

# two lines with 3 alignment tokens
$expect = "1$expect" if $expect           =~ /^e/i;
$p      = "1$p"      if defined $p and $p =~ /^e/i;

# two lines where alignment causes a large gap
is( eval { sysopen( my $ro, $foo, &O_RDONLY | $TAINT0 ) }, undef );
is( $@,                                                    '' );
#13...........
        },

        'align22.def' => {
            source => "align22",
            params => "def",
            expect => <<'#14...........',
# two equality lines with different patterns to left of equals do not align
$signame{$_} = ++$signal;
$signum[$signal] = $_;
#14...........
        },

        'align23.def' => {
            source => "align23",
            params => "def",
            expect => <<'#15...........',
# two equality lines with same pattern on left of equals will align
my $orig = my $format = "^<<<<< ~~\n";
my $abc  = "abc";
#15...........
        },

        'align24.def' => {
            source => "align24",
            params => "def",
            expect => <<'#16...........',
# Do not align interior fat commas here; different container types
my $p = TAP::Parser::SubclassTest->new(
    {
        exec    => [ $cat => $file ],
        sources => { MySourceHandler => { accept_all => 1 } },
    }
);
#16...........
        },

        'align25.def' => {
            source => "align25",
            params => "def",
            expect => <<'#17...........',
# do not align internal commas here; different container types
is_deeply( [ $a, $a ],               [ $b, $c ] );
is_deeply( { foo => $a, bar => $a }, { foo => $b, bar => $c } );
is_deeply( [ \$a, \$a ],             [ \$b, \$c ] );

#17...........
        },

        'align26.def' => {
            source => "align26",
            params => "def",
            expect => <<'#18...........',
#  align first of multiple equals
$SIG{PIPE}  = sub { die "writingtoaclosedpipe" };
$SIG{BREAK} = $SIG{INT} = $SIG{TERM};
$SIG{HUP}   = \&some_handler;
#18...........
        },

        'align27.def' => {
            source => "align27",
            params => "def",
            expect => <<'#19...........',
# do not align first equals here (unmatched commas on left side of =)
my ( $self, $name, $type ) = @_;
my $html_toc_fh    = $self->{_html_toc_fh};
my $html_prelim_fh = $self->{_html_prelim_fh};
#19...........
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

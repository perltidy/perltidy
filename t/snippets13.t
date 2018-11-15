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
        'def'      => "",
        'rt127633' => <<'----------',
-wba=':'
----------
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

        'break5' => <<'----------',
# do not break at .'s after the ?
return (
    ( $pod eq $pod2 ) & amp;
      &amp;
      ( $htype eq "NAME" )
  )
  ? "\n&lt;A NAME=\""
  . $value
  . "\"&gt;\n$text&lt;/A&gt;\n"
  : "\n$type$pod2.html\#" . $value . "\"&gt;$text&lt;\/A&gt;\n";
----------

        'rt127633' => <<'----------',
# do not break after return with -wba=':'
return $ref eq 'SCALAR' ? $self->encode_scalar( $object, $name, $type, $attr ) : $ref eq 'ARRAY';
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
    my $type = shift || "o";
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
# do not break after return with -wba=':'
return $ref eq 'SCALAR'
  ? $self->encode_scalar( $object, $name, $type, $attr )
  : $ref eq 'ARRAY';
#5...........
        },

        'rt127633.rt127633' => {
            source => "rt127633",
            params => "rt127633",
            expect => <<'#6...........',
# do not break after return with -wba=':'
return $ref eq 'SCALAR' ? $self->encode_scalar( $object, $name, $type, $attr ) :
  $ref eq 'ARRAY';
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
return (
    ( $pod eq $pod2 ) & amp;
    &amp;
    ( $htype eq "NAME" )
  )
  ? "\n&lt;A NAME=\"" . $value . "\"&gt;\n$text&lt;/A&gt;\n"
  : "\n$type$pod2.html\#" . $value . "\"&gt;$text&lt;\/A&gt;\n";
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

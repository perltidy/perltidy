# Created with: ./make_t.pl

# Contents:
#1 otr1.otr
#2 pbp1.def
#3 pbp1.pbp
#4 pbp2.def
#5 pbp2.pbp
#6 pbp3.def
#7 pbp3.pbp
#8 pbp4.def
#9 pbp4.pbp
#10 pbp5.def
#11 pbp5.pbp
#12 print1.def
#13 q1.def
#14 q2.def
#15 recombine1.def
#16 recombine2.def
#17 recombine3.def
#18 recombine4.def
#19 rt101547.def
#20 rt102371.def

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
        'def' => "",
        'otr' => <<'----------',
-ohbr
-opr
-osbr
----------
        'pbp' => "-pbp -nst -nse",
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

        'otr1' => <<'----------',
return $pdl->slice(
    join ',',
    (
        map {
                $_ eq "X" ? ":"
              : ref $_ eq "ARRAY" ? join ':', @$_
              : !ref $_ ? $_
              : die "INVALID SLICE DEF $_"
        } @_
    )
);
----------

        'pbp1' => <<'----------',
            # break after '+' if default, before + if pbp
            my $min_gnu_indentation = $standard_increment +
              $gnu_stack[$max_gnu_stack_index]->get_SPACES();
----------

        'pbp2' => <<'----------',
$tmp = $day - 32075 + 1461 * ( $year + 4800 - ( 14 - $month ) / 12 ) / 4 + 367 * ( $month - 2 + ( ( 14 - $month ) / 12 ) * 12 ) / 12 - 3 * ( ( $year + 4900 - ( 14 - $month ) / 12 ) / 100 ) / 4;
----------

        'pbp3' => <<'----------',
return $sec + $SecOff + ( SECS_PER_MINUTE * $min ) + ( SECS_PER_HOUR * $hour ) + ( SECS_PER_DAY * $days );


----------

        'pbp4' => <<'----------',
# with defaults perltidy will break after the '=' here
my @host_seq = $level eq "easy" ?
	    @reordered : 0..$last;  # reordered has CDROM up front
----------

        'pbp5' => <<'----------',
# illustates problem with -pbp: -ci should not equal -i
say 'ok_200_24_hours.value '.average({'$and'=>[{time=>{'$gt',$time-60*60*24}},{status=>200}]});

----------

        'print1' => <<'----------',
# same text twice. Has uncontained commas; -- leave as is
print "conformability (Not the same dimension)\n",
  "\t",
  $have, " is ",
  text_unit($hu), "\n", "\t", $want, " is ", text_unit($wu), "\n",;

print
  "conformability (Not the same dimension)\n",
  "\t", $have, " is ", text_unit($hu), "\n",
  "\t", $want, " is ", text_unit($wu), "\n",
  ;
----------

        'q1' => <<'----------',
print qq(You are in zone $thisTZ
Difference with respect to GMT is ), $offset / 3600, qq( hours
And local time is $hour hours $min minutes $sec seconds
);
----------

        'q2' => <<'----------',
$a=qq
XHello World\nX;
print "$a";
----------

        'recombine1' => <<'----------',
# recombine '= [' here:
$retarray =
  [ &{ $sth->{'xbase_parsed_sql'}{'selectfn'} }
      ( $xbase, $values, $sth->{'xbase_bind_values'} ) ]
  if defined $values;
----------

        'recombine2' => <<'----------',
    # recombine = unless old break there
    $a = [ length( $self->{fb}[-1] ), $#{ $self->{fb} } ] ;    # set cursor at end of buffer and print this cursor
----------

        'recombine3' => <<'----------',
        # recombine final line
        $command = (
                    ($catpage =~ m:\.gz:)
                    ? $ZCAT
                    : $CAT
                   )
          . " < $catpage";
----------

        'recombine4' => <<'----------',
    # do not recombine into two lines after a comma if
    # the term is complex (has parens) or changes level
    $delta_time = sprintf "%.4f", ( ( $done[0] + ( $done[1] / 1e6 ) ) - ( $start[0] + ( $start[1] / 1e6 ) ) );
----------

        'rt101547' => <<'----------',
{ source_host => MM::Config->instance->host // q{}, }
----------

        'rt102371' => <<'----------',
state $b //= ccc();
----------
    };

    ####################################
    # BEGIN SECTION 3: Expected output #
    ####################################
    $rtests = {

        'otr1.otr' => {
            source => "otr1",
            params => "otr",
            expect => <<'#1...........',
return $pdl->slice(
    join ',', (
        map {
                $_ eq "X"         ? ":"
              : ref $_ eq "ARRAY" ? join ':', @$_
              : !ref $_           ? $_
              : die "INVALID SLICE DEF $_"
        } @_
    )
);
#1...........
        },

        'pbp1.def' => {
            source => "pbp1",
            params => "def",
            expect => <<'#2...........',
            # break after '+' if default, before + if pbp
            my $min_gnu_indentation = $standard_increment +
              $gnu_stack[$max_gnu_stack_index]->get_SPACES();
#2...........
        },

        'pbp1.pbp' => {
            source => "pbp1",
            params => "pbp",
            expect => <<'#3...........',
            # break after '+' if default, before + if pbp
            my $min_gnu_indentation = $standard_increment
                + $gnu_stack[$max_gnu_stack_index]->get_SPACES();
#3...........
        },

        'pbp2.def' => {
            source => "pbp2",
            params => "def",
            expect => <<'#4...........',
$tmp =
  $day - 32075 +
  1461 * ( $year + 4800 - ( 14 - $month ) / 12 ) / 4 +
  367 * ( $month - 2 + ( ( 14 - $month ) / 12 ) * 12 ) / 12 -
  3 * ( ( $year + 4900 - ( 14 - $month ) / 12 ) / 100 ) / 4;
#4...........
        },

        'pbp2.pbp' => {
            source => "pbp2",
            params => "pbp",
            expect => <<'#5...........',
$tmp
    = $day - 32075
    + 1461 * ( $year + 4800 - ( 14 - $month ) / 12 ) / 4
    + 367 * ( $month - 2 + ( ( 14 - $month ) / 12 ) * 12 ) / 12
    - 3 * ( ( $year + 4900 - ( 14 - $month ) / 12 ) / 100 ) / 4;
#5...........
        },

        'pbp3.def' => {
            source => "pbp3",
            params => "def",
            expect => <<'#6...........',
return $sec + $SecOff +
  ( SECS_PER_MINUTE * $min ) +
  ( SECS_PER_HOUR * $hour ) +
  ( SECS_PER_DAY * $days );

#6...........
        },

        'pbp3.pbp' => {
            source => "pbp3",
            params => "pbp",
            expect => <<'#7...........',
return
      $sec + $SecOff
    + ( SECS_PER_MINUTE * $min )
    + ( SECS_PER_HOUR * $hour )
    + ( SECS_PER_DAY * $days );

#7...........
        },

        'pbp4.def' => {
            source => "pbp4",
            params => "def",
            expect => <<'#8...........',
# with defaults perltidy will break after the '=' here
my @host_seq =
  $level eq "easy" ? @reordered : 0 .. $last;    # reordered has CDROM up front
#8...........
        },

        'pbp4.pbp' => {
            source => "pbp4",
            params => "pbp",
            expect => <<'#9...........',
# with defaults perltidy will break after the '=' here
my @host_seq
    = $level eq "easy"
    ? @reordered
    : 0 .. $last;    # reordered has CDROM up front
#9...........
        },

        'pbp5.def' => {
            source => "pbp5",
            params => "def",
            expect => <<'#10...........',
# illustates problem with -pbp: -ci should not equal -i
say 'ok_200_24_hours.value '
  . average(
    {
        '$and' =>
          [ { time => { '$gt', $time - 60 * 60 * 24 } }, { status => 200 } ]
    }
  );

#10...........
        },

        'pbp5.pbp' => {
            source => "pbp5",
            params => "pbp",
            expect => <<'#11...........',
# illustates problem with -pbp: -ci should not equal -i
say 'ok_200_24_hours.value '
    . average(
    {   '$and' => [
            { time => { '$gt', $time - 60 * 60 * 24 } }, { status => 200 }
        ]
    }
    );

#11...........
        },

        'print1.def' => {
            source => "print1",
            params => "def",
            expect => <<'#12...........',
# same text twice. Has uncontained commas; -- leave as is
print "conformability (Not the same dimension)\n",
  "\t",
  $have, " is ",
  text_unit($hu), "\n", "\t", $want, " is ", text_unit($wu), "\n",;

print
  "conformability (Not the same dimension)\n",
  "\t", $have, " is ", text_unit($hu), "\n",
  "\t", $want, " is ", text_unit($wu), "\n",
  ;
#12...........
        },

        'q1.def' => {
            source => "q1",
            params => "def",
            expect => <<'#13...........',
print qq(You are in zone $thisTZ
Difference with respect to GMT is ), $offset / 3600, qq( hours
And local time is $hour hours $min minutes $sec seconds
);
#13...........
        },

        'q2.def' => {
            source => "q2",
            params => "def",
            expect => <<'#14...........',
$a = qq
XHello World\nX;
print "$a";
#14...........
        },

        'recombine1.def' => {
            source => "recombine1",
            params => "def",
            expect => <<'#15...........',
# recombine '= [' here:
$retarray =
  [ &{ $sth->{'xbase_parsed_sql'}{'selectfn'} }
      ( $xbase, $values, $sth->{'xbase_bind_values'} ) ]
  if defined $values;
#15...........
        },

        'recombine2.def' => {
            source => "recombine2",
            params => "def",
            expect => <<'#16...........',
    # recombine = unless old break there
    $a = [ length( $self->{fb}[-1] ), $#{ $self->{fb} } ]
      ;    # set cursor at end of buffer and print this cursor
#16...........
        },

        'recombine3.def' => {
            source => "recombine3",
            params => "def",
            expect => <<'#17...........',
        # recombine final line
        $command = (
            ( $catpage =~ m:\.gz: )
            ? $ZCAT
            : $CAT
        ) . " < $catpage";
#17...........
        },

        'recombine4.def' => {
            source => "recombine4",
            params => "def",
            expect => <<'#18...........',
    # do not recombine into two lines after a comma if
    # the term is complex (has parens) or changes level
    $delta_time = sprintf "%.4f",
      ( ( $done[0] + ( $done[1] / 1e6 ) ) -
          ( $start[0] + ( $start[1] / 1e6 ) ) );
#18...........
        },

        'rt101547.def' => {
            source => "rt101547",
            params => "def",
            expect => <<'#19...........',
{ source_host => MM::Config->instance->host // q{}, }
#19...........
        },

        'rt102371.def' => {
            source => "rt102371",
            params => "def",
            expect => <<'#20...........',
state $b //= ccc();
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

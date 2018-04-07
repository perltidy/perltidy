# **This script was automatically generated**
# Created with: ./make_t.pl
# Thu Apr  5 07:31:23 2018

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
        'def' => "",
        'pbp' => "-pbp -nst -nse",
    };

    ######################
    # SECTION 2: Sources #
    ######################
    $rsources = {

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

        'rt102451' => <<'----------',
# RT#102451 bug test; unwanted spaces added before =head1 on each pass
#<<<

=head1 NAME

=cut

my %KA_CACHE; # indexed by uhost currently, points to [$handle...] array


=head1 NAME

=cut

#>>>
----------

        'rt116344' => <<'----------',
# Rt116344
# Attempting to tidy the following code failed:
sub broken {
    return ref {} ? 1 : 0;
    something();
}
----------

        'rt123774' => <<'----------',
# retain any space between backslash and quote to avoid fooling html formatters
my $var1 = \ "bubba";
my $var2 = \"bubba";
my $var3 = \ 'bubba';
my $var4 = \'bubba';
my $var5 = \            "bubba";
----------
    };

    ##############################
    # SECTION 3: Expected output #
    ##############################
    $rtests = {

        'pbp1.def' => {
            source => "pbp1",
            params => "def",
            expect => <<'#1...........',
            # break after '+' if default, before + if pbp
            my $min_gnu_indentation =
              $standard_increment +
              $gnu_stack[$max_gnu_stack_index]->get_SPACES();
#1...........
        },

        'pbp1.pbp' => {
            source => "pbp1",
            params => "pbp",
            expect => <<'#2...........',
            # break after '+' if default, before + if pbp
            my $min_gnu_indentation = $standard_increment
                + $gnu_stack[$max_gnu_stack_index]->get_SPACES();
#2...........
        },

        'pbp2.def' => {
            source => "pbp2",
            params => "def",
            expect => <<'#3...........',
$tmp =
  $day - 32075 +
  1461 * ( $year + 4800 - ( 14 - $month ) / 12 ) / 4 +
  367 * ( $month - 2 + ( ( 14 - $month ) / 12 ) * 12 ) / 12 -
  3 * ( ( $year + 4900 - ( 14 - $month ) / 12 ) / 100 ) / 4;
#3...........
        },

        'pbp2.pbp' => {
            source => "pbp2",
            params => "pbp",
            expect => <<'#4...........',
$tmp
    = $day - 32075
    + 1461 * ( $year + 4800 - ( 14 - $month ) / 12 ) / 4
    + 367 * ( $month - 2 + ( ( 14 - $month ) / 12 ) * 12 ) / 12
    - 3 * ( ( $year + 4900 - ( 14 - $month ) / 12 ) / 100 ) / 4;
#4...........
        },

        'pbp3.def' => {
            source => "pbp3",
            params => "def",
            expect => <<'#5...........',
return $sec + $SecOff +
  ( SECS_PER_MINUTE * $min ) +
  ( SECS_PER_HOUR * $hour ) +
  ( SECS_PER_DAY * $days );

#5...........
        },

        'pbp3.pbp' => {
            source => "pbp3",
            params => "pbp",
            expect => <<'#6...........',
return
      $sec + $SecOff
    + ( SECS_PER_MINUTE * $min )
    + ( SECS_PER_HOUR * $hour )
    + ( SECS_PER_DAY * $days );

#6...........
        },

        'pbp4.def' => {
            source => "pbp4",
            params => "def",
            expect => <<'#7...........',
# with defaults perltidy will break after the '=' here
my @host_seq =
  $level eq "easy" ? @reordered : 0 .. $last;    # reordered has CDROM up front
#7...........
        },

        'pbp4.pbp' => {
            source => "pbp4",
            params => "pbp",
            expect => <<'#8...........',
# with defaults perltidy will break after the '=' here
my @host_seq
    = $level eq "easy"
    ? @reordered
    : 0 .. $last;    # reordered has CDROM up front
#8...........
        },

        'pbp5.def' => {
            source => "pbp5",
            params => "def",
            expect => <<'#9...........',
# illustates problem with -pbp: -ci should not equal -i
say 'ok_200_24_hours.value '
  . average(
    {
        '$and' =>
          [ { time => { '$gt', $time - 60 * 60 * 24 } }, { status => 200 } ]
    }
  );

#9...........
        },

        'pbp5.pbp' => {
            source => "pbp5",
            params => "pbp",
            expect => <<'#10...........',
# illustates problem with -pbp: -ci should not equal -i
say 'ok_200_24_hours.value '
    . average(
    {   '$and' => [
            { time => { '$gt', $time - 60 * 60 * 24 } }, { status => 200 }
        ]
    }
    );

#10...........
        },

        'print1.def' => {
            source => "print1",
            params => "def",
            expect => <<'#11...........',
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
#11...........
        },

        'q1.def' => {
            source => "q1",
            params => "def",
            expect => <<'#12...........',
print qq(You are in zone $thisTZ
Difference with respect to GMT is ), $offset / 3600, qq( hours
And local time is $hour hours $min minutes $sec seconds
);
#12...........
        },

        'q2.def' => {
            source => "q2",
            params => "def",
            expect => <<'#13...........',
$a = qq
XHello World\nX;
print "$a";
#13...........
        },

        'recombine1.def' => {
            source => "recombine1",
            params => "def",
            expect => <<'#14...........',
# recombine '= [' here:
$retarray =
  [ &{ $sth->{'xbase_parsed_sql'}{'selectfn'} }
      ( $xbase, $values, $sth->{'xbase_bind_values'} ) ]
  if defined $values;
#14...........
        },

        'recombine2.def' => {
            source => "recombine2",
            params => "def",
            expect => <<'#15...........',
    # recombine = unless old break there
    $a = [ length( $self->{fb}[-1] ), $#{ $self->{fb} } ]
      ;    # set cursor at end of buffer and print this cursor
#15...........
        },

        'recombine3.def' => {
            source => "recombine3",
            params => "def",
            expect => <<'#16...........',
        # recombine final line
        $command = (
            ( $catpage =~ m:\.gz: )
            ? $ZCAT
            : $CAT
        ) . " < $catpage";
#16...........
        },

        'recombine4.def' => {
            source => "recombine4",
            params => "def",
            expect => <<'#17...........',
    # do not recombine into two lines after a comma if
    # the term is complex (has parens) or changes level
    $delta_time = sprintf "%.4f",
      ( ( $done[0] + ( $done[1] / 1e6 ) ) -
          ( $start[0] + ( $start[1] / 1e6 ) ) );
#17...........
        },

        'rt102451.def' => {
            source => "rt102451",
            params => "def",
            expect => <<'#18...........',
# RT#102451 bug test; unwanted spaces added before =head1 on each pass
#<<<

=head1 NAME

=cut

my %KA_CACHE; # indexed by uhost currently, points to [$handle...] array


=head1 NAME

=cut

#>>>
#18...........
        },

        'rt116344.def' => {
            source => "rt116344",
            params => "def",
            expect => <<'#19...........',
# Rt116344
# Attempting to tidy the following code failed:
sub broken {
    return ref {} ? 1 : 0;
    something();
}
#19...........
        },

        'rt123774.def' => {
            source => "rt123774",
            params => "def",
            expect => <<'#20...........',
# retain any space between backslash and quote to avoid fooling html formatters
my $var1 = \ "bubba";
my $var2 = \"bubba";
my $var3 = \ 'bubba';
my $var4 = \'bubba';
my $var5 = \ "bubba";
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

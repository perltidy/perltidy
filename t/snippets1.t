# Created with: ./make_t.pl

# Contents:
#1 105484.def
#2 align1.def
#3 align2.def
#4 align3.def
#5 align4.def
#6 align5.def
#7 align6.def
#8 align7.def
#9 align8.def
#10 align9.def
#11 andor1.def
#12 andor10.def
#13 andor2.def
#14 andor3.def
#15 andor4.def
#16 andor5.def
#17 andor6.def
#18 andor7.def
#19 andor8.def
#20 andor9.def

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
    $rparams = { 'def' => "", };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

        '105484' => <<'----------',
switch (1) {
    case x { 2 } else { }
}
----------

        'align1' => <<'----------',
return ( $fetch_key eq $fk
      && $store_key eq $sk
      && $fetch_value eq $fv
      && $store_value eq $sv
      && $_ eq 'original' );
----------

        'align2' => <<'----------',
same =
  (      ( $aP eq $bP )
      && ( $aS eq $bS )
      && ( $aT eq $bT )
      && ( $a->{'title'} eq $b->{'title'} )
      && ( $a->{'href'} eq $b->{'href'} ) );
----------

        'align3' => <<'----------',
# This greatly improved after dropping 'ne' and 'eq':
if (
    $dir eq $updir           and    # if we have an updir
    @collapsed               and    # and something to collapse
    length $collapsed[-1]    and    # and its not the rootdir
    $collapsed[-1] ne $updir and    # nor another updir
    $collapsed[-1] ne $curdir       # nor the curdir
  ) { $bla}
----------

        'align4' => <<'----------',
# removed 'eq' and '=~' from alignment tokens to get alignment of '?'s
my $salute =
    $name eq $EMPTY_STR                      ? 'Customer'
  : $name =~ m/\A((?:Sir|Dame) \s+ \S+) /xms ? $1
  : $name =~ m/(.*), \s+ Ph[.]?D \z     /xms ? "Dr $1"
  :                                            $name;
----------

        'align5' => <<'----------',
# some lists
printline( "Broadcast", &bintodq($b),    ( $b,    $mask, $bcolor, 0 ) );
printline( "HostMin",   &bintodq($hmin), ( $hmin, $mask, $bcolor, 0 ) );
printline( "HostMax",   &bintodq($hmax), ( $hmax, $mask, $bcolor, 0 ) );
----------

        'align6' => <<'----------',
# align opening parens
if ( ( index( $msg_line_lc, $nick1 ) != -1 ) ||
     ( index( $msg_line_lc, $nick2 ) != -1 ) ||
     ( index( $msg_line_lc, $nick3 ) != -1 ) ) {
    do_something();
}
----------

        'align7' => <<'----------',
# Alignment with two fat commas in second line
my $ct = Courriel::Header::ContentType->new(
    mime_type  => 'multipart/alternative',
    attributes => { boundary => unique_boundary },
);
----------

        'align8' => <<'----------',
# aligning '=' and padding 'if'
if    ( $tag == 263 ) { $bbi->{"Info.Thresholding"}   = $value }
elsif ( $tag == 264 ) { $bbi->{"Info.CellWidth"}      = $value }
elsif ( $tag == 265 ) { $bbi->{"Info.CellLength"}     = $value }
----------

        'align9' => <<'----------',
# test of aligning || 
my $os =
  ( $ExtUtils::MM_Unix::Is_OS2   || 0 ) +
  ( $ExtUtils::MM_Unix::Is_Mac   || 0 ) +
  ( $ExtUtils::MM_Unix::Is_Win32 || 0 ) +
  ( $ExtUtils::MM_Unix::Is_Dos   || 0 ) +
  ( $ExtUtils::MM_Unix::Is_VMS   || 0 );
----------

        'andor1' => <<'----------',
return 1 if $det_a < 0 and $det_b > 0 or
            $det_a > 0 and $det_b < 0;
----------

        'andor10' => <<'----------',
if ( (       ($a) and ( $b == 13 ) and ( $c - 24 = 0 ) and ("test")
         and ( $rudolph eq "reindeer" or $rudolph eq "red nosed" )
         and $test
     ) or ( $nobody and ( $noone or $none ) ) 
  )
{ $i++; }
----------

        'andor2' => <<'----------',
# breaks at = or at && but not both
my $success = ( system("$Config{cc} -o $te $tc $libs $HIDE") == 0 ) && -e $te ? 1 : 0;
----------

        'andor3' => <<'----------',
ok(       ( $obj->name() eq $obj2->name() )
      and ( $obj->version() eq $obj2->version() )
      and ( $obj->help()    eq $obj2->help() ) );
----------

        'andor4' => <<'----------',
    if ( !$verbose_error && ( !$options->{'log'}
          && ( ( $options->{'verbose'} & 8 ) || ( $options->{'verbose'} & 16 )
              || ( $options->{'verbose'} & 32 )
              || ( $options->{'verbose'} & 64 ) ) ) )
----------

        'andor5' => <<'----------',
    # two levels of && with side comments
    if (
        defined &syscopy
        && \&syscopy != \&copy
        && !$to_a_handle
        && !( $from_a_handle && $^O eq 'os2' )      # OS/2 cannot handle
        && !( $from_a_handle && $^O eq 'mpeix' )    # and neither can MPE/iX.
      )
    {
        return syscopy( $from, $to );
    }
----------

        'andor6' => <<'----------',
# Example of nested ands and ors
sub is_miniwhile {    # check for one-line loop (`foo() while $y--')
    my $op = shift;
    return (
              !null($op) and null( $op->sibling )
          and $op->ppaddr eq "pp_null"
          and class($op) eq "UNOP"
          and (
            (
                    $op->first->ppaddr =~ /^pp_(and|or)$/
                and $op->first->first->sibling->ppaddr eq "pp_lineseq"
            )
            or (    $op->first->ppaddr eq "pp_lineseq"
                and not null $op->first->first->sibling
                and $op->first->first->sibling->ppaddr eq "pp_unstack" )
          )
    );
}
----------

        'andor7' => <<'----------',
        # original is single line:
        $a = 1 if $l and !$r or !$l and $r;
----------

        'andor8' => <<'----------',
        # original is broken:
        $a = 1 
        if $l and !$r or !$l and $r;
----------

        'andor9' => <<'----------',
if ( (      ( $old_new and $old_new eq 'changed' )
        and ( $db_new and $db_new eq 'changed' ) 
        and ( not defined $old_db ) 
     ) or ( ( $old_new and $old_new eq 'changed' )
        and ( $db_new and $db_new eq 'new' )
        and ( $old_db and $old_db eq 'new' ) 
     ) or ( ( $old_new and $old_new eq 'new' )
        and ( $db_new and $db_new eq 'new' )
        and ( not defined $old_db ) 
   ) )
{   
    return "update";
}
----------
    };

    ####################################
    # BEGIN SECTION 3: Expected output #
    ####################################
    $rtests = {

        '105484.def' => {
            source => "105484",
            params => "def",
            expect => <<'#1...........',
switch (1) {
    case x { 2 } else { }
}
#1...........
        },

        'align1.def' => {
            source => "align1",
            params => "def",
            expect => <<'#2...........',
return ( $fetch_key eq $fk
      && $store_key eq $sk
      && $fetch_value eq $fv
      && $store_value eq $sv
      && $_ eq 'original' );
#2...........
        },

        'align2.def' => {
            source => "align2",
            params => "def",
            expect => <<'#3...........',
same =
  (      ( $aP eq $bP )
      && ( $aS eq $bS )
      && ( $aT eq $bT )
      && ( $a->{'title'} eq $b->{'title'} )
      && ( $a->{'href'} eq $b->{'href'} ) );
#3...........
        },

        'align3.def' => {
            source => "align3",
            params => "def",
            expect => <<'#4...........',
# This greatly improved after dropping 'ne' and 'eq':
if (
    $dir eq $updir           and    # if we have an updir
    @collapsed               and    # and something to collapse
    length $collapsed[-1]    and    # and its not the rootdir
    $collapsed[-1] ne $updir and    # nor another updir
    $collapsed[-1] ne $curdir       # nor the curdir
  )
{
    $bla;
}
#4...........
        },

        'align4.def' => {
            source => "align4",
            params => "def",
            expect => <<'#5...........',
# removed 'eq' and '=~' from alignment tokens to get alignment of '?'s
my $salute =
    $name eq $EMPTY_STR                      ? 'Customer'
  : $name =~ m/\A((?:Sir|Dame) \s+ \S+) /xms ? $1
  : $name =~ m/(.*), \s+ Ph[.]?D \z     /xms ? "Dr $1"
  :                                            $name;
#5...........
        },

        'align5.def' => {
            source => "align5",
            params => "def",
            expect => <<'#6...........',
# some lists
printline( "Broadcast", &bintodq($b),    ( $b,    $mask, $bcolor, 0 ) );
printline( "HostMin",   &bintodq($hmin), ( $hmin, $mask, $bcolor, 0 ) );
printline( "HostMax",   &bintodq($hmax), ( $hmax, $mask, $bcolor, 0 ) );
#6...........
        },

        'align6.def' => {
            source => "align6",
            params => "def",
            expect => <<'#7...........',
# align opening parens
if (   ( index( $msg_line_lc, $nick1 ) != -1 )
    || ( index( $msg_line_lc, $nick2 ) != -1 )
    || ( index( $msg_line_lc, $nick3 ) != -1 ) )
{
    do_something();
}
#7...........
        },

        'align7.def' => {
            source => "align7",
            params => "def",
            expect => <<'#8...........',
# Alignment with two fat commas in second line
my $ct = Courriel::Header::ContentType->new(
    mime_type  => 'multipart/alternative',
    attributes => { boundary => unique_boundary },
);
#8...........
        },

        'align8.def' => {
            source => "align8",
            params => "def",
            expect => <<'#9...........',
# aligning '=' and padding 'if'
if    ( $tag == 263 ) { $bbi->{"Info.Thresholding"} = $value }
elsif ( $tag == 264 ) { $bbi->{"Info.CellWidth"}    = $value }
elsif ( $tag == 265 ) { $bbi->{"Info.CellLength"}   = $value }
#9...........
        },

        'align9.def' => {
            source => "align9",
            params => "def",
            expect => <<'#10...........',
# test of aligning ||
my $os =
  ( $ExtUtils::MM_Unix::Is_OS2   || 0 ) +
  ( $ExtUtils::MM_Unix::Is_Mac   || 0 ) +
  ( $ExtUtils::MM_Unix::Is_Win32 || 0 ) +
  ( $ExtUtils::MM_Unix::Is_Dos   || 0 ) +
  ( $ExtUtils::MM_Unix::Is_VMS   || 0 );
#10...........
        },

        'andor1.def' => {
            source => "andor1",
            params => "def",
            expect => <<'#11...........',
return 1
  if $det_a < 0 and $det_b > 0
  or $det_a > 0 and $det_b < 0;
#11...........
        },

        'andor10.def' => {
            source => "andor10",
            params => "def",
            expect => <<'#12...........',
if (
    (
            ($a)
        and ( $b == 13 )
        and ( $c - 24 = 0 )
        and ("test")
        and ( $rudolph eq "reindeer" or $rudolph eq "red nosed" )
        and $test
    )
    or ( $nobody and ( $noone or $none ) )
  )
{
    $i++;
}
#12...........
        },

        'andor2.def' => {
            source => "andor2",
            params => "def",
            expect => <<'#13...........',
# breaks at = or at && but not both
my $success =
  ( system("$Config{cc} -o $te $tc $libs $HIDE") == 0 ) && -e $te ? 1 : 0;
#13...........
        },

        'andor3.def' => {
            source => "andor3",
            params => "def",
            expect => <<'#14...........',
ok(       ( $obj->name() eq $obj2->name() )
      and ( $obj->version() eq $obj2->version() )
      and ( $obj->help() eq $obj2->help() ) );
#14...........
        },

        'andor4.def' => {
            source => "andor4",
            params => "def",
            expect => <<'#15...........',
    if (
        !$verbose_error
        && (
            !$options->{'log'}
            && (   ( $options->{'verbose'} & 8 )
                || ( $options->{'verbose'} & 16 )
                || ( $options->{'verbose'} & 32 )
                || ( $options->{'verbose'} & 64 ) )
        )
      )
#15...........
        },

        'andor5.def' => {
            source => "andor5",
            params => "def",
            expect => <<'#16...........',
    # two levels of && with side comments
    if (
           defined &syscopy
        && \&syscopy != \&copy
        && !$to_a_handle
        && !( $from_a_handle && $^O eq 'os2' )      # OS/2 cannot handle
        && !( $from_a_handle && $^O eq 'mpeix' )    # and neither can MPE/iX.
      )
    {
        return syscopy( $from, $to );
    }
#16...........
        },

        'andor6.def' => {
            source => "andor6",
            params => "def",
            expect => <<'#17...........',
# Example of nested ands and ors
sub is_miniwhile {    # check for one-line loop (`foo() while $y--')
    my $op = shift;
    return (
              !null($op) and null( $op->sibling )
          and $op->ppaddr eq "pp_null"
          and class($op) eq "UNOP"
          and (
            (
                    $op->first->ppaddr =~ /^pp_(and|or)$/
                and $op->first->first->sibling->ppaddr eq "pp_lineseq"
            )
            or (    $op->first->ppaddr eq "pp_lineseq"
                and not null $op->first->first->sibling
                and $op->first->first->sibling->ppaddr eq "pp_unstack" )
          )
    );
}
#17...........
        },

        'andor7.def' => {
            source => "andor7",
            params => "def",
            expect => <<'#18...........',
        # original is single line:
        $a = 1 if $l and !$r or !$l and $r;
#18...........
        },

        'andor8.def' => {
            source => "andor8",
            params => "def",
            expect => <<'#19...........',
        # original is broken:
        $a = 1
          if $l  and !$r
          or !$l and $r;
#19...........
        },

        'andor9.def' => {
            source => "andor9",
            params => "def",
            expect => <<'#20...........',
if (
    (
            ( $old_new and $old_new eq 'changed' )
        and ( $db_new  and $db_new eq 'changed' )
        and ( not defined $old_db )
    )
    or (    ( $old_new and $old_new eq 'changed' )
        and ( $db_new and $db_new eq 'new' )
        and ( $old_db and $old_db eq 'new' ) )
    or (    ( $old_new and $old_new eq 'new' )
        and ( $db_new and $db_new eq 'new' )
        and ( not defined $old_db ) )
  )
{
    return "update";
}
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
        errorfile => \$errorfile_string,    # not used when -se flag is set
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

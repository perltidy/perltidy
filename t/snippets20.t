# Created with: ./make_t.pl

# Contents:
#1 space6.def
#2 space6.space6
#3 sub3.def
#4 wc.def
#5 wc.wc1
#6 wc.wc2
#7 ce2.ce
#8 ce2.def
#9 gnu6.def
#10 gnu6.gnu
#11 git25.def
#12 git25.git25
#13 outdent.outdent2
#14 kpit.def
#15 kpit.kpit
#16 kpitl.def
#17 kpitl.kpitl
#18 hanging_side_comments3.def
#19 lop.def

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
        'ce'    => "-cuddled-blocks",
        'def'   => "",
        'git25' => "-l=0",
        'gnu'   => "-gnu",
        'kpit'  => "-pt=2 -kpit=0",
        'kpitl' => <<'----------',
-kpit=0 -kpitl='return factorial' -pt=2
----------
        'outdent2' => <<'----------',
# test -okw and -okwl
-okw -okwl='next'
----------
        'space6' => <<'----------',
-nwrs="+ - / *"
-nwls="+ - / *"
----------
        'wc1' => "-wc=4",
        'wc2' => "-wc=4 -wn",
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

        'ce2' => <<'----------',
# Previously, perltidy -ce would move a closing brace below a pod section to
# form '} else {'. No longer doing this because if you change back to -nce, the
# brace cannot go back to where it was.
if ($notty) {
    $runnonstop = 1;
	share($runnonstop);
}

=pod

If there is a TTY, we have to determine who it belongs to before we can
...

=cut

else {

    # Is Perl being run from a slave editor or graphical debugger?
    ...
}
----------

        'git25' => <<'----------',
# example for git #25; use -l=0; was losing alignment;  sub 'fix_ragged_lists' was added to fix this
my $mapping = [
# ...
    { 'is_col' => 'dsstdat',                      'cr_col' => 'enroll_isaric_date',         'trans' => 0, },
    { 'is_col' => 'corona_ieorres',               'cr_col' => '',                           'trans' => 0, },
    { 'is_col' => 'symptoms_fever',               'cr_col' => 'elig_fever',                 'trans' => 1, 'manually_reviewed' => '@TODO', 'map' => { '0' => '0', '1' => '1', '9' => '@TODO' }, },
    { 'is_col' => 'symptoms_cough',               'cr_col' => 'elig_cough',                 'trans' => 1, 'manually_reviewed' => '@TODO', 'map' => { '0' => '0', '1' => '1', '9' => '@TODO' }, },
    { 'is_col' => 'symptoms_dys_tachy_noea',      'cr_col' => 'elig_dyspnea',               'trans' => 1, 'manually_reviewed' => '@TODO', 'map' => { '0' => '0', '1' => '1', '9' => '@TODO' }, },
    { 'is_col' => 'symptoms_clinical_susp',       'cr_col' => 'elig_ari',                   'trans' => 0, },
    { 'is_col' => 'sex',                          'cr_col' => 'sex',                        'trans' => 1, 'manually_reviewed' => 1, 'map' => { '0' => '1', '1' => '2' }, },
    { 'is_col' => 'age',                          'cr_col' => '',                           'trans' => 0, },
    { 'is_col' => 'ageu',                         'cr_col' => '',                           'trans' => 0, },
# ...
];

----------

        'gnu6' => <<'----------',
# These closing braces no longer have the same position with -gnu after an
# update 13 dec 2021 in which the vertical aligner zeros recoverable spaces.
# But adding the -xlp should make them all have the same indentation.
    $var1 = {
        'foo10' => undef,
        'foo72' => ' ',
    };
    $var1 = {
        'foo10' => undef,
        'foo72' => '
',
    };
    $var2 = {
        'foo72' => '
',
        'foo10' => undef,
    };
----------

        'hanging_side_comments3' => <<'----------',
    if ( $var eq 'wastebasket' ) {    # this sends a pure block
                                      # of hanging side comments
                                      #to the vertical aligner.
                                      #It caused a crash in
                                      #a test version of
                                      #sub 'delete_unmatched_tokens'
                                      #...
                                      #}
    }
    elsif ( $var eq 'spacecommand' ) {
        &die("No $val function") unless eval "defined &$val";
    }
----------

        'kpit' => <<'----------',
if ( seek(DATA, 0, 0) ) { ... }

# The foreach keyword may be separated from the next opening paren
foreach $req(@bgQueue) {
   ...
}

# This had trouble because a later padding operation removed the inside space
while ($CmdJob eq "" && @CmdQueue > 0 && $RunNightlyWhenIdle != 1
        || @CmdQueue > 0 && $RunNightlyWhenIdle == 2 && $bpc->isAdminJob($CmdQueue[0]->{host})) {
  ... 
}

----------

        'kpitl' => <<'----------',
return ( $r**$n ) * ( pi**( $n / 2 ) ) / ( sqrt(pi) * factorial( 2 * ( int( $n
/ 2 ) ) + 2 ) / factorial( int( $n / 2 ) + 1 ) / ( 4**( int( $n / 2 ) + 1 ) )
);
----------

        'lop' => <<'----------',
# logical padding examples
$same =
  (      ( $aP eq $bP )
      && ( $aS eq $bS )
      && ( $aT eq $bT )
      && ( $a->{'title'} eq $b->{'title'} )
      && ( $a->{'href'} eq $b->{'href'} ) );

$bits =
    $top > 0xffff ? 32
  : $top > 0xff   ? 16
  : $top > 1      ? 8
  :                 1;

lc( $self->mime_attr('content-type')
        || $self->{MIH_DefaultType}
        || 'text/plain' );

if (1) { ... }

# Padding can also remove spaces; here the space after the '(' is lost:
elsif ($statement_type =~ /^sub\b/
    || $paren_type[$paren_depth] =~ /^sub\b/ )
{
}
----------

        'outdent' => <<'----------',
        my $i;
      LOOP: while ( $i = <FOTOS> ) {
            chomp($i);
            next unless $i;
            fixit($i);
        }

----------

        'space6' => <<'----------',
# test some spacing rules at possible filehandles
my $z=$x/$y;     # ok to change spaces around both sides of the /
print $x / $y;   # do not remove space before or after / here
print $x/$y;     # do not add a space before the / here
print $x+$y;     # do not add a space before the + here
----------

        'sub3' => <<'----------',
# keep these one-line blocks intact

my $aa = sub
#line 245 "Parse.yp"
{ n_stmtexp $_[1] };

my $bb = sub    #
{ n_stmtexp $_[1] };
----------

        'wc' => <<'----------',
{
    my (@indices) =
      sort {
        $dir eq 'left' ? $cells[$a] <=> $cells[$b] : $cells[$b] <=> $cells[$a];
    } (0 .. $#cells);

{{{{
                    if ( !$array[0] ) {
                        $array[0] =
                          &$CantProcessPartFunc( $entity->{'fields'}{
                          'content-type'} );
                    }
                    
}}}}}

----------
    };

    ####################################
    # BEGIN SECTION 3: Expected output #
    ####################################
    $rtests = {

        'space6.def' => {
            source => "space6",
            params => "def",
            expect => <<'#1...........',
# test some spacing rules at possible filehandles
my $z = $x / $y;    # ok to change spaces around both sides of the /
print $x / $y;      # do not remove space before or after / here
print $x/ $y;       # do not add a space before the / here
print $x+ $y;       # do not add a space before the + here
#1...........
        },

        'space6.space6' => {
            source => "space6",
            params => "space6",
            expect => <<'#2...........',
# test some spacing rules at possible filehandles
my $z = $x/$y;    # ok to change spaces around both sides of the /
print $x / $y;    # do not remove space before or after / here
print $x/$y;      # do not add a space before the / here
print $x+$y;      # do not add a space before the + here
#2...........
        },

        'sub3.def' => {
            source => "sub3",
            params => "def",
            expect => <<'#3...........',
# keep these one-line blocks intact

my $aa = sub
#line 245 "Parse.yp"
{ n_stmtexp $_[1] };

my $bb = sub    #
{ n_stmtexp $_[1] };
#3...........
        },

        'wc.def' => {
            source => "wc",
            params => "def",
            expect => <<'#4...........',
{
    my (@indices) =
      sort {
        $dir eq 'left' ? $cells[$a] <=> $cells[$b] : $cells[$b] <=> $cells[$a];
      } ( 0 .. $#cells );

    {
        {
            {
                {
                    if ( !$array[0] ) {
                        $array[0] =
                          &$CantProcessPartFunc(
                            $entity->{'fields'}{'content-type'} );
                    }

                }
            }
        }
    }
}

#4...........
        },

        'wc.wc1' => {
            source => "wc",
            params => "wc1",
            expect => <<'#5...........',
{
    my (@indices) =
      sort {
        $dir eq 'left' ? $cells[$a] <=> $cells[$b] : $cells[$b] <=> $cells[$a];
      } ( 0 .. $#cells );

    {
        {
            {
                {
    if ( !$array[0] ) {
        $array[0] =
          &$CantProcessPartFunc( $entity->{'fields'}{'content-type'} );
    }

                }
            }
        }
    }
}

#5...........
        },

        'wc.wc2' => {
            source => "wc",
            params => "wc2",
            expect => <<'#6...........',
{
    my (@indices) =
      sort {
        $dir eq 'left' ? $cells[$a] <=> $cells[$b] : $cells[$b] <=> $cells[$a];
      } ( 0 .. $#cells );

    { { { {
        if ( !$array[0] ) {
            $array[0] =
              &$CantProcessPartFunc( $entity->{'fields'}{'content-type'} );
        }

    } } } }
}

#6...........
        },

        'ce2.ce' => {
            source => "ce2",
            params => "ce",
            expect => <<'#7...........',
# Previously, perltidy -ce would move a closing brace below a pod section to
# form '} else {'. No longer doing this because if you change back to -nce, the
# brace cannot go back to where it was.
if ($notty) {
    $runnonstop = 1;
    share($runnonstop);

}

=pod

If there is a TTY, we have to determine who it belongs to before we can
...

=cut

else {

    # Is Perl being run from a slave editor or graphical debugger?
    ...;
}
#7...........
        },

        'ce2.def' => {
            source => "ce2",
            params => "def",
            expect => <<'#8...........',
# Previously, perltidy -ce would move a closing brace below a pod section to
# form '} else {'. No longer doing this because if you change back to -nce, the
# brace cannot go back to where it was.
if ($notty) {
    $runnonstop = 1;
    share($runnonstop);
}

=pod

If there is a TTY, we have to determine who it belongs to before we can
...

=cut

else {

    # Is Perl being run from a slave editor or graphical debugger?
    ...;
}
#8...........
        },

        'gnu6.def' => {
            source => "gnu6",
            params => "def",
            expect => <<'#9...........',
    # These closing braces no longer have the same position with -gnu after an
    # update 13 dec 2021 in which the vertical aligner zeros recoverable spaces.
    # But adding the -xlp should make them all have the same indentation.
    $var1 = {
        'foo10' => undef,
        'foo72' => ' ',
    };
    $var1 = {
        'foo10' => undef,
        'foo72' => '
',
    };
    $var2 = {
        'foo72' => '
',
        'foo10' => undef,
    };
#9...........
        },

        'gnu6.gnu' => {
            source => "gnu6",
            params => "gnu",
            expect => <<'#10...........',
    # These closing braces no longer have the same position with -gnu after an
    # update 13 dec 2021 in which the vertical aligner zeros recoverable spaces.
    # But adding the -xlp should make them all have the same indentation.
    $var1 = {
             'foo10' => undef,
             'foo72' => ' ',
            };
    $var1 = {
        'foo10' => undef,
        'foo72' => '
',
    };
    $var2 = {
        'foo72' => '
',
        'foo10' => undef,
            };
#10...........
        },

        'git25.def' => {
            source => "git25",
            params => "def",
            expect => <<'#11...........',
# example for git #25; use -l=0; was losing alignment;  sub 'fix_ragged_lists' was added to fix this
my $mapping = [

    # ...
    { 'is_col' => 'dsstdat', 'cr_col' => 'enroll_isaric_date', 'trans' => 0, },
    { 'is_col' => 'corona_ieorres', 'cr_col' => '',            'trans' => 0, },
    {
        'is_col'            => 'symptoms_fever',
        'cr_col'            => 'elig_fever',
        'trans'             => 1,
        'manually_reviewed' => '@TODO',
        'map'               => { '0' => '0', '1' => '1', '9' => '@TODO' },
    },
    {
        'is_col'            => 'symptoms_cough',
        'cr_col'            => 'elig_cough',
        'trans'             => 1,
        'manually_reviewed' => '@TODO',
        'map'               => { '0' => '0', '1' => '1', '9' => '@TODO' },
    },
    {
        'is_col'            => 'symptoms_dys_tachy_noea',
        'cr_col'            => 'elig_dyspnea',
        'trans'             => 1,
        'manually_reviewed' => '@TODO',
        'map'               => { '0' => '0', '1' => '1', '9' => '@TODO' },
    },
    {
        'is_col' => 'symptoms_clinical_susp',
        'cr_col' => 'elig_ari',
        'trans'  => 0,
    },
    {
        'is_col'            => 'sex',
        'cr_col'            => 'sex',
        'trans'             => 1,
        'manually_reviewed' => 1,
        'map'               => { '0' => '1', '1' => '2' },
    },
    { 'is_col' => 'age',  'cr_col' => '', 'trans' => 0, },
    { 'is_col' => 'ageu', 'cr_col' => '', 'trans' => 0, },

    # ...
];

#11...........
        },

        'git25.git25' => {
            source => "git25",
            params => "git25",
            expect => <<'#12...........',
# example for git #25; use -l=0; was losing alignment;  sub 'fix_ragged_lists' was added to fix this
my $mapping = [

    # ...
    { 'is_col' => 'dsstdat',                 'cr_col' => 'enroll_isaric_date', 'trans' => 0, },
    { 'is_col' => 'corona_ieorres',          'cr_col' => '',                   'trans' => 0, },
    { 'is_col' => 'symptoms_fever',          'cr_col' => 'elig_fever',         'trans' => 1, 'manually_reviewed' => '@TODO', 'map' => { '0' => '0', '1' => '1', '9' => '@TODO' }, },
    { 'is_col' => 'symptoms_cough',          'cr_col' => 'elig_cough',         'trans' => 1, 'manually_reviewed' => '@TODO', 'map' => { '0' => '0', '1' => '1', '9' => '@TODO' }, },
    { 'is_col' => 'symptoms_dys_tachy_noea', 'cr_col' => 'elig_dyspnea',       'trans' => 1, 'manually_reviewed' => '@TODO', 'map' => { '0' => '0', '1' => '1', '9' => '@TODO' }, },
    { 'is_col' => 'symptoms_clinical_susp',  'cr_col' => 'elig_ari',           'trans' => 0, },
    { 'is_col' => 'sex',                     'cr_col' => 'sex',                'trans' => 1, 'manually_reviewed' => 1, 'map' => { '0' => '1', '1' => '2' }, },
    { 'is_col' => 'age',                     'cr_col' => '',                   'trans' => 0, },
    { 'is_col' => 'ageu',                    'cr_col' => '',                   'trans' => 0, },

    # ...
];

#12...........
        },

        'outdent.outdent2' => {
            source => "outdent",
            params => "outdent2",
            expect => <<'#13...........',
        my $i;
      LOOP: while ( $i = <FOTOS> ) {
            chomp($i);
          next unless $i;
            fixit($i);
        }

#13...........
        },

        'kpit.def' => {
            source => "kpit",
            params => "def",
            expect => <<'#14...........',
if ( seek( DATA, 0, 0 ) ) { ... }

# The foreach keyword may be separated from the next opening paren
foreach $req (@bgQueue) {
    ...;
}

# This had trouble because a later padding operation removed the inside space
while ($CmdJob eq "" && @CmdQueue > 0 && $RunNightlyWhenIdle != 1
    || @CmdQueue > 0
    && $RunNightlyWhenIdle == 2
    && $bpc->isAdminJob( $CmdQueue[0]->{host} ) )
{
    ...;
}

#14...........
        },

        'kpit.kpit' => {
            source => "kpit",
            params => "kpit",
            expect => <<'#15...........',
if ( seek(DATA, 0, 0) ) { ... }

# The foreach keyword may be separated from the next opening paren
foreach $req ( @bgQueue ) {
    ...;
}

# This had trouble because a later padding operation removed the inside space
while ( $CmdJob eq "" && @CmdQueue > 0 && $RunNightlyWhenIdle != 1
    || @CmdQueue > 0
    && $RunNightlyWhenIdle == 2
    && $bpc->isAdminJob($CmdQueue[0]->{host}) )
{
    ...;
}

#15...........
        },

        'kpitl.def' => {
            source => "kpitl",
            params => "def",
            expect => <<'#16...........',
return ( $r**$n ) *
  ( pi**( $n / 2 ) ) /
  (
    sqrt(pi) *
      factorial( 2 * ( int( $n / 2 ) ) + 2 ) /
      factorial( int( $n / 2 ) + 1 ) /
      ( 4**( int( $n / 2 ) + 1 ) ) );
#16...........
        },

        'kpitl.kpitl' => {
            source => "kpitl",
            params => "kpitl",
            expect => <<'#17...........',
return ( $r**$n ) *
  (pi**($n / 2)) /
  (
    sqrt(pi) *
      factorial( 2 * (int($n / 2)) + 2 ) /
      factorial( int($n / 2) + 1 ) /
      (4**(int($n / 2) + 1)));
#17...........
        },

        'hanging_side_comments3.def' => {
            source => "hanging_side_comments3",
            params => "def",
            expect => <<'#18...........',
    if ( $var eq 'wastebasket' ) {    # this sends a pure block
                                      # of hanging side comments
                                      #to the vertical aligner.
                                      #It caused a crash in
                                      #a test version of
                                      #sub 'delete_unmatched_tokens'
                                      #...
                                      #}
    }
    elsif ( $var eq 'spacecommand' ) {
        &die("No $val function") unless eval "defined &$val";
    }
#18...........
        },

        'lop.def' => {
            source => "lop",
            params => "def",
            expect => <<'#19...........',
# logical padding examples
$same =
  (      ( $aP eq $bP )
      && ( $aS eq $bS )
      && ( $aT eq $bT )
      && ( $a->{'title'} eq $b->{'title'} )
      && ( $a->{'href'} eq $b->{'href'} ) );

$bits =
    $top > 0xffff ? 32
  : $top > 0xff   ? 16
  : $top > 1      ? 8
  :                 1;

lc(      $self->mime_attr('content-type')
      || $self->{MIH_DefaultType}
      || 'text/plain' );

if (1) { ... }

# Padding can also remove spaces; here the space after the '(' is lost:
elsif ($statement_type =~ /^sub\b/
    || $paren_type[$paren_depth] =~ /^sub\b/ )
{
}
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

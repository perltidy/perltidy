# Created with: ./make_t.pl

# Contents:
#1 lop.lop
#2 switch_plain.def
#3 switch_plain.switch_plain
#4 sot.def
#5 sot.sot
#6 prune.def
#7 align33.def
#8 gnu7.def
#9 gnu7.gnu
#10 git33.def
#11 git33.git33
#12 rt133130.def
#13 rt133130.rt133130
#14 nib.def
#15 nib.nib1
#16 nib.nib2
#17 scbb-csc.def
#18 scbb-csc.scbb-csc
#19 here_long.def

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
        'def'   => "",
        'git33' => <<'----------',
-wls='->' -wrs='->'

----------
        'gnu'  => "-gnu",
        'lop'  => "-nlop",
        'nib1' => "-nnib",
        'nib2' => <<'----------',
-nib -nibp='#\+\+'
----------
        'rt133130' => <<'----------',
# only the method should get a csc:
-csc -cscl=sub -sal=method
----------
        'scbb-csc'     => "-scbb -csc",
        'sot'          => "-sot -sct",
        'switch_plain' => "-nola",
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

        'align33' => <<'----------',
$wl  = int( $wl * $f + .5 );
$wr  = int( $wr * $f + .5 );
$pag = int( $pageh * $f + .5 );
$fe  = $opt_F ? "t" : "f";
$cf  = $opt_U ? "t" : "f";
$tp  = $opt_t ? "t" : "f";
$rm  = $numbstyle ? "t" : "f";
$pa = $showurl   ? "t" : "f";
$nh = $seq_number ? "t" : "f";
----------

        'git33' => <<'----------',
# test -wls='->' -wrs='->'
use Net::Ping;
my ($ping) = Net::Ping->new();
$ping->ping($host);

----------

        'gnu7' => <<'----------',
# hanging side comments
if ( $seen == 1 ) {    # We're the first word so far to have
    # this abbreviation.
    $hashref->{$abbrev} = $word;
}
elsif ( $seen == 2 ) {    # We're the second word to have this
    # abbreviation, so we can't use it.
    delete $hashref->{$abbrev};
}
else {                    # We're the third word to have this
    # abbreviation, so skip to the next word.
    next WORD;
}
----------

        'here_long' => <<'----------',
# must not break after here target regardless of maximum-line-length
$sth= $dbh->prepare (<<"END_OF_SELECT") or die "Couldn't prepare SQL" ;
    SELECT COUNT(duration),SUM(duration) 
    FROM logins WHERE username='$user'
END_OF_SELECT

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

        'nib' => <<'----------',
{    #<<<
{    #<<<
{    #++
    print "hello world\n";
}
}
}

{    #++
    {    #++
        {    #<<<
        print "hello world\n";
        }
    }
}

----------

        'prune' => <<'----------',
# some tests for 'sub prune_alignment_tree'

$request->header( 'User-Agent' => $agent )              if $agent;
$request->header( 'From'       => $from )               if $from;
$request->header( 'Range'      => "bytes=0-$max_size" ) if $max_size;

for (
    [ 'CONSTANT', sub { join "foo", "bar" },         0, "bar" ],
    [ 'CONSTANT', sub { join "foo", "bar", 3 },      1, "barfoo3" ],
    [ '$var',     sub { join $_, "bar" },            0, "bar" ],
    [ '$myvar',   sub { my $var; join $var, "bar" }, 0, "bar" ],
);

[
    [ [NewXSHdr],     [ NewXSName, NewXSArgs ],            "XSHdr" ],
    [ [NewXSCHdrs],   [ NewXSName, NewXSArgs, GlobalNew ], "XSCHdrs" ],
    [ [DefSyms],      [StructName],                        "MkDefSyms" ],
    [ [NewXSSymTab],  [ DefSyms, NewXSArgs ],              "AddArgsyms" ],
    [ [NewXSLocals],  [NewXSSymTab],                       "Sym2Loc" ],
    [ [IsAffineFlag], [],                                  sub { return "0" } ],
];

@degen_nums[ 1, 2, 4, 8 ]         = ( 'a', 'c', 'g', 't' );
@degen_nums[ 5, 10, 9, 6, 3, 12 ] = ( 'r', 'y', 'w', 's', 'm', 'k' );
@degen_nums[ 14, 13, 11, 7, 15 ]  = ( 'b', 'd', 'h', 'v', 'n' );

$_CreateFile   = ff( "k32", "CreateFile",   [ P, N, N, N, N, N, N ], N );
$_CloseHandle  = ff( "k32", "CloseHandle",  [N],                     N );
$_GetCommState = ff( "k32", "GetCommState", [ N, P ],                I );
$_SetCommState = ff( "k32", "SetCommState", [ N, P ],                I );
$_SetupComm    = ff( "k32", "SetupComm",    [ N, N, N ],             I );
$_PurgeComm    = ff( "k32", "PurgeComm",    [ N, N ],                I );
$_CreateEvent  = ff( "k32", "CreateEvent",  [ P, I, I, P ],          N );


is_deeply \@t, [

 [3],  [0],  [1],  [0],
 3,   [1],  3,   [1],
 2,   [0],  [1],  [0],
 [1],  [1],  [1],  2,
 3,   [1],  2,   [3],
 4,   [ 7, 8 ],  9,   ["a"],
 "b",  3,   2,   5,
 3,   2,   5,   3,
  [2],    5,      4,      5,
  [ 3, 2, 1 ],  1,      2,      3,
  [ -1, -2, -3 ], [ -1, -2, -3 ], [ -1, -2, -3 ], [ -1, -2 ],
  3,      [ -1, -2 ],   3,      [ -1, -2, -3 ],
  [ !1 ],   [ 8, 7, 6 ],  [ 8, 7, 6 ],  [4],
  !!0,
];
----------

        'rt133130' => <<'----------',
method sum_radlinks {
    my ( $global_radiation_matrix, $local_radiation_matrix, $rngg ) = @_;
    my ( $i, $j, $n1, $n2, $num );
    my $rggij;
    $num = @$rngg;
    for ( $i = 0 ; $i < $num ; $i++ ) {
        $n1 = $rngg->[$i];
        for ( $j = 0 ; $j < $num ; $j++ ) {
            $n2    = $rngg->[$j];
            $rggij = $local_radiation_matrix->[$i][$j];
            if ( $rggij && ( $n1 != $n2 ) ) {
                $global_radiation_matrix->[$n1][$n2] += $rggij;
            }
        }
    }
}
----------

        'scbb-csc' => <<'----------',
sub perlmod_install_advice
{
my(@mod) = @_;
if ($auto_install_cpan) {
require AutoInstall::Tk;
my $r = AutoInstall::Tk::do_autoinstall_tk(@mod);
if ($r > 0) {
for my $mod (@mod) {
warn "Re-require $mod...\n";
eval "require $mod";
die __LINE__ . ": $@" if $@;
}}
} 
else {
my $shell = ($os eq 'win' ? M"Eingabeaufforderung" : M"Shell");
status_message
(
Mfmt(
(
@mod > 1
? "Die fehlenden Perl-Module können aus der %s mit dem Kommando\n"
: "Das fehlende Perl-Modul kann aus der %s mit dem Kommando\n"
),
$shell
)
.
"    perl -MCPAN -e \"install " . join(", ", @mod) . "\"\n" .
"aus dem Internet geholt und installiert werden.\n",
"err"
);
} 
} 

----------

        'sot' => <<'----------',
$opt_c = Text::CSV_XS->new(
{
    binary       => 1, sep_char     => $opt_c, always_quote => 1,
}
);

$c->Tk::bind(
'<Control-f>' => sub {
my ($c) = @_;
my $e = $c->XEvent;
itemsUnderArea $c;
} );

__PACKAGE__->load_components( qw(
PK::Auto
Core
) );
----------

        'switch_plain' => <<'----------',
# run with -nola to keep default from outdenting
use Switch::Plain;
my $r = 'fail';
my $x = int rand 100_000;
nswitch (1 + $x * 2) {
    case $x: {}
    default: {
        $r = 'ok';
    }
}

my @words = qw(cinnamon ginger nutmeg cloves);
my $test = 1;
$r = $test
  ? do {
    sswitch( $words[ rand @words ] ) {
        case $words[0]:
        case $words[1]:
        case $words[2]:
        case $words[3]: { 'ok' }
      default: { 'default case' }
    }
  }
  : 'not ok';
----------
    };

    ####################################
    # BEGIN SECTION 3: Expected output #
    ####################################
    $rtests = {

        'lop.lop' => {
            source => "lop",
            params => "lop",
            expect => <<'#1...........',
# logical padding examples
$same =
  ( ( $aP eq $bP )
      && ( $aS eq $bS )
      && ( $aT eq $bT )
      && ( $a->{'title'} eq $b->{'title'} )
      && ( $a->{'href'} eq $b->{'href'} ) );

$bits =
  $top > 0xffff ? 32
  : $top > 0xff ? 16
  : $top > 1    ? 8
  :               1;

lc( $self->mime_attr('content-type')
      || $self->{MIH_DefaultType}
      || 'text/plain' );

if (1) { ... }

# Padding can also remove spaces; here the space after the '(' is lost:
elsif ( $statement_type =~ /^sub\b/
    || $paren_type[$paren_depth] =~ /^sub\b/ )
{
}
#1...........
        },

        'switch_plain.def' => {
            source => "switch_plain",
            params => "def",
            expect => <<'#2...........',
# run with -nola to keep default from outdenting
use Switch::Plain;
my $r = 'fail';
my $x = int rand 100_000;
nswitch( 1 + $x * 2 ) {
    case $x: { }
  default: {
        $r = 'ok';
    }
}

my @words = qw(cinnamon ginger nutmeg cloves);
my $test  = 1;
$r = $test
  ? do {
    sswitch( $words[ rand @words ] ) {
        case $words[0]:
        case $words[1]:
        case $words[2]:
        case $words[3]: { 'ok' }
      default: { 'default case' }
    }
  }
  : 'not ok';
#2...........
        },

        'switch_plain.switch_plain' => {
            source => "switch_plain",
            params => "switch_plain",
            expect => <<'#3...........',
# run with -nola to keep default from outdenting
use Switch::Plain;
my $r = 'fail';
my $x = int rand 100_000;
nswitch( 1 + $x * 2 ) {
    case $x: { }
    default: {
        $r = 'ok';
    }
}

my @words = qw(cinnamon ginger nutmeg cloves);
my $test  = 1;
$r = $test
  ? do {
    sswitch( $words[ rand @words ] ) {
        case $words[0]:
        case $words[1]:
        case $words[2]:
        case $words[3]: { 'ok' }
        default: { 'default case' }
    }
  }
  : 'not ok';
#3...........
        },

        'sot.def' => {
            source => "sot",
            params => "def",
            expect => <<'#4...........',
$opt_c = Text::CSV_XS->new(
    {
        binary       => 1,
        sep_char     => $opt_c,
        always_quote => 1,
    }
);

$c->Tk::bind(
    '<Control-f>' => sub {
        my ($c) = @_;
        my $e = $c->XEvent;
        itemsUnderArea $c;
    }
);

__PACKAGE__->load_components(
    qw(
      PK::Auto
      Core
    )
);
#4...........
        },

        'sot.sot' => {
            source => "sot",
            params => "sot",
            expect => <<'#5...........',
$opt_c = Text::CSV_XS->new( {
    binary       => 1,
    sep_char     => $opt_c,
    always_quote => 1,
} );

$c->Tk::bind(
    '<Control-f>' => sub {
        my ($c) = @_;
        my $e = $c->XEvent;
        itemsUnderArea $c;
    } );

__PACKAGE__->load_components( qw(
      PK::Auto
      Core
) );
#5...........
        },

        'prune.def' => {
            source => "prune",
            params => "def",
            expect => <<'#6...........',
# some tests for 'sub prune_alignment_tree'

$request->header( 'User-Agent' => $agent )              if $agent;
$request->header( 'From'       => $from )               if $from;
$request->header( 'Range'      => "bytes=0-$max_size" ) if $max_size;

for (
    [ 'CONSTANT', sub { join "foo", "bar" },         0, "bar" ],
    [ 'CONSTANT', sub { join "foo", "bar", 3 },      1, "barfoo3" ],
    [ '$var',     sub { join $_, "bar" },            0, "bar" ],
    [ '$myvar',   sub { my $var; join $var, "bar" }, 0, "bar" ],
);

[
    [ [NewXSHdr],     [ NewXSName, NewXSArgs ],            "XSHdr" ],
    [ [NewXSCHdrs],   [ NewXSName, NewXSArgs, GlobalNew ], "XSCHdrs" ],
    [ [DefSyms],      [StructName],                        "MkDefSyms" ],
    [ [NewXSSymTab],  [ DefSyms, NewXSArgs ],              "AddArgsyms" ],
    [ [NewXSLocals],  [NewXSSymTab],                       "Sym2Loc" ],
    [ [IsAffineFlag], [],                                  sub { return "0" } ],
];

@degen_nums[ 1, 2, 4, 8 ]         = ( 'a', 'c', 'g', 't' );
@degen_nums[ 5, 10, 9, 6, 3, 12 ] = ( 'r', 'y', 'w', 's', 'm', 'k' );
@degen_nums[ 14, 13, 11, 7, 15 ]  = ( 'b', 'd', 'h', 'v', 'n' );

$_CreateFile   = ff( "k32", "CreateFile",   [ P, N, N, N, N, N, N ], N );
$_CloseHandle  = ff( "k32", "CloseHandle",  [N],                     N );
$_GetCommState = ff( "k32", "GetCommState", [ N, P ],                I );
$_SetCommState = ff( "k32", "SetCommState", [ N, P ],                I );
$_SetupComm    = ff( "k32", "SetupComm",    [ N, N, N ],             I );
$_PurgeComm    = ff( "k32", "PurgeComm",    [ N, N ],                I );
$_CreateEvent  = ff( "k32", "CreateEvent",  [ P, I, I, P ],          N );

is_deeply \@t, [

    [3],            [0],            [1],            [0],
    3,              [1],            3,              [1],
    2,              [0],            [1],            [0],
    [1],            [1],            [1],            2,
    3,              [1],            2,              [3],
    4,              [ 7, 8 ],       9,              ["a"],
    "b",            3,              2,              5,
    3,              2,              5,              3,
    [2],            5,              4,              5,
    [ 3, 2, 1 ],    1,              2,              3,
    [ -1, -2, -3 ], [ -1, -2, -3 ], [ -1, -2, -3 ], [ -1, -2 ],
    3,              [ -1, -2 ],     3,              [ -1, -2, -3 ],
    [ !1 ],         [ 8, 7, 6 ],    [ 8, 7, 6 ],    [4],
    !!0,
];
#6...........
        },

        'align33.def' => {
            source => "align33",
            params => "def",
            expect => <<'#7...........',
$wl  = int( $wl * $f + .5 );
$wr  = int( $wr * $f + .5 );
$pag = int( $pageh * $f + .5 );
$fe  = $opt_F      ? "t" : "f";
$cf  = $opt_U      ? "t" : "f";
$tp  = $opt_t      ? "t" : "f";
$rm  = $numbstyle  ? "t" : "f";
$pa  = $showurl    ? "t" : "f";
$nh  = $seq_number ? "t" : "f";
#7...........
        },

        'gnu7.def' => {
            source => "gnu7",
            params => "def",
            expect => <<'#8...........',
# hanging side comments
if ( $seen == 1 ) {    # We're the first word so far to have
                       # this abbreviation.
    $hashref->{$abbrev} = $word;
}
elsif ( $seen == 2 ) {    # We're the second word to have this
                          # abbreviation, so we can't use it.
    delete $hashref->{$abbrev};
}
else {    # We're the third word to have this
          # abbreviation, so skip to the next word.
    next WORD;
}
#8...........
        },

        'gnu7.gnu' => {
            source => "gnu7",
            params => "gnu",
            expect => <<'#9...........',
# hanging side comments
if ($seen == 1)
{    # We're the first word so far to have
     # this abbreviation.
    $hashref->{$abbrev} = $word;
}
elsif ($seen == 2)
{    # We're the second word to have this
     # abbreviation, so we can't use it.
    delete $hashref->{$abbrev};
}
else
{    # We're the third word to have this
     # abbreviation, so skip to the next word.
    next WORD;
}
#9...........
        },

        'git33.def' => {
            source => "git33",
            params => "def",
            expect => <<'#10...........',
# test -wls='->' -wrs='->'
use Net::Ping;
my ($ping) = Net::Ping->new();
$ping->ping($host);

#10...........
        },

        'git33.git33' => {
            source => "git33",
            params => "git33",
            expect => <<'#11...........',
# test -wls='->' -wrs='->'
use Net::Ping;
my ($ping) = Net::Ping -> new();
$ping -> ping($host);

#11...........
        },

        'rt133130.def' => {
            source => "rt133130",
            params => "def",
            expect => <<'#12...........',
method sum_radlinks {
    my ( $global_radiation_matrix, $local_radiation_matrix, $rngg ) = @_;
    my ( $i, $j, $n1, $n2, $num );
    my $rggij;
    $num = @$rngg;
    for ( $i = 0 ; $i < $num ; $i++ ) {
        $n1 = $rngg->[$i];
        for ( $j = 0 ; $j < $num ; $j++ ) {
            $n2    = $rngg->[$j];
            $rggij = $local_radiation_matrix->[$i][$j];
            if ( $rggij && ( $n1 != $n2 ) ) {
                $global_radiation_matrix->[$n1][$n2] += $rggij;
            }
        }
    }
}
#12...........
        },

        'rt133130.rt133130' => {
            source => "rt133130",
            params => "rt133130",
            expect => <<'#13...........',
method sum_radlinks {
    my ( $global_radiation_matrix, $local_radiation_matrix, $rngg ) = @_;
    my ( $i, $j, $n1, $n2, $num );
    my $rggij;
    $num = @$rngg;
    for ( $i = 0 ; $i < $num ; $i++ ) {
        $n1 = $rngg->[$i];
        for ( $j = 0 ; $j < $num ; $j++ ) {
            $n2    = $rngg->[$j];
            $rggij = $local_radiation_matrix->[$i][$j];
            if ( $rggij && ( $n1 != $n2 ) ) {
                $global_radiation_matrix->[$n1][$n2] += $rggij;
            }
        }
    }
} ## end sub sum_radlinks
#13...........
        },

        'nib.def' => {
            source => "nib",
            params => "def",
            expect => <<'#14...........',
{ #<<<
{ #<<<
{    #++
    print "hello world\n";
}
}
}

{    #++
    {    #++
        { #<<<
        print "hello world\n";
        }
    }
}

#14...........
        },

        'nib.nib1' => {
            source => "nib",
            params => "nib1",
            expect => <<'#15...........',
{    #<<<
    {    #<<<
        {    #++
            print "hello world\n";
        }
    }
}

{    #++
    {    #++
        {    #<<<
            print "hello world\n";
        }
    }
}

#15...........
        },

        'nib.nib2' => {
            source => "nib",
            params => "nib2",
            expect => <<'#16...........',
{    #<<<
    {    #<<<
        { #++
        print "hello world\n";
        }
    }
}

{ #++
{ #++
{    #<<<
    print "hello world\n";
}
}
}

#16...........
        },

        'scbb-csc.def' => {
            source => "scbb-csc",
            params => "def",
            expect => <<'#17...........',
sub perlmod_install_advice {
    my (@mod) = @_;
    if ($auto_install_cpan) {
        require AutoInstall::Tk;
        my $r = AutoInstall::Tk::do_autoinstall_tk(@mod);
        if ( $r > 0 ) {
            for my $mod (@mod) {
                warn "Re-require $mod...\n";
                eval "require $mod";
                die __LINE__ . ": $@" if $@;
            }
        }
    }
    else {
        my $shell = ( $os eq 'win' ? M "Eingabeaufforderung" : M "Shell" );
        status_message(
            Mfmt(
                (
                    @mod > 1
                    ? "Die fehlenden Perl-Module können aus der %s mit dem Kommando\n"
                    : "Das fehlende Perl-Modul kann aus der %s mit dem Kommando\n"
                ),
                $shell
              )
              . "    perl -MCPAN -e \"install "
              . join( ", ", @mod ) . "\"\n"
              . "aus dem Internet geholt und installiert werden.\n",
            "err"
        );
    }
}

#17...........
        },

        'scbb-csc.scbb-csc' => {
            source => "scbb-csc",
            params => "scbb-csc",
            expect => <<'#18...........',
sub perlmod_install_advice {
    my (@mod) = @_;
    if ($auto_install_cpan) {
        require AutoInstall::Tk;
        my $r = AutoInstall::Tk::do_autoinstall_tk(@mod);
        if ( $r > 0 ) {
            for my $mod (@mod) {
                warn "Re-require $mod...\n";
                eval "require $mod";
                die __LINE__ . ": $@" if $@;
            }
        } ## end if ( $r > 0 )
    } ## end if ($auto_install_cpan)
    else {
        my $shell = ( $os eq 'win' ? M "Eingabeaufforderung" : M "Shell" );
        status_message(
            Mfmt(
                (
                    @mod > 1
                    ? "Die fehlenden Perl-Module können aus der %s mit dem Kommando\n"
                    : "Das fehlende Perl-Modul kann aus der %s mit dem Kommando\n"
                ),
                $shell
              )
              . "    perl -MCPAN -e \"install "
              . join( ", ", @mod ) . "\"\n"
              . "aus dem Internet geholt und installiert werden.\n",
            "err"
        );
    } ## end else [ if ($auto_install_cpan)]
} ## end sub perlmod_install_advice

#18...........
        },

        'here_long.def' => {
            source => "here_long",
            params => "def",
            expect => <<'#19...........',
# must not break after here target regardless of maximum-line-length
$sth = $dbh->prepare(<<"END_OF_SELECT") or die "Couldn't prepare SQL";
    SELECT COUNT(duration),SUM(duration) 
    FROM logins WHERE username='$user'
END_OF_SELECT

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

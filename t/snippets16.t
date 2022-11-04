# Created with: ./make_t.pl

# Contents:
#1 spp.spp1
#2 spp.spp2
#3 git16.def
#4 git10.def
#5 git10.git10
#6 multiple_equals.def
#7 align31.def
#8 almost1.def
#9 almost2.def
#10 almost3.def
#11 rt130394.def
#12 rt131115.def
#13 rt131115.rt131115
#14 ndsm1.def
#15 ndsm1.ndsm
#16 rt131288.def
#17 rt130394.rt130394
#18 git18.def
#19 here2.def

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
        'git10'    => "-wn -ce -cbl=sort,map,grep",
        'ndsm'     => "-ndsm",
        'rt130394' => "-olbn=1",
        'rt131115' => "-bli",
        'spp1'     => "-spp=1",
        'spp2'     => "-spp=2",
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

        'align31' => <<'----------',
# do not align the commas
$w->insert(
    ListBox => origin => [ 270, 160 ],
    size    => [ 200,           55 ],
);
----------

        'almost1' => <<'----------',
# not a good alignment
my $realname     = catfile( $dir,                  $file );
my $display_name = defined $disp ? catfile( $disp, $file ) : $file;
----------

        'almost2' => <<'----------',
# not a good alignment
my $substname = ( $indtot > 1            ? $indname . $indno : $indname );
my $incname   = $indname . ( $indtot > 1 ? $indno            : "" );
----------

        'almost3' => <<'----------',
# not a good alignment
sub head {
    match_on_type @_ => Null => sub { die "Cannot get head of Null" },
      ArrayRef       => sub         { $_->[0] };
}

----------

        'git10' => <<'----------',
# perltidy -wn -ce -cbl=sort,map,grep
@sorted = map {
    $_->[0]
} sort {
    $a->[1] <=> $b->[1] or $a->[0] cmp $b->[0]
} map {
    [ $_, length($_) ]
} @unsorted;
----------

        'git16' => <<'----------',
# git#16, two equality lines with fat commas on the right
my $Package = $Self->RepositoryGet( %Param, Result => 'SCALAR' );
my %Structure = $Self->PackageParse( String => $Package );
----------

        'git18' => <<'----------',
# parsing stuff like 'x17' before fat comma
my %bb = (
    123x18 => '123x18',
    123 x19 => '123 x19', 
    123x 20 => '123x 20',
    2 x 7    => '2 x 7', 
    x40      => 'x40',
    'd' x17    => "'d' x17",
    c x17    => 'c x17', 
);
foreach my $key ( keys %bb ) {
    print "key='$key' => $bb{$key}\n";
}
----------

        'here2' => <<'----------',
$_ = "";
s|(?:)|"${\<<END}"
ok $test - here2.in "" in multiline s///e outside eval
END
|e;
print $_ || "not ok $test\n";
----------

        'multiple_equals' => <<'----------',
# ignore second '=' here
$|          = $debug = 1 if $opt_d;
$full_index = 1          if $opt_i;
$query_all  = $opt_A     if $opt_A;

# not aligning multiple '='s here
$start   = $end     = $len = $ismut = $number = $allele_ori = $allele_mut =
  $proof = $xxxxreg = $reg = $dist  = '';
----------

        'ndsm1' => <<'----------',
;;;;; # 1 trapped semicolon 
sub numerically {$a <=> $b};
;;;;; 
sub Numerically {$a <=> $b};  # trapped semicolon
@: = qw;2c72656b636168 
  2020202020 
  ;; __;
----------

        'rt130394' => <<'----------',
# rt130394: keep on one line with -olbn=1
$factorial = sub { reduce { $a * $b } 1 .. 11 };
----------

        'rt131115' => <<'----------',
# closing braces to be inteded with -bli
sub a {
    my %uniq;
    foreach my $par (@_) {
        $uniq{$par} = 1;
    }
}
----------

        'rt131288' => <<'----------',
sub OptArgs2::STYLE_FULL { 3 }
$style == OptArgs2::STYLE_FULL ? 'FullUsage' : 'NormalUsage', 'usage: ' . $usage . "\n";
----------

        'spp' => <<'----------',
sub get_val() { }

sub get_Val  () { }

sub Get_val		() { }
my $sub1=sub                     () { };
my $sub2=sub () { };
----------
    };

    ####################################
    # BEGIN SECTION 3: Expected output #
    ####################################
    $rtests = {

        'spp.spp1' => {
            source => "spp",
            params => "spp1",
            expect => <<'#1...........',
sub get_val() { }

sub get_Val () { }

sub Get_val () { }
my $sub1 = sub () { };
my $sub2 = sub () { };
#1...........
        },

        'spp.spp2' => {
            source => "spp",
            params => "spp2",
            expect => <<'#2...........',
sub get_val () { }

sub get_Val () { }

sub Get_val () { }
my $sub1 = sub () { };
my $sub2 = sub () { };
#2...........
        },

        'git16.def' => {
            source => "git16",
            params => "def",
            expect => <<'#3...........',
# git#16, two equality lines with fat commas on the right
my $Package   = $Self->RepositoryGet( %Param, Result => 'SCALAR' );
my %Structure = $Self->PackageParse( String => $Package );
#3...........
        },

        'git10.def' => {
            source => "git10",
            params => "def",
            expect => <<'#4...........',
# perltidy -wn -ce -cbl=sort,map,grep
@sorted =
  map  { $_->[0] }
  sort { $a->[1] <=> $b->[1] or $a->[0] cmp $b->[0] }
  map  { [ $_, length($_) ] } @unsorted;
#4...........
        },

        'git10.git10' => {
            source => "git10",
            params => "git10",
            expect => <<'#5...........',
# perltidy -wn -ce -cbl=sort,map,grep
@sorted = map {
    $_->[0]
  } sort {
    $a->[1] <=> $b->[1] or $a->[0] cmp $b->[0]
  } map {
    [ $_, length($_) ]
  } @unsorted;
#5...........
        },

        'multiple_equals.def' => {
            source => "multiple_equals",
            params => "def",
            expect => <<'#6...........',
# ignore second '=' here
$|          = $debug = 1 if $opt_d;
$full_index = 1          if $opt_i;
$query_all  = $opt_A     if $opt_A;

# not aligning multiple '='s here
$start = $end = $len = $ismut = $number = $allele_ori = $allele_mut = $proof =
  $xxxxreg = $reg = $dist = '';
#6...........
        },

        'align31.def' => {
            source => "align31",
            params => "def",
            expect => <<'#7...........',
# do not align the commas
$w->insert(
    ListBox => origin => [ 270, 160 ],
    size    => [ 200, 55 ],
);
#7...........
        },

        'almost1.def' => {
            source => "almost1",
            params => "def",
            expect => <<'#8...........',
# not a good alignment
my $realname     = catfile( $dir, $file );
my $display_name = defined $disp ? catfile( $disp, $file ) : $file;
#8...........
        },

        'almost2.def' => {
            source => "almost2",
            params => "def",
            expect => <<'#9...........',
# not a good alignment
my $substname = ( $indtot > 1 ? $indname . $indno : $indname );
my $incname   = $indname . ( $indtot > 1 ? $indno : "" );
#9...........
        },

        'almost3.def' => {
            source => "almost3",
            params => "def",
            expect => <<'#10...........',
# not a good alignment
sub head {
    match_on_type @_ => Null => sub { die "Cannot get head of Null" },
      ArrayRef       => sub { $_->[0] };
}

#10...........
        },

        'rt130394.def' => {
            source => "rt130394",
            params => "def",
            expect => <<'#11...........',
# rt130394: keep on one line with -olbn=1
$factorial = sub {
    reduce { $a * $b } 1 .. 11;
};
#11...........
        },

        'rt131115.def' => {
            source => "rt131115",
            params => "def",
            expect => <<'#12...........',
# closing braces to be inteded with -bli
sub a {
    my %uniq;
    foreach my $par (@_) {
        $uniq{$par} = 1;
    }
}
#12...........
        },

        'rt131115.rt131115' => {
            source => "rt131115",
            params => "rt131115",
            expect => <<'#13...........',
# closing braces to be inteded with -bli
sub a
  {
    my %uniq;
    foreach my $par (@_)
      {
        $uniq{$par} = 1;
      }
  }
#13...........
        },

        'ndsm1.def' => {
            source => "ndsm1",
            params => "def",
            expect => <<'#14...........',
;    # 1 trapped semicolon
sub numerically { $a <=> $b }

sub Numerically { $a <=> $b };    # trapped semicolon
@: = qw;2c72656b636168
  2020202020
  ;;
__;
#14...........
        },

        'ndsm1.ndsm' => {
            source => "ndsm1",
            params => "ndsm",
            expect => <<'#15...........',
;
;
;
;
;    # 1 trapped semicolon
sub numerically { $a <=> $b };
;
;
;
;
;
sub Numerically { $a <=> $b };    # trapped semicolon
@: = qw;2c72656b636168
  2020202020
  ;;
__;
#15...........
        },

        'rt131288.def' => {
            source => "rt131288",
            params => "def",
            expect => <<'#16...........',
sub OptArgs2::STYLE_FULL { 3 }
$style == OptArgs2::STYLE_FULL ? 'FullUsage' : 'NormalUsage',
  'usage: ' . $usage . "\n";
#16...........
        },

        'rt130394.rt130394' => {
            source => "rt130394",
            params => "rt130394",
            expect => <<'#17...........',
# rt130394: keep on one line with -olbn=1
$factorial = sub { reduce { $a * $b } 1 .. 11 };
#17...........
        },

        'git18.def' => {
            source => "git18",
            params => "def",
            expect => <<'#18...........',
# parsing stuff like 'x17' before fat comma
my %bb = (
    123 x 18 => '123x18',
    123 x 19 => '123 x19',
    123 x 20 => '123x 20',
    2 x 7    => '2 x 7',
    x40      => 'x40',
    'd' x 17 => "'d' x17",
    c x17    => 'c x17',
);
foreach my $key ( keys %bb ) {
    print "key='$key' => $bb{$key}\n";
}
#18...........
        },

        'here2.def' => {
            source => "here2",
            params => "def",
            expect => <<'#19...........',
$_ = "";
s|(?:)|"${\<<END}"
ok $test - here2.in "" in multiline s///e outside eval
END
|e;
print $_ || "not ok $test\n";
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

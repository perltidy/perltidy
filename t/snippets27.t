# Created with: ./make_t.pl

# Contents:
#1 wtc.wtc1
#2 wtc.wtc2
#3 wtc.wtc3
#4 wtc.wtc4
#5 wtc.wtc5
#6 wtc.wtc6
#7 dwic.def
#8 dwic.dwic
#9 wtc.wtc7
#10 rt144979.def
#11 rt144979.rt144979

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
        'dwic'     => "-wn -dwic",
        'rt144979' => "-xci -ce -lp",
        'wtc1'     => "-wtc=0 -dtc",
        'wtc2'     => "-wtc=1 -atc",
        'wtc3'     => "-wtc=m -atc",
        'wtc4'     => "-wtc=m -atc -dtc",
        'wtc5'     => "-wtc=b -atc -dtc -vtc=2",
        'wtc6'     => "-wtc=i -atc -dtc -vtc=2",
        'wtc7'     => "-wtc=h -atc -dtc -vtc=2",
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

        'dwic' => <<'----------',
    skip_symbols(
        [ qw(
            Perl_dump_fds
            Perl_ErrorNo
            Perl_GetVars
            PL_sys_intern
        ) ],
    );
----------

        'rt144979' => <<'----------',
# part 1
GetOptions(
      "format|f=s" => sub {
          my ( $n, $v ) = @_;
          if ( ( my $k = $formats{$v} ) ) {
              $format = $k;
      } else {
              die("--format must be 'system' or 'user'\n");
          }
          return;
      },
); 

# part 2
{{{
            my $desc =
              $access
              ? "for -$op under use filetest 'access' $desc_tail"
              : "for -$op $desc_tail";
            {
                local $SIG{__WARN__} = sub {
                    my $w = shift;
                    if ($w =~ /^File::stat ignores VMS ACLs/)
                    {
                        ++$vwarn;
                      } elsif (
                              $w =~ /^File::stat ignores use filetest 'access'/)
                    {
                        ++$awarn;
                    } else
                    {
                        $warnings .= $w;
                    }
                };
                $rv = eval "$access; -$op \$stat";
            }
}}}

----------

        'wtc' => <<'----------',
# both single and multiple line lists:
@LoL = (
    [ "fred",   "barney", ],
    [ "george", "jane",  "elroy" ],
    [ "homer",  "marge", "bart", ],
);

# single line
( $name, $body ) = ( $2, $3, );

# multiline, but not bare
$text = $main->Scrolled( TextUndo, $yyy, $zzz, $wwwww,
    selectbackgroundxxxxx => 'yellow', );

# this will pass for 'h'
my $new = {
      %$item,
      text => $leaf,
      color => 'green',
};

# matches 'i'
my @list = (

    $xx,
    $yy
);

# does not match 'h'
$c1->create(
    'rectangle', 40, 60, 80, 80,
    -fill => 'red',
    -tags => 'rectangle'
);

$dasm_frame->Button(
    -text    => 'Locate',
    -command => sub {
        $target_binary = $fs->Show( -popover => 'cursor', -create  => 1, );
    },
)->pack( -side => 'left', );

my $no_index_1_1 =
  { 'map' =>
      { ':key' => { name => \&string, list => { value => \&string }, }, }, };


----------
    };

    ####################################
    # BEGIN SECTION 3: Expected output #
    ####################################
    $rtests = {

        'wtc.wtc1' => {
            source => "wtc",
            params => "wtc1",
            expect => <<'#1...........',
# both single and multiple line lists:
@LoL = (
    [ "fred",   "barney" ],
    [ "george", "jane",  "elroy" ],
    [ "homer",  "marge", "bart" ]
);

# single line
( $name, $body ) = ( $2, $3 );

# multiline, but not bare
$text = $main->Scrolled( TextUndo, $yyy, $zzz, $wwwww,
    selectbackgroundxxxxx => 'yellow' );

# this will pass for 'h'
my $new = {
    %$item,
    text  => $leaf,
    color => 'green'
};

# matches 'i'
my @list = (

    $xx,
    $yy
);

# does not match 'h'
$c1->create(
    'rectangle', 40, 60, 80, 80,
    -fill => 'red',
    -tags => 'rectangle'
);

$dasm_frame->Button(
    -text    => 'Locate',
    -command => sub {
        $target_binary = $fs->Show( -popover => 'cursor', -create => 1 );
    }
)->pack( -side => 'left' );

my $no_index_1_1 =
  { 'map' => { ':key' => { name => \&string, list => { value => \&string } } }
  };

#1...........
        },

        'wtc.wtc2' => {
            source => "wtc",
            params => "wtc2",
            expect => <<'#2...........',
# both single and multiple line lists:
@LoL = (
    [ "fred",   "barney", ],
    [ "george", "jane",  "elroy", ],
    [ "homer",  "marge", "bart", ],
);

# single line
( $name, $body, ) = ( $2, $3, );

# multiline, but not bare
$text = $main->Scrolled( TextUndo, $yyy, $zzz, $wwwww,
    selectbackgroundxxxxx => 'yellow', );

# this will pass for 'h'
my $new = {
    %$item,
    text  => $leaf,
    color => 'green',
};

# matches 'i'
my @list = (

    $xx,
    $yy,
);

# does not match 'h'
$c1->create(
    'rectangle', 40, 60, 80, 80,
    -fill => 'red',
    -tags => 'rectangle',
);

$dasm_frame->Button(
    -text    => 'Locate',
    -command => sub {
        $target_binary = $fs->Show( -popover => 'cursor', -create => 1, );
    },
)->pack( -side => 'left', );

my $no_index_1_1 =
  { 'map' =>
      { ':key' => { name => \&string, list => { value => \&string }, }, }, };

#2...........
        },

        'wtc.wtc3' => {
            source => "wtc",
            params => "wtc3",
            expect => <<'#3...........',
# both single and multiple line lists:
@LoL = (
    [ "fred",   "barney", ],
    [ "george", "jane",  "elroy" ],
    [ "homer",  "marge", "bart", ],
);

# single line
( $name, $body ) = ( $2, $3, );

# multiline, but not bare
$text = $main->Scrolled( TextUndo, $yyy, $zzz, $wwwww,
    selectbackgroundxxxxx => 'yellow', );

# this will pass for 'h'
my $new = {
    %$item,
    text  => $leaf,
    color => 'green',
};

# matches 'i'
my @list = (

    $xx,
    $yy,
);

# does not match 'h'
$c1->create(
    'rectangle', 40, 60, 80, 80,
    -fill => 'red',
    -tags => 'rectangle',
);

$dasm_frame->Button(
    -text    => 'Locate',
    -command => sub {
        $target_binary = $fs->Show( -popover => 'cursor', -create => 1, );
    },
)->pack( -side => 'left', );

my $no_index_1_1 =
  { 'map' =>
      { ':key' => { name => \&string, list => { value => \&string }, }, }, };

#3...........
        },

        'wtc.wtc4' => {
            source => "wtc",
            params => "wtc4",
            expect => <<'#4...........',
# both single and multiple line lists:
@LoL = (
    [ "fred",   "barney" ],
    [ "george", "jane",  "elroy" ],
    [ "homer",  "marge", "bart" ],
);

# single line
( $name, $body ) = ( $2, $3 );

# multiline, but not bare
$text = $main->Scrolled( TextUndo, $yyy, $zzz, $wwwww,
    selectbackgroundxxxxx => 'yellow', );

# this will pass for 'h'
my $new = {
    %$item,
    text  => $leaf,
    color => 'green',
};

# matches 'i'
my @list = (

    $xx,
    $yy,
);

# does not match 'h'
$c1->create(
    'rectangle', 40, 60, 80, 80,
    -fill => 'red',
    -tags => 'rectangle',
);

$dasm_frame->Button(
    -text    => 'Locate',
    -command => sub {
        $target_binary = $fs->Show( -popover => 'cursor', -create => 1 );
    },
)->pack( -side => 'left' );

my $no_index_1_1 =
  { 'map' => { ':key' => { name => \&string, list => { value => \&string } } },
  };

#4...........
        },

        'wtc.wtc5' => {
            source => "wtc",
            params => "wtc5",
            expect => <<'#5...........',
# both single and multiple line lists:
@LoL = (
    [ "fred",   "barney" ],
    [ "george", "jane",  "elroy" ],
    [ "homer",  "marge", "bart" ],
);

# single line
( $name, $body ) = ( $2, $3 );

# multiline, but not bare
$text = $main->Scrolled( TextUndo, $yyy, $zzz, $wwwww,
    selectbackgroundxxxxx => 'yellow' );

# this will pass for 'h'
my $new = {
    %$item,
    text  => $leaf,
    color => 'green',
};

# matches 'i'
my @list = (

    $xx,
    $yy,
);

# does not match 'h'
$c1->create(
    'rectangle', 40, 60, 80, 80,
    -fill => 'red',
    -tags => 'rectangle',
);

$dasm_frame->Button(
    -text    => 'Locate',
    -command => sub {
        $target_binary = $fs->Show( -popover => 'cursor', -create => 1 );
    },
)->pack( -side => 'left' );

my $no_index_1_1 =
  { 'map' => { ':key' => { name => \&string, list => { value => \&string } } }
  };

#5...........
        },

        'wtc.wtc6' => {
            source => "wtc",
            params => "wtc6",
            expect => <<'#6...........',
# both single and multiple line lists:
@LoL = (
    [ "fred",   "barney" ],
    [ "george", "jane",  "elroy" ],
    [ "homer",  "marge", "bart" ] );

# single line
( $name, $body ) = ( $2, $3 );

# multiline, but not bare
$text = $main->Scrolled( TextUndo, $yyy, $zzz, $wwwww,
    selectbackgroundxxxxx => 'yellow' );

# this will pass for 'h'
my $new = {
    %$item,
    text  => $leaf,
    color => 'green',
};

# matches 'i'
my @list = (

    $xx,
    $yy,
);

# does not match 'h'
$c1->create(
    'rectangle', 40, 60, 80, 80,
    -fill => 'red',
    -tags => 'rectangle' );

$dasm_frame->Button(
    -text    => 'Locate',
    -command => sub {
        $target_binary = $fs->Show( -popover => 'cursor', -create => 1 );
    },
)->pack( -side => 'left' );

my $no_index_1_1 =
  { 'map' => { ':key' => { name => \&string, list => { value => \&string } } }
  };

#6...........
        },

        'dwic.def' => {
            source => "dwic",
            params => "def",
            expect => <<'#7...........',
    skip_symbols(
        [
            qw(
              Perl_dump_fds
              Perl_ErrorNo
              Perl_GetVars
              PL_sys_intern
            )
        ],
    );
#7...........
        },

        'dwic.dwic' => {
            source => "dwic",
            params => "dwic",
            expect => <<'#8...........',
    skip_symbols( [ qw(
        Perl_dump_fds
        Perl_ErrorNo
        Perl_GetVars
        PL_sys_intern
    ) ] );
#8...........
        },

        'wtc.wtc7' => {
            source => "wtc",
            params => "wtc7",
            expect => <<'#9...........',
# both single and multiple line lists:
@LoL = (
    [ "fred",   "barney" ],
    [ "george", "jane",  "elroy" ],
    [ "homer",  "marge", "bart" ] );

# single line
( $name, $body ) = ( $2, $3 );

# multiline, but not bare
$text = $main->Scrolled( TextUndo, $yyy, $zzz, $wwwww,
    selectbackgroundxxxxx => 'yellow' );

# this will pass for 'h'
my $new = {
    %$item,
    text  => $leaf,
    color => 'green',
};

# matches 'i'
my @list = (

    $xx,
    $yy );

# does not match 'h'
$c1->create(
    'rectangle', 40, 60, 80, 80,
    -fill => 'red',
    -tags => 'rectangle' );

$dasm_frame->Button(
    -text    => 'Locate',
    -command => sub {
        $target_binary = $fs->Show( -popover => 'cursor', -create => 1 );
    },
)->pack( -side => 'left' );

my $no_index_1_1 =
  { 'map' => { ':key' => { name => \&string, list => { value => \&string } } }
  };

#9...........
        },

        'rt144979.def' => {
            source => "rt144979",
            params => "def",
            expect => <<'#10...........',
# part 1
GetOptions(
    "format|f=s" => sub {
        my ( $n, $v ) = @_;
        if ( ( my $k = $formats{$v} ) ) {
            $format = $k;
        }
        else {
            die("--format must be 'system' or 'user'\n");
        }
        return;
    },
);

# part 2
{
    {
        {
            my $desc =
              $access
              ? "for -$op under use filetest 'access' $desc_tail"
              : "for -$op $desc_tail";
            {
                local $SIG{__WARN__} = sub {
                    my $w = shift;
                    if ( $w =~ /^File::stat ignores VMS ACLs/ ) {
                        ++$vwarn;
                    }
                    elsif ( $w =~ /^File::stat ignores use filetest 'access'/ )
                    {
                        ++$awarn;
                    }
                    else {
                        $warnings .= $w;
                    }
                };
                $rv = eval "$access; -$op \$stat";
            }
        }
    }
}

#10...........
        },

        'rt144979.rt144979' => {
            source => "rt144979",
            params => "rt144979",
            expect => <<'#11...........',
# part 1
GetOptions(
      "format|f=s" => sub {
          my ( $n, $v ) = @_;
          if ( ( my $k = $formats{$v} ) ) {
              $format = $k;
          } else {
              die("--format must be 'system' or 'user'\n");
          }
          return;
      },
);

# part 2
{
    {
        {
            my $desc =
              $access
              ? "for -$op under use filetest 'access' $desc_tail"
              : "for -$op $desc_tail";
            {
                local $SIG{__WARN__} = sub {
                    my $w = shift;
                    if ( $w =~ /^File::stat ignores VMS ACLs/ ) {
                        ++$vwarn;
                    } elsif (
                             $w =~ /^File::stat ignores use filetest 'access'/ )
                    {
                        ++$awarn;
                    } else {
                        $warnings .= $w;
                    }
                };
                $rv = eval "$access; -$op \$stat";
            }
        }
    }
}

#11...........
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

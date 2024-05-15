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
#12 bfvt.bfvt0
#13 bfvt.bfvt2
#14 bfvt.def
#15 cpb.cpb
#16 cpb.def
#17 rt145706.def
#18 olbxl.def
#19 olbxl.olbxl1

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
        'bfvt0'    => "-bfvt=0",
        'bfvt2'    => "-bfvt=2",
        'cpb'      => "-cpb",
        'def'      => "",
        'dwic'     => "-wn -dwic",
        'olbxl1'   => "-olbxl=eval",
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

        'bfvt' => <<'----------',
# combines with -bfvt>0
eval {
    require XSLoader;
    XSLoader::load( 'Sys::Syslog', $VERSION );
    1;
}
  or do {
    require DynaLoader;
    push @ISA, 'DynaLoader';
    bootstrap Sys::Syslog $VERSION;
  };

# combines with -bfvt=2
eval {
    ( $line, $cond ) = $self->_normalize_if_elif($line);
    1;
}
  or die sprintf "Error at line %d\nLine %d: %s\n%s",
  ( $line_info->start_line_num() ) x 2, $line, $@;

# stable for bfvt<2; combines for bfvt=2; has ci
my $domain = shift
  || eval {
    require Net::Domain;
    Net::Domain::hostfqdn();
}
  || "";

# stays combined for all bfvt; has ci
my $domain = shift
  || eval {
    require Net::Domain;
    Net::Domain::hostfqdn();
} || "";
----------

        'cpb' => <<'----------',
foreach my $dir (
    '05_lexer', '07_token', '08_regression', '11_util',
    '13_data',  '15_transform'
  )
{
    my @perl = find_files( catdir( 't', 'data', $dir ) );
    push @files, @perl;
}

----------

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

        'olbxl' => <<'----------',
            eval {
               require Ace };

            @list = map {
                $frm{ ( /@(.*?)>/ ? $1 : $_ ) }++ ? () : ($_);
            } @list;

            $color = join(
                '/',
                sort {
                    $color_value{$::a} <=> $color_value{$::b};
                } keys %colors
            );

            @sorted = sort {
                $SortDir * $PageTotal{$a} <=> $SortDir * $PageTotal{$b}
                };
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

        'rt145706' => <<'----------',
# some tests for default setting --use-feature=class, rt145706
class Example::Subclass1 : isa(Example::Base) { ... }
class Example::Subclass2 : isa(Example::Base 2.345) { ... }
class Example::Subclass3 : isa(Example::Base) 1.345 { ... }
field $y : param(the_y_value);
class Pointer 2.0 {
    field $x : param;
    field $y : param;

    method to_string() {
        return "($x, $y)";
    }
}

ADJUST {
    $x = 0;
}

# these should not produce errors
method paint => sub {
    ...;
};
method painter

  => sub {
    ...;
  };
is( ( method Pack "a", "b", "c" ), "method,a,b,c" );
class ExtendsBasicAttributes is BasicAttributes{
 ...
}
class BrokenExtendsBasicAttributes
is BasicAttributes{
 ...
}
class +Night with +Bad {
    public nine { return 'crazy' }
};
my $x = field(50);
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
      { ':key' => { name => \&string, list => { value => \&string, }, }, }, };

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

        'bfvt.bfvt0' => {
            source => "bfvt",
            params => "bfvt0",
            expect => <<'#12...........',
# combines with -bfvt>0
eval {
    require XSLoader;
    XSLoader::load( 'Sys::Syslog', $VERSION );
    1;
}
  or do {
    require DynaLoader;
    push @ISA, 'DynaLoader';
    bootstrap Sys::Syslog $VERSION;
  };

# combines with -bfvt=2
eval {
    ( $line, $cond ) = $self->_normalize_if_elif($line);
    1;
}
  or die sprintf "Error at line %d\nLine %d: %s\n%s",
  ( $line_info->start_line_num() ) x 2, $line, $@;

# stable for bfvt<2; combines for bfvt=2; has ci
my $domain = shift
  || eval {
    require Net::Domain;
    Net::Domain::hostfqdn();
  }
  || "";

# stays combined for all bfvt; has ci
my $domain = shift
  || eval {
    require Net::Domain;
    Net::Domain::hostfqdn();
  } || "";
#12...........
        },

        'bfvt.bfvt2' => {
            source => "bfvt",
            params => "bfvt2",
            expect => <<'#13...........',
# combines with -bfvt>0
eval {
    require XSLoader;
    XSLoader::load( 'Sys::Syslog', $VERSION );
    1;
} or do {
    require DynaLoader;
    push @ISA, 'DynaLoader';
    bootstrap Sys::Syslog $VERSION;
};

# combines with -bfvt=2
eval {
    ( $line, $cond ) = $self->_normalize_if_elif($line);
    1;
} or die sprintf "Error at line %d\nLine %d: %s\n%s",
  ( $line_info->start_line_num() ) x 2, $line, $@;

# stable for bfvt<2; combines for bfvt=2; has ci
my $domain = shift
  || eval {
    require Net::Domain;
    Net::Domain::hostfqdn();
  } || "";

# stays combined for all bfvt; has ci
my $domain = shift
  || eval {
    require Net::Domain;
    Net::Domain::hostfqdn();
  } || "";
#13...........
        },

        'bfvt.def' => {
            source => "bfvt",
            params => "def",
            expect => <<'#14...........',
# combines with -bfvt>0
eval {
    require XSLoader;
    XSLoader::load( 'Sys::Syslog', $VERSION );
    1;
} or do {
    require DynaLoader;
    push @ISA, 'DynaLoader';
    bootstrap Sys::Syslog $VERSION;
};

# combines with -bfvt=2
eval {
    ( $line, $cond ) = $self->_normalize_if_elif($line);
    1;
}
  or die sprintf "Error at line %d\nLine %d: %s\n%s",
  ( $line_info->start_line_num() ) x 2, $line, $@;

# stable for bfvt<2; combines for bfvt=2; has ci
my $domain = shift
  || eval {
    require Net::Domain;
    Net::Domain::hostfqdn();
  }
  || "";

# stays combined for all bfvt; has ci
my $domain = shift
  || eval {
    require Net::Domain;
    Net::Domain::hostfqdn();
  } || "";
#14...........
        },

        'cpb.cpb' => {
            source => "cpb",
            params => "cpb",
            expect => <<'#15...........',
foreach my $dir (
    '05_lexer', '07_token', '08_regression', '11_util',
    '13_data',  '15_transform'
) {
    my @perl = find_files( catdir( 't', 'data', $dir ) );
    push @files, @perl;
}

#15...........
        },

        'cpb.def' => {
            source => "cpb",
            params => "def",
            expect => <<'#16...........',
foreach my $dir (
    '05_lexer', '07_token', '08_regression', '11_util',
    '13_data',  '15_transform'
  )
{
    my @perl = find_files( catdir( 't', 'data', $dir ) );
    push @files, @perl;
}

#16...........
        },

        'rt145706.def' => {
            source => "rt145706",
            params => "def",
            expect => <<'#17...........',
# some tests for default setting --use-feature=class, rt145706
class Example::Subclass1 : isa(Example::Base) { ... }

class Example::Subclass2 : isa(Example::Base 2.345) { ... }

class Example::Subclass3 : isa(Example::Base) 1.345 { ... }
field $y : param(the_y_value);

class Pointer 2.0 {
    field $x : param;
    field $y : param;

    method to_string() {
        return "($x, $y)";
    }
}

ADJUST {
    $x = 0;
}

# these should not produce errors
method paint => sub {
    ...;
};
method painter

  => sub {
    ...;
  };
is( ( method Pack "a", "b", "c" ), "method,a,b,c" );
class ExtendsBasicAttributes is BasicAttributes {
    ...
}
class BrokenExtendsBasicAttributes is BasicAttributes {
    ...
}
class +Night with +Bad {
    public nine { return 'crazy' }
};
my $x = field(50);
#17...........
        },

        'olbxl.def' => {
            source => "olbxl",
            params => "def",
            expect => <<'#18...........',
            eval { require Ace };

            @list =
              map { $frm{ ( /@(.*?)>/ ? $1 : $_ ) }++ ? () : ($_); } @list;

            $color = join( '/',
                sort { $color_value{$::a} <=> $color_value{$::b}; }
                  keys %colors );

            @sorted =
              sort { $SortDir * $PageTotal{$a} <=> $SortDir * $PageTotal{$b} };
#18...........
        },

        'olbxl.olbxl1' => {
            source => "olbxl",
            params => "olbxl1",
            expect => <<'#19...........',
            eval {
                require Ace;
            };

            @list =
              map { $frm{ ( /@(.*?)>/ ? $1 : $_ ) }++ ? () : ($_); } @list;

            $color = join( '/',
                sort { $color_value{$::a} <=> $color_value{$::b}; }
                  keys %colors );

            @sorted =
              sort { $SortDir * $PageTotal{$a} <=> $SortDir * $PageTotal{$b} };
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

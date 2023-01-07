# Created with: ./make_t.pl

# Contents:
#1 here_long.here_long
#2 bbhb.bbhb2
#3 bbhb.bbhb3
#4 bbhb.def
#5 bbhb.bbhb4
#6 bbhb.bbhb5
#7 braces.braces7
#8 xci.def
#9 xci.xci1
#10 xci.xci2
#11 mangle4.def
#12 mangle4.mangle
#13 extrude5.def
#14 extrude5.extrude
#15 kba1.def
#16 kba1.kba1
#17 git45.def
#18 git45.git45
#19 boa.boa

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
        'bbhb2' => "-bbhb=2 -bbp=2",
        'bbhb3' => "-bbhb=3 -bbp=3",
        'bbhb4' => "-bbhb=3 -bbp=3 -bbhbi=2 -bbpi=2",
        'bbhb5' => "-bbhb=3 -bbp=3 -bbhbi=1 -bbpi=1",
        'boa'   => <<'----------',
# -boa is default so we test nboa
-nboa
----------
        'braces7' => <<'----------',
-bli -blil='*' -blixl='eval'
----------
        'def'       => "",
        'extrude'   => "--extrude",
        'git45'     => "-vtc=1 -wn",
        'here_long' => "-l=33",
        'kba1'      => <<'----------',
-kbb='=> ,' -kba='=>'
----------
        'mangle' => "--mangle",
        'xci1'   => "-xci",
        'xci2'   => "-pbp -nst -nse -xci",
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

        'bbhb' => <<'----------',
my %temp = 
( 
supsup => 123, 
nested => { 
asdf => 456, 
yarg => 'yarp', 
}, );
----------

        'boa' => <<'----------',
my @field
  : field
  : Default(1)
  : Get('Name' => 'foo') 
  : Set('Name');
----------

        'braces' => <<'----------',
sub message {
    if ( !defined( $_[0] ) ) {
        print("Hello, World\n");
    }
    else {
        print( $_[0], "\n" );
    }
}

$myfun = sub {
    print("Hello, World\n");
};

eval {
    my $app = App::perlbrew->new( "install-patchperl", "-q" );
    $app->run();
} or do {
    $error          = $@;
    $produced_error = 1;
};

Mojo::IOLoop->next_tick(
    sub {
        $ua->get(
            '/' => sub {
                push @kept_alive, pop->kept_alive;
                Mojo::IOLoop->next_tick( sub { Mojo::IOLoop->stop } );
            }
        );
    }
);

$r = do {
    sswitch( $words[ rand @words ] ) {
        case $words[0]:
        case $words[1]:
        case $words[2]:
        case $words[3]: { 'ok' }
      default: { 'wtf' }
    }
};

try {
    die;
}
catch {
    die;
};
----------

        'extrude5' => <<'----------',
use perl6-alpha;
$var{-y} = 1;
----------

        'git45' => <<'----------',
# git#45 -vtc=n and -wn were not working together
if (
    $self->_add_fqdn_host(
        name  => $name,
        realm => $realm
    )
  )
{
    ...;
}

# do not stack )->pack(
my $hlist = $control::control->Scrolled(
    'HList',
    drawbranch  => 1,
    width       => 20,
    -scrollbars => 'w'
)->pack(
    -side   => 'bottom',
    -expand => 1
);

----------

        'here_long' => <<'----------',
# must not break after here target regardless of maximum-line-length
$sth= $dbh->prepare (<<"END_OF_SELECT") or die "Couldn't prepare SQL" ;
    SELECT COUNT(duration),SUM(duration) 
    FROM logins WHERE username='$user'
END_OF_SELECT

----------

        'kba1' => <<'----------',
$this_env = join("", $before, $closures
	  , $contents
	  , ($defenv ? '': &balance_tags())
	  , $reopens ); $_ = $after;

method 'foo1'
  => [ Int, Int ]
  => sub {
    my ( $self, $x, $y ) = ( shift, @_ );
    ...;
  };

method 'foo2'=>
  [ Int, Int ]=>
  sub {
    my ( $self, $x, $y ) = ( shift, @_ );
    ...;
  };

----------

        'mangle4' => <<'----------',
# a useful parsing test from 'signatures.t'
use feature "signatures";
no warnings "experimental::signatures";
sub t086
    ( #foo)))
    $ #foo)))
    a #foo)))
    , #foo)))
    , #foo)))
    $ #foo)))
    b #foo)))
    = #foo)))
    333 #foo)))
    , #foo)))
    , #foo)))
    ) #foo)))
    { $a.$b }
----------

        'xci' => <<'----------',
$self->{_text} = (
  !$section  ? ''
 : $type eq 'item' ? "the $section entry"
 : "the section on $section"
 )
 . (
 $page
 ? ( $section ? ' in ' : '' ) . "the $page$page_ext manpage"
 : ' elsewhere in this document'
 );

my $otherHashRef =
 $condition
 ? {
 'a' => 'a value',
 'b' => 'b value',
 'c' => {
  'd' => 'd value',
  'e' => 'e value'
 }
 }
 : undef;

my @globlist = ( grep { defined } @opt{qw( l q S t )} )
  ? do {
    local *DIR;
    opendir DIR, './' or die "can't opendir './': $!";
    my @a = grep { not /^\.+$/ } readdir DIR;
    closedir DIR;
    @a;
  }
  : ();
----------
    };

    ####################################
    # BEGIN SECTION 3: Expected output #
    ####################################
    $rtests = {

        'here_long.here_long' => {
            source => "here_long",
            params => "here_long",
            expect => <<'#1...........',
# must not break after here target regardless of maximum-line-length
$sth =
  $dbh->prepare(
    <<"END_OF_SELECT") or die "Couldn't prepare SQL";
    SELECT COUNT(duration),SUM(duration) 
    FROM logins WHERE username='$user'
END_OF_SELECT

#1...........
        },

        'bbhb.bbhb2' => {
            source => "bbhb",
            params => "bbhb2",
            expect => <<'#2...........',
my %temp =
  (
    supsup => 123,
    nested =>
      {
        asdf => 456,
        yarg => 'yarp',
      },
  );
#2...........
        },

        'bbhb.bbhb3' => {
            source => "bbhb",
            params => "bbhb3",
            expect => <<'#3...........',
my %temp =
  (
    supsup => 123,
    nested =>
      {
        asdf => 456,
        yarg => 'yarp',
      },
  );
#3...........
        },

        'bbhb.def' => {
            source => "bbhb",
            params => "def",
            expect => <<'#4...........',
my %temp = (
    supsup => 123,
    nested => {
        asdf => 456,
        yarg => 'yarp',
    },
);
#4...........
        },

        'bbhb.bbhb4' => {
            source => "bbhb",
            params => "bbhb4",
            expect => <<'#5...........',
my %temp =
    (
    supsup => 123,
    nested =>
        {
        asdf => 456,
        yarg => 'yarp',
        },
    );
#5...........
        },

        'bbhb.bbhb5' => {
            source => "bbhb",
            params => "bbhb5",
            expect => <<'#6...........',
my %temp =
(
    supsup => 123,
    nested =>
    {
        asdf => 456,
        yarg => 'yarp',
    },
);
#6...........
        },

        'braces.braces7' => {
            source => "braces",
            params => "braces7",
            expect => <<'#7...........',
sub message
  {
    if ( !defined( $_[0] ) )
      {
        print("Hello, World\n");
      }
    else
      {
        print( $_[0], "\n" );
      }
  }

$myfun = sub
  {
    print("Hello, World\n");
  };

eval {
    my $app = App::perlbrew->new( "install-patchperl", "-q" );
    $app->run();
} or do
  {
    $error          = $@;
    $produced_error = 1;
  };

Mojo::IOLoop->next_tick(
    sub
      {
        $ua->get(
            '/' => sub
              {
                push @kept_alive, pop->kept_alive;
                Mojo::IOLoop->next_tick( sub { Mojo::IOLoop->stop } );
              }
        );
      }
);

$r = do
  {
    sswitch( $words[ rand @words ] )
      {
        case $words[0]:
        case $words[1]:
        case $words[2]:
        case $words[3]: { 'ok' }
      default: { 'wtf' }
      }
  };

try
  {
    die;
  }
catch
  {
    die;
  };
#7...........
        },

        'xci.def' => {
            source => "xci",
            params => "def",
            expect => <<'#8...........',
$self->{_text} = (
     !$section        ? ''
    : $type eq 'item' ? "the $section entry"
    :                   "the section on $section"
  )
  . (
    $page
    ? ( $section ? ' in ' : '' ) . "the $page$page_ext manpage"
    : ' elsewhere in this document'
  );

my $otherHashRef =
  $condition
  ? {
    'a' => 'a value',
    'b' => 'b value',
    'c' => {
        'd' => 'd value',
        'e' => 'e value'
    }
  }
  : undef;

my @globlist = ( grep { defined } @opt{qw( l q S t )} )
  ? do {
    local *DIR;
    opendir DIR, './' or die "can't opendir './': $!";
    my @a = grep { not /^\.+$/ } readdir DIR;
    closedir DIR;
    @a;
  }
  : ();
#8...........
        },

        'xci.xci1' => {
            source => "xci",
            params => "xci1",
            expect => <<'#9...........',
$self->{_text} = (
     !$section        ? ''
    : $type eq 'item' ? "the $section entry"
    :                   "the section on $section"
  )
  . (
      $page
      ? ( $section ? ' in ' : '' ) . "the $page$page_ext manpage"
      : ' elsewhere in this document'
  );

my $otherHashRef =
  $condition
  ? {
      'a' => 'a value',
      'b' => 'b value',
      'c' => {
          'd' => 'd value',
          'e' => 'e value'
      }
  }
  : undef;

my @globlist = ( grep { defined } @opt{qw( l q S t )} )
  ? do {
      local *DIR;
      opendir DIR, './' or die "can't opendir './': $!";
      my @a = grep { not /^\.+$/ } readdir DIR;
      closedir DIR;
      @a;
  }
  : ();
#9...........
        },

        'xci.xci2' => {
            source => "xci",
            params => "xci2",
            expect => <<'#10...........',
$self->{_text} = (
     !$section        ? ''
    : $type eq 'item' ? "the $section entry"
    :                   "the section on $section"
    )
    . ( $page
        ? ( $section ? ' in ' : '' ) . "the $page$page_ext manpage"
        : ' elsewhere in this document'
    );

my $otherHashRef
    = $condition
    ? { 'a' => 'a value',
        'b' => 'b value',
        'c' => {
            'd' => 'd value',
            'e' => 'e value'
        }
    }
    : undef;

my @globlist = ( grep {defined} @opt{qw( l q S t )} )
    ? do {
        local *DIR;
        opendir DIR, './' or die "can't opendir './': $!";
        my @a = grep { not /^\.+$/ } readdir DIR;
        closedir DIR;
        @a;
    }
    : ();
#10...........
        },

        'mangle4.def' => {
            source => "mangle4",
            params => "def",
            expect => <<'#11...........',
# a useful parsing test from 'signatures.t'
use feature "signatures";
no warnings "experimental::signatures";

sub t086 (    #foo)))
    $         #foo)))
      a       #foo)))
    ,         #foo)))
    ,         #foo)))
    $         #foo)))
      b       #foo)))
      =       #foo)))
      333     #foo)))
    ,         #foo)))
    ,         #foo)))
  )           #foo)))
{ $a . $b }
#11...........
        },

        'mangle4.mangle' => {
            source => "mangle4",
            params => "mangle",
            expect => <<'#12...........',
# a useful parsing test from 'signatures.t'
use feature "signatures";
no warnings "experimental::signatures";
sub t086(#foo)))
$ #foo)))
  a#foo)))
,#foo)))
,#foo)))
$ #foo)))
  b#foo)))
  =#foo)))
  333#foo)))
,#foo)))
,#foo)))
  )#foo)))
{$a.$b}
#12...........
        },

        'extrude5.def' => {
            source => "extrude5",
            params => "def",
            expect => <<'#13...........',
use perl6-alpha;
$var{-y} = 1;
#13...........
        },

        'extrude5.extrude' => {
            source => "extrude5",
            params => "extrude",
            expect => <<'#14...........',
use
perl6-alpha
;
$var{-y}
=
1
;
#14...........
        },

        'kba1.def' => {
            source => "kba1",
            params => "def",
            expect => <<'#15...........',
$this_env = join( "",
    $before, $closures, $contents, ( $defenv ? '' : &balance_tags() ),
    $reopens );
$_ = $after;

method 'foo1' => [ Int, Int ] => sub {
    my ( $self, $x, $y ) = ( shift, @_ );
    ...;
};

method 'foo2' => [ Int, Int ] => sub {
    my ( $self, $x, $y ) = ( shift, @_ );
    ...;
};

#15...........
        },

        'kba1.kba1' => {
            source => "kba1",
            params => "kba1",
            expect => <<'#16...........',
$this_env = join(
    "", $before, $closures
    ,   $contents
    , ( $defenv ? '' : &balance_tags() )
    , $reopens
);
$_ = $after;

method 'foo1'
  => [ Int, Int ]
  => sub {
    my ( $self, $x, $y ) = ( shift, @_ );
    ...;
  };

method 'foo2'  =>
  [ Int, Int ] =>
  sub {
    my ( $self, $x, $y ) = ( shift, @_ );
    ...;
  };

#16...........
        },

        'git45.def' => {
            source => "git45",
            params => "def",
            expect => <<'#17...........',
# git#45 -vtc=n and -wn were not working together
if (
    $self->_add_fqdn_host(
        name  => $name,
        realm => $realm
    )
  )
{
    ...;
}

# do not stack )->pack(
my $hlist = $control::control->Scrolled(
    'HList',
    drawbranch  => 1,
    width       => 20,
    -scrollbars => 'w'
)->pack(
    -side   => 'bottom',
    -expand => 1
);

#17...........
        },

        'git45.git45' => {
            source => "git45",
            params => "git45",
            expect => <<'#18...........',
# git#45 -vtc=n and -wn were not working together
if ( $self->_add_fqdn_host(
    name  => $name,
    realm => $realm ) )
{
    ...;
}

# do not stack )->pack(
my $hlist = $control::control->Scrolled(
    'HList',
    drawbranch  => 1,
    width       => 20,
    -scrollbars => 'w'
)->pack(
    -side   => 'bottom',
    -expand => 1 );

#18...........
        },

        'boa.boa' => {
            source => "boa",
            params => "boa",
            expect => <<'#19...........',
my @field : field : Default(1) : Get('Name' => 'foo') : Set('Name');
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

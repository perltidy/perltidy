# Created with: ./make_t.pl

# Contents:
#1 novalign.def
#2 novalign.novalign1
#3 novalign.novalign2
#4 novalign.novalign3
#5 lp2.def
#6 lp2.lp
#7 braces.braces8
#8 rt140025.def
#9 rt140025.rt140025
#10 xlp1.def
#11 xlp1.xlp1
#12 git74.def
#13 git74.git74
#14 git77.def
#15 git77.git77
#16 vxl.def
#17 vxl.vxl1
#18 vxl.vxl2
#19 bal.bal1

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
        'bal1'    => "-bal=1",
        'braces8' => <<'----------',
-bl -bbvt=1 -blxl=' ' -bll='sub do asub'
----------
        'def'   => "",
        'git74' => <<'----------',
-xlp
--iterations=2
--maximum-line-length=120
--line-up-parentheses
--continuation-indentation=4
--closing-token-indentation=1
--want-left-space="= -> ( )"
--want-right-space="= -> ( )"
--space-function-paren
--space-keyword-paren
--space-terminal-semicolon
--opening-brace-on-new-line
--opening-sub-brace-on-new-line
--opening-anonymous-sub-brace-on-new-line
--brace-left-and-indent
--brace-left-and-indent-list="*"
--break-before-hash-brace=3
----------
        'git77' => <<'----------',
-gal='Grep Map'
----------
        'lp'        => "-lp",
        'novalign1' => "-novalign",
        'novalign2' => "-nvsc -nvbc -msc=2",
        'novalign3' => "-nvc",
        'rt140025'  => "-lp -xci -ci=4 -ce",
        'vxl1'      => <<'----------',
-vxl='='
----------
        'vxl2' => <<'----------',
-vxl='*' -vil='='
----------
        'xlp1' => "-xlp",
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

        'bal' => <<'----------',
{
  L1:
  L2:
  L3: return;
};
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

        'git74' => <<'----------',
$self->func(
  {
    command  => [ 'command', 'argument1', 'argument2' ],
    callback => sub {
      my ($res) = @_;
      print($res);
    }
  }
);

my $test_var = $self->test_call(    #
    $arg1,
    $arg2
);

my $test_var = $self->test_call(
    $arg1,                          #
    $arg2
);

my $test_var = $self->test_call(
    #
    $arg1,
    $arg2,
);

my $test_var = $self->test_call(

    $arg1,
    $arg2,
);

my $test_var = $self->test_call(
    $arg1,
    $arg2

);

my $test_var = $self->test_call(

    $arg1,
    $arg2,

);

my $test_var =

  $self->test_call(
    $arg1,
    $arg2

  );

----------

        'git77' => <<'----------',
# These should format about the same with -gal='Map Grep'.
# NOTE: The braces only align if the internal code flag ALIGN_GREP_ALIASES is set
    return +{
        Map  {
$_->init_arg => $_->get_value($instance) }
        Grep { $_->has_value($instance) }
        Grep {
defined( $_->init_arg ) }
$class->get_all_attributes
    };

    return +{
        map  {
$_->init_arg => $_->get_value($instance) }
        grep { $_->has_value($instance) }
        grep {
defined( $_->init_arg ) }
$class->get_all_attributes
    };
----------

        'lp2' => <<'----------',
# test issue git #74, lost -lp when final anon sub brace followed by '}'
Util::Parser->new(
    Handlers => {
        Init  => sub { $self->init(@_) },
        Mid =>  { sub { shift; $self->mid(@_) } },
        Final => sub { shift; $self->final(@_) }
    }
)->parse( $_[0] );
----------

        'novalign' => <<'----------',
{
# simple vertical alignment of '=' and '#'
# A long line to test -nvbc ... normally this will cause the previous line to move left
my $lines = 0;    # checksum: #lines
my $bytes = 0;    # checksum: #bytes
my $sum = 0;    # checksum: system V sum
my $patchdata = 0;    # saw patch data
my $pos = 0;    # start of patch data
                                         # a hanging side comment
my $endkit = 0;    # saw end of kit
my $fail = 0;    # failed
}

----------

        'rt140025' => <<'----------',
eval {
my $cpid;
my $cmd;

 FORK: {
 if( $cpid = fork ) {
 close( STDOUT );
 last;
 } elsif( defined $cpid ) {
 close( STDIN );
 open( STDIN, '<', '/dev/null' ) or die( "open3: $!\n" );
 exec $cmd or die( "exec: $!\n" );
 } elsif( $! == EAGAIN ) {
 sleep 3;
 redo FORK;
 } else {
 die( "Can't fork: $!\n" );
 }
 }
};
----------

        'vxl' => <<'----------',
# if equals is excluded then ternary is automatically excluded
# side comment alignments always remain
$co_description = ($color) ? 'bold cyan' : '';          # description
$co_prompt      = ($color) ? 'bold green' : '';         # prompt
$co_unused      = ($color) ? 'on_green' : 'reverse';    # unused
----------

        'xlp1' => <<'----------',
# test -xlp with comments, broken sub blocks, blank line, line length limit
$cb1 = $act_page->Checkbutton(
  -text     => M "Verwenden",
  -variable => \$qualitaet_s_optimierung,
  -command  => sub {
    change_state_all( $act_page1, $qualitaet_s_optimierung, { $cb1 => 1 } )
      ;    # sc
  },
)->grid(

  # block comment
  -row    => $gridy++,
  -column => 2,
  -sticky => 'e'
);
----------
    };

    ####################################
    # BEGIN SECTION 3: Expected output #
    ####################################
    $rtests = {

        'novalign.def' => {
            source => "novalign",
            params => "def",
            expect => <<'#1...........',
{
# simple vertical alignment of '=' and '#'
# A long line to test -nvbc ... normally this will cause the previous line to move left
    my $lines     = 0;    # checksum: #lines
    my $bytes     = 0;    # checksum: #bytes
    my $sum       = 0;    # checksum: system V sum
    my $patchdata = 0;    # saw patch data
    my $pos       = 0;    # start of patch data
                          # a hanging side comment
    my $endkit    = 0;    # saw end of kit
    my $fail      = 0;    # failed
}

#1...........
        },

        'novalign.novalign1' => {
            source => "novalign",
            params => "novalign1",
            expect => <<'#2...........',
{
    # simple vertical alignment of '=' and '#'
# A long line to test -nvbc ... normally this will cause the previous line to move left
    my $lines = 0;    # checksum: #lines
    my $bytes = 0;    # checksum: #bytes
    my $sum = 0;    # checksum: system V sum
    my $patchdata = 0;    # saw patch data
    my $pos = 0;    # start of patch data
                    # a hanging side comment
    my $endkit = 0;    # saw end of kit
    my $fail = 0;    # failed
}

#2...........
        },

        'novalign.novalign2' => {
            source => "novalign",
            params => "novalign2",
            expect => <<'#3...........',
{
    # simple vertical alignment of '=' and '#'
# A long line to test -nvbc ... normally this will cause the previous line to move left
    my $lines     = 0;  # checksum: #lines
    my $bytes     = 0;  # checksum: #bytes
    my $sum       = 0;  # checksum: system V sum
    my $patchdata = 0;  # saw patch data
    my $pos       = 0;  # start of patch data
      # a hanging side comment
    my $endkit = 0;  # saw end of kit
    my $fail = 0;  # failed
}

#3...........
        },

        'novalign.novalign3' => {
            source => "novalign",
            params => "novalign3",
            expect => <<'#4...........',
{
# simple vertical alignment of '=' and '#'
# A long line to test -nvbc ... normally this will cause the previous line to move left
    my $lines = 0;        # checksum: #lines
    my $bytes = 0;        # checksum: #bytes
    my $sum = 0;          # checksum: system V sum
    my $patchdata = 0;    # saw patch data
    my $pos = 0;          # start of patch data
                          # a hanging side comment
    my $endkit = 0;       # saw end of kit
    my $fail = 0;         # failed
}

#4...........
        },

        'lp2.def' => {
            source => "lp2",
            params => "def",
            expect => <<'#5...........',
# test issue git #74, lost -lp when final anon sub brace followed by '}'
Util::Parser->new(
    Handlers => {
        Init  => sub { $self->init(@_) },
        Mid   => { sub { shift; $self->mid(@_) } },
        Final => sub { shift; $self->final(@_) }
    }
)->parse( $_[0] );
#5...........
        },

        'lp2.lp' => {
            source => "lp2",
            params => "lp",
            expect => <<'#6...........',
# test issue git #74, lost -lp when final anon sub brace followed by '}'
Util::Parser->new(
                   Handlers => {
                                 Init  => sub { $self->init(@_) },
                                 Mid   => { sub { shift; $self->mid(@_) } },
                                 Final => sub { shift; $self->final(@_) }
                   }
)->parse( $_[0] );
#6...........
        },

        'braces.braces8' => {
            source => "braces",
            params => "braces8",
            expect => <<'#7...........',
sub message
{   if ( !defined( $_[0] ) ) {
        print("Hello, World\n");
    }
    else {
        print( $_[0], "\n" );
    }
}

$myfun = sub
{   print("Hello, World\n");
};

eval {
    my $app = App::perlbrew->new( "install-patchperl", "-q" );
    $app->run();
} or do
{   $error          = $@;
    $produced_error = 1;
};

Mojo::IOLoop->next_tick(
    sub
    {   $ua->get(
            '/' => sub
            {   push @kept_alive, pop->kept_alive;
                Mojo::IOLoop->next_tick( sub { Mojo::IOLoop->stop } );
            }
        );
    }
);

$r = do
{   sswitch( $words[ rand @words ] ) {
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
#7...........
        },

        'rt140025.def' => {
            source => "rt140025",
            params => "def",
            expect => <<'#8...........',
eval {
    my $cpid;
    my $cmd;

  FORK: {
        if ( $cpid = fork ) {
            close(STDOUT);
            last;
        }
        elsif ( defined $cpid ) {
            close(STDIN);
            open( STDIN, '<', '/dev/null' ) or die("open3: $!\n");
            exec $cmd                       or die("exec: $!\n");
        }
        elsif ( $! == EAGAIN ) {
            sleep 3;
            redo FORK;
        }
        else {
            die("Can't fork: $!\n");
        }
    }
};
#8...........
        },

        'rt140025.rt140025' => {
            source => "rt140025",
            params => "rt140025",
            expect => <<'#9...........',
eval {
    my $cpid;
    my $cmd;

FORK: {
        if ( $cpid = fork ) {
            close(STDOUT);
            last;
        } elsif ( defined $cpid ) {
            close(STDIN);
            open( STDIN, '<', '/dev/null' ) or die("open3: $!\n");
            exec $cmd                       or die("exec: $!\n");
        } elsif ( $! == EAGAIN ) {
            sleep 3;
            redo FORK;
        } else {
            die("Can't fork: $!\n");
        }
    }
};
#9...........
        },

        'xlp1.def' => {
            source => "xlp1",
            params => "def",
            expect => <<'#10...........',
# test -xlp with comments, broken sub blocks, blank line, line length limit
$cb1 = $act_page->Checkbutton(
    -text     => M "Verwenden",
    -variable => \$qualitaet_s_optimierung,
    -command  => sub {
        change_state_all( $act_page1, $qualitaet_s_optimierung, { $cb1 => 1 } )
          ;    # sc
    },
)->grid(

    # block comment
    -row    => $gridy++,
    -column => 2,
    -sticky => 'e'
);
#10...........
        },

        'xlp1.xlp1' => {
            source => "xlp1",
            params => "xlp1",
            expect => <<'#11...........',
# test -xlp with comments, broken sub blocks, blank line, line length limit
$cb1 = $act_page->Checkbutton(
                              -text     => M "Verwenden",
                              -variable => \$qualitaet_s_optimierung,
                              -command  => sub {
                                  change_state_all( $act_page1,
                                       $qualitaet_s_optimierung, { $cb1 => 1 } )
                                    ;    # sc
                              },
)->grid(

        # block comment
        -row    => $gridy++,
        -column => 2,
        -sticky => 'e'
);
#11...........
        },

        'git74.def' => {
            source => "git74",
            params => "def",
            expect => <<'#12...........',
$self->func(
    {
        command  => [ 'command', 'argument1', 'argument2' ],
        callback => sub {
            my ($res) = @_;
            print($res);
        }
    }
);

my $test_var = $self->test_call(    #
    $arg1,
    $arg2
);

my $test_var = $self->test_call(
    $arg1,                          #
    $arg2
);

my $test_var = $self->test_call(
    #
    $arg1,
    $arg2,
);

my $test_var = $self->test_call(

    $arg1,
    $arg2,
);

my $test_var = $self->test_call(
    $arg1,
    $arg2

);

my $test_var = $self->test_call(

    $arg1,
    $arg2,

);

my $test_var =

  $self->test_call(
    $arg1,
    $arg2

  );

#12...........
        },

        'git74.git74' => {
            source => "git74",
            params => "git74",
            expect => <<'#13...........',
$self -> func (
                {
                   command  => [ 'command', 'argument1', 'argument2' ],
                   callback => sub
                       {
                       my ($res) = @_ ;
                       print ($res) ;
                       }
                }
              ) ;

my $test_var = $self -> test_call (    #
                                    $arg1,
                                    $arg2
                                  ) ;

my $test_var = $self -> test_call (
                                    $arg1,    #
                                    $arg2
                                  ) ;

my $test_var = $self -> test_call (
                                   #
                                   $arg1,
                                   $arg2,
                                  ) ;

my $test_var = $self -> test_call (

                                   $arg1,
                                   $arg2,
                                  ) ;

my $test_var = $self -> test_call (
                                    $arg1,
                                    $arg2

                                  ) ;

my $test_var = $self -> test_call (

                                   $arg1,
                                   $arg2,

                                  ) ;

my $test_var =

    $self -> test_call (
                         $arg1,
                         $arg2

                       ) ;

#13...........
        },

        'git77.def' => {
            source => "git77",
            params => "def",
            expect => <<'#14...........',
# These should format about the same with -gal='Map Grep'.
# NOTE: The braces only align if the internal code flag ALIGN_GREP_ALIASES is set
    return +{
        Map {
            $_->init_arg => $_->get_value($instance)
        } Grep { $_->has_value($instance) }
        Grep {
            defined( $_->init_arg )
        }
        $class->get_all_attributes
    };

    return +{
        map  { $_->init_arg => $_->get_value($instance) }
        grep { $_->has_value($instance) }
        grep { defined( $_->init_arg ) } $class->get_all_attributes
    };
#14...........
        },

        'git77.git77' => {
            source => "git77",
            params => "git77",
            expect => <<'#15...........',
# These should format about the same with -gal='Map Grep'.
# NOTE: The braces only align if the internal code flag ALIGN_GREP_ALIASES is set
    return +{
        Map { $_->init_arg => $_->get_value($instance) }
        Grep { $_->has_value($instance) }
        Grep { defined( $_->init_arg ) } $class->get_all_attributes
    };

    return +{
        map  { $_->init_arg => $_->get_value($instance) }
        grep { $_->has_value($instance) }
        grep { defined( $_->init_arg ) } $class->get_all_attributes
    };
#15...........
        },

        'vxl.def' => {
            source => "vxl",
            params => "def",
            expect => <<'#16...........',
# if equals is excluded then ternary is automatically excluded
# side comment alignments always remain
$co_description = ($color) ? 'bold cyan'  : '';           # description
$co_prompt      = ($color) ? 'bold green' : '';           # prompt
$co_unused      = ($color) ? 'on_green'   : 'reverse';    # unused
#16...........
        },

        'vxl.vxl1' => {
            source => "vxl",
            params => "vxl1",
            expect => <<'#17...........',
# if equals is excluded then ternary is automatically excluded
# side comment alignments always remain
$co_description = ($color) ? 'bold cyan' : '';     # description
$co_prompt = ($color) ? 'bold green' : '';         # prompt
$co_unused = ($color) ? 'on_green' : 'reverse';    # unused
#17...........
        },

        'vxl.vxl2' => {
            source => "vxl",
            params => "vxl2",
            expect => <<'#18...........',
# if equals is excluded then ternary is automatically excluded
# side comment alignments always remain
$co_description = ($color) ? 'bold cyan' : '';          # description
$co_prompt      = ($color) ? 'bold green' : '';         # prompt
$co_unused      = ($color) ? 'on_green' : 'reverse';    # unused
#18...........
        },

        'bal.bal1' => {
            source => "bal",
            params => "bal1",
            expect => <<'#19...........',
{
  L1:
  L2:
  L3:
    return;
};
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

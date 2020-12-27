# Created with: ./make_t.pl

# Contents:
#1 boa.def
#2 bol.bol
#3 bol.def
#4 bot.bot
#5 bot.def
#6 hash_bang.def
#7 hash_bang.hash_bang
#8 listop1.listop1
#9 sbcp.def
#10 sbcp.sbcp1
#11 wnxl.def
#12 wnxl.wnxl1
#13 wnxl.wnxl2
#14 wnxl.wnxl3
#15 wnxl.wnxl4
#16 align34.def
#17 git47.def
#18 git47.git47
#19 qw.def

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
        'bol' => <<'----------',
# -bol is default, so test -nbol
-nbol
----------
        'bot' => <<'----------',
# -bot is default so we test -nbot
-nbot
----------
        'def'   => "",
        'git47' => <<'----------',
# perltidyrc from git #47
-pbp     # Start with Perl Best Practices
-w       # Show all warnings
-iob     # Ignore old breakpoints
-l=120   # 120 characters per line
-mbl=2   # No more than 2 blank lines
-i=2     # Indentation is 2 columns
-ci=2    # Continuation indentation is 2 columns
-vt=0    # Less vertical tightness
-pt=2    # High parenthesis tightness
-bt=2    # High brace tightness
-sbt=2   # High square bracket tightness
-wn      # Weld nested containers
-isbc    # Don't indent comments without leading space
-nst     # Don't output to STDOUT
----------
        'hash_bang' => "-x",
        'listop1'   => <<'----------',
# -bok is default so we test nbok
-nbok
----------
        'sbcp1' => <<'----------',
-sbc -sbcp='#x#'
----------
        'wnxl1' => <<'----------',
# only weld parens, and only if leading keyword
-wn -wnxl='^K( [ { q'
----------
        'wnxl2' => <<'----------',
# do not weld leading '['
-wn -wnxl='^['
----------
        'wnxl3' => <<'----------',
# do not weld interior or ending '{' without a keyword
-wn -wnxl='.K{'

----------
        'wnxl4' => <<'----------',
# do not weld except parens or trailing brace with keyword
-wn -wnxl='.K{ ^{ ['
----------
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

        'align34' => <<'----------',
# align all '{' and runs of '='
if    ( $line =~ /^NAME>(.*)/i )       { $Cookies{'name'} = $1; }
elsif ( $line =~ /^EMAIL>(.*)/i )      { $email = $1; }
elsif ( $line =~ /^IP_ADDRESS>(.*)/i ) { $ipaddress = $1; }
elsif ( $line =~ /^<!--(.*)-->/i )     { $remoteuser = $1; }
elsif ( $line =~ /^PASSWORD>(.*)/i )   { next; }
elsif ( $line =~ /^IMAGE>(.*)/i )      { $image_url = $1; }
elsif ( $line =~ /^LINKNAME>(.*)/i )   { $linkname = $1; }
elsif ( $line =~ /^LINKURL>(.*)/i )    { $linkurl = $1; }
else                                   { $body .= $line; }
----------

        'boa' => <<'----------',
my @field
  : field
  : Default(1)
  : Get('Name' => 'foo') 
  : Set('Name');
----------

        'bol' => <<'----------',
return unless $cmd = $cmd || ($dot 
          && $Last_Shell) || &prompt('|');
----------

        'bot' => <<'----------',
$foo =
  $condition
  ? undef
  : 1;
----------

        'git47' => <<'----------',
# cannot weld here
$promises[$i]->then(
    sub { $all->resolve(@_); () },
    sub {
        $results->[$i] = [@_];
        $all->reject(@$results) if --$remaining <= 0;
        return ();
    }
);

sub _absolutize { [
    map { _is_scoped($_) ? $_ : [ [ [ 'pc', 'scope' ] ], ' ', @$_ ] }
      @{ shift() } ] }

$c->helpers->log->debug( sub {
    my $req    = $c->req;
    my $method = $req->method;
    my $path   = $req->url->path->to_abs_string;
    $c->helpers->timing->begin('mojo.timer');
    return qq{$method "$path"};
} ) unless $stash->{'mojo.static'};

# A single signature var can weld
return Mojo::Promise->resolve($query_params)->then(&_reveal_event)->then(
    sub ($code) {
        return $c->render( text => '', status => $code );
    }
);
----------

        'hash_bang' => <<'----------',




# above spaces will be retained with -x but not by default
#!/usr/bin/perl
my $date = localtime();
----------

        'listop1' => <<'----------',
my @sorted = map { $_->[0] }
  sort { $a->[1] <=> $b->[1] }
  map { [ $_, rand ] } @list;
----------

        'qw' => <<'----------',
    # do not outdent ending ) more than initial qw line
    if ( $pos == 0 ) {
        @return = grep( /^$word/,
            sort qw(
              ! a b d h i m o q r u autobundle clean
              make test install force reload look
        ) );
    }

    # outdent ')' even if opening is not '('
    @EXPORT = (
        qw)
          i Re Im rho theta arg
          sqrt log ln
          log10 logn cbrt root
          cplx cplxe
        ),
        @trig
    );

    # outdent '>' like ')'
    @EXPORT = (
        qw<
          i Re Im rho theta arg
          sqrt log ln
          log10 logn cbrt root
          cplx cplxe
        >,
        @trig
    );

    # but ';' not outdented
    @EXPORT = (
        qw;
          i Re Im rho theta arg
          sqrt log ln
          log10 logn cbrt root
          cplx cplxe
          ;,
        @trig
    );
----------

        'sbcp' => <<'----------',
@month_of_year = (
    'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct',
#x# 'Dec', 'Nov'
## 'Dec', 'Nov'
    'Nov', 'Dec'
);
----------

        'wnxl' => <<'----------',
if ( $PLATFORM eq 'aix' ) {
    skip_symbols(
        [ qw(
            Perl_dump_fds
            Perl_ErrorNo
            Perl_GetVars
            PL_sys_intern
        ) ]
    );
}

if ( _add_fqdn_host(
    name => ...,
    fqdn => ...
) )
{
    ...;
}

do {{
    next if ($n % 2);
    print $n, "\n";
}} while ($n++ < 10);

threads->create( sub {
    my (%hash3);
    share(%hash3);
    $hash2{hash} = \%hash3;
    $hash3{"thread"} = "yes";
} )->join();
----------
    };

    ####################################
    # BEGIN SECTION 3: Expected output #
    ####################################
    $rtests = {

        'boa.def' => {
            source => "boa",
            params => "def",
            expect => <<'#1...........',
my @field
  : field
  : Default(1)
  : Get('Name' => 'foo')
  : Set('Name');
#1...........
        },

        'bol.bol' => {
            source => "bol",
            params => "bol",
            expect => <<'#2...........',
return unless $cmd = $cmd || ( $dot && $Last_Shell ) || &prompt('|');
#2...........
        },

        'bol.def' => {
            source => "bol",
            params => "def",
            expect => <<'#3...........',
return
  unless $cmd = $cmd
  || ( $dot
    && $Last_Shell )
  || &prompt('|');
#3...........
        },

        'bot.bot' => {
            source => "bot",
            params => "bot",
            expect => <<'#4...........',
$foo = $condition ? undef : 1;
#4...........
        },

        'bot.def' => {
            source => "bot",
            params => "def",
            expect => <<'#5...........',
$foo =
  $condition
  ? undef
  : 1;
#5...........
        },

        'hash_bang.def' => {
            source => "hash_bang",
            params => "def",
            expect => <<'#6...........',

# above spaces will be retained with -x but not by default
#!/usr/bin/perl
my $date = localtime();
#6...........
        },

        'hash_bang.hash_bang' => {
            source => "hash_bang",
            params => "hash_bang",
            expect => <<'#7...........',




# above spaces will be retained with -x but not by default
#!/usr/bin/perl
my $date = localtime();
#7...........
        },

        'listop1.listop1' => {
            source => "listop1",
            params => "listop1",
            expect => <<'#8...........',
my @sorted =
  map { $_->[0] } sort { $a->[1] <=> $b->[1] } map { [ $_, rand ] } @list;
#8...........
        },

        'sbcp.def' => {
            source => "sbcp",
            params => "def",
            expect => <<'#9...........',
@month_of_year = (
    'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct',

    #x# 'Dec', 'Nov'
## 'Dec', 'Nov'
    'Nov', 'Dec'
);
#9...........
        },

        'sbcp.sbcp1' => {
            source => "sbcp",
            params => "sbcp1",
            expect => <<'#10...........',
@month_of_year = (
    'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct',
#x# 'Dec', 'Nov'
    ## 'Dec', 'Nov'
    'Nov', 'Dec'
);
#10...........
        },

        'wnxl.def' => {
            source => "wnxl",
            params => "def",
            expect => <<'#11...........',
if ( $PLATFORM eq 'aix' ) {
    skip_symbols(
        [
            qw(
              Perl_dump_fds
              Perl_ErrorNo
              Perl_GetVars
              PL_sys_intern
            )
        ]
    );
}

if (
    _add_fqdn_host(
        name => ...,
        fqdn => ...
    )
  )
{
    ...;
}

do {
    {
        next if ( $n % 2 );
        print $n, "\n";
    }
} while ( $n++ < 10 );

threads->create(
    sub {
        my (%hash3);
        share(%hash3);
        $hash2{hash}     = \%hash3;
        $hash3{"thread"} = "yes";
    }
)->join();
#11...........
        },

        'wnxl.wnxl1' => {
            source => "wnxl",
            params => "wnxl1",
            expect => <<'#12...........',
if ( $PLATFORM eq 'aix' ) {
    skip_symbols(
        [
            qw(
              Perl_dump_fds
              Perl_ErrorNo
              Perl_GetVars
              PL_sys_intern
            )
        ]
    );
}

if ( _add_fqdn_host(
    name => ...,
    fqdn => ...
) )
{
    ...;
}

do {
    {
        next if ( $n % 2 );
        print $n, "\n";
    }
} while ( $n++ < 10 );

threads->create(
    sub {
        my (%hash3);
        share(%hash3);
        $hash2{hash}     = \%hash3;
        $hash3{"thread"} = "yes";
    }
)->join();
#12...........
        },

        'wnxl.wnxl2' => {
            source => "wnxl",
            params => "wnxl2",
            expect => <<'#13...........',
if ( $PLATFORM eq 'aix' ) {
    skip_symbols( [ qw(
        Perl_dump_fds
        Perl_ErrorNo
        Perl_GetVars
        PL_sys_intern
    ) ] );
}

if ( _add_fqdn_host(
    name => ...,
    fqdn => ...
) )
{
    ...;
}

do { {
    next if ( $n % 2 );
    print $n, "\n";
} } while ( $n++ < 10 );

threads->create( sub {
    my (%hash3);
    share(%hash3);
    $hash2{hash}     = \%hash3;
    $hash3{"thread"} = "yes";
} )->join();
#13...........
        },

        'wnxl.wnxl3' => {
            source => "wnxl",
            params => "wnxl3",
            expect => <<'#14...........',
if ( $PLATFORM eq 'aix' ) {
    skip_symbols( [ qw(
        Perl_dump_fds
        Perl_ErrorNo
        Perl_GetVars
        PL_sys_intern
    ) ] );
}

if ( _add_fqdn_host(
    name => ...,
    fqdn => ...
) )
{
    ...;
}

do {
    {
        next if ( $n % 2 );
        print $n, "\n";
    }
} while ( $n++ < 10 );

threads->create( sub {
    my (%hash3);
    share(%hash3);
    $hash2{hash}     = \%hash3;
    $hash3{"thread"} = "yes";
} )->join();
#14...........
        },

        'wnxl.wnxl4' => {
            source => "wnxl",
            params => "wnxl4",
            expect => <<'#15...........',
if ( $PLATFORM eq 'aix' ) {
    skip_symbols(
        [
            qw(
              Perl_dump_fds
              Perl_ErrorNo
              Perl_GetVars
              PL_sys_intern
            )
        ]
    );
}

if ( _add_fqdn_host(
    name => ...,
    fqdn => ...
) )
{
    ...;
}

do {
    {
        next if ( $n % 2 );
        print $n, "\n";
    }
} while ( $n++ < 10 );

threads->create( sub {
    my (%hash3);
    share(%hash3);
    $hash2{hash}     = \%hash3;
    $hash3{"thread"} = "yes";
} )->join();
#15...........
        },

        'align34.def' => {
            source => "align34",
            params => "def",
            expect => <<'#16...........',
# align all '{' and runs of '='
if    ( $line =~ /^NAME>(.*)/i )       { $Cookies{'name'} = $1; }
elsif ( $line =~ /^EMAIL>(.*)/i )      { $email           = $1; }
elsif ( $line =~ /^IP_ADDRESS>(.*)/i ) { $ipaddress       = $1; }
elsif ( $line =~ /^<!--(.*)-->/i )     { $remoteuser      = $1; }
elsif ( $line =~ /^PASSWORD>(.*)/i )   { next; }
elsif ( $line =~ /^IMAGE>(.*)/i )      { $image_url = $1; }
elsif ( $line =~ /^LINKNAME>(.*)/i )   { $linkname  = $1; }
elsif ( $line =~ /^LINKURL>(.*)/i )    { $linkurl   = $1; }
else                                   { $body .= $line; }
#16...........
        },

        'git47.def' => {
            source => "git47",
            params => "def",
            expect => <<'#17...........',
# cannot weld here
$promises[$i]->then(
    sub { $all->resolve(@_); () },
    sub {
        $results->[$i] = [@_];
        $all->reject(@$results) if --$remaining <= 0;
        return ();
    }
);

sub _absolutize {
    [ map { _is_scoped($_) ? $_ : [ [ [ 'pc', 'scope' ] ], ' ', @$_ ] }
          @{ shift() } ]
}

$c->helpers->log->debug(
    sub {
        my $req    = $c->req;
        my $method = $req->method;
        my $path   = $req->url->path->to_abs_string;
        $c->helpers->timing->begin('mojo.timer');
        return qq{$method "$path"};
    }
) unless $stash->{'mojo.static'};

# A single signature var can weld
return Mojo::Promise->resolve($query_params)->then(&_reveal_event)->then(
    sub ($code) {
        return $c->render( text => '', status => $code );
    }
);
#17...........
        },

        'git47.git47' => {
            source => "git47",
            params => "git47",
            expect => <<'#18...........',
# cannot weld here
$promises[$i]->then(
  sub { $all->resolve(@_); () },
  sub {
    $results->[$i] = [@_];
    $all->reject(@$results) if --$remaining <= 0;
    return ();
  }
);

sub _absolutize { [map { _is_scoped($_) ? $_ : [[['pc', 'scope']], ' ', @$_] } @{shift()}] }

$c->helpers->log->debug(sub {
  my $req    = $c->req;
  my $method = $req->method;
  my $path   = $req->url->path->to_abs_string;
  $c->helpers->timing->begin('mojo.timer');
  return qq{$method "$path"};
}) unless $stash->{'mojo.static'};

# A single signature var can weld
return Mojo::Promise->resolve($query_params)->then(&_reveal_event)->then(sub ($code) {
  return $c->render(text => '', status => $code);
});
#18...........
        },

        'qw.def' => {
            source => "qw",
            params => "def",
            expect => <<'#19...........',
    # do not outdent ending ) more than initial qw line
    if ( $pos == 0 ) {
        @return = grep( /^$word/,
            sort qw(
              ! a b d h i m o q r u autobundle clean
              make test install force reload look
            ) );
    }

    # outdent ')' even if opening is not '('
    @EXPORT = (
        qw)
          i Re Im rho theta arg
          sqrt log ln
          log10 logn cbrt root
          cplx cplxe
        ),
        @trig
    );

    # outdent '>' like ')'
    @EXPORT = (
        qw<
          i Re Im rho theta arg
          sqrt log ln
          log10 logn cbrt root
          cplx cplxe
        >,
        @trig
    );

    # but ';' not outdented
    @EXPORT = (
        qw;
          i Re Im rho theta arg
          sqrt log ln
          log10 logn cbrt root
          cplx cplxe
          ;,
        @trig
    );
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

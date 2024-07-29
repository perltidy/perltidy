# Created with: ./make_t.pl

# Contents:
#1 wn7.wn
#2 wn8.def
#3 wn8.wn
#4 comments.comments5
#5 braces.braces1
#6 braces.braces2
#7 braces.braces3
#8 braces.def
#9 csc.csc1
#10 csc.csc2
#11 csc.def
#12 iob.def
#13 iob.iob
#14 kis.def
#15 kis.kis
#16 maths.def
#17 maths.maths1
#18 maths.maths2
#19 misc_tests.def

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
        'braces1'   => "-bl -asbl",
        'braces2'   => "-sbl",
        'braces3'   => "-bli -bbvt=1",
        'comments5' => <<'----------',
# testing --delete-side-comments and --nostatic-block-comments
-dsc -nsbc
----------
        'csc1'   => "-csc -csci=2 -ncscb -cscxl=asub",
        'csc2'   => "-dcsc",
        'def'    => "",
        'iob'    => "-iob",
        'kis'    => "-kis",
        'maths1' => <<'----------',
# testing -break-before-all-operators and no spaces around math operators
-bbao -nwls="= + - / *"  -nwrs="= + - / *"
----------
        'maths2' => <<'----------',
# testing -break-after-all-operators and no spaces around math operators
-baao -nwls="= + - / *"  -nwrs="= + - / *"
----------
        'wn' => "-wn",
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

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

        'comments' => <<'----------',
#!/usr/bin/perl -w
# an initial hash bang line cannot be deleted with -dp
#<<< format skipping of first code can cause an error message in perltidy v20210625
my $rvar = [ [ 1, 2, 3 ], [ 4, 5, 6 ] ];
#>>>
sub length { return length($_[0]) }    # side comment
                             # hanging side comment
                             # very longgggggggggggggggggggggggggggggggggggggggggggggggggggg hanging side comment

# a blank will be inserted to prevent forming a hanging side comment
sub macro_get_names { #
# 
# %name = macro_get_names();  (key=macrohandle, value=macroname)
#
##local(%name);  # a static block comment without indentation
   local(%name)=();  ## a static side comment to test -ssc

 # a spaced block comment to test -isbc
   for (0..$#mac_ver) {
      # a very long comment for testing the parameter --nooutdent-long-comments (or -nolc)
      $name{$_} = $mac_ext[$idx{$mac_exti[$_]}];
      $vmsfile =~ s/;[\d\-]*$//; # very long side comment; Clip off version number; we can use a newer version as well

   }
   %name;
} 



    @month_of_year = ( 
        'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct',
    ##  'Dec', 'Nov'   [a static block comment with indentation]
        'Nov', 'Dec');


{    # this side comment will not align
    my $IGNORE = 0;    # This is a side comment
                       # This is a hanging side comment
                       # And so is this

    # A blank line interrupts the hsc's; this is a block comment

}

# side comments at different indentation levels should not normally be aligned
{ { { { { ${msg} = "Hello World!"; print "My message: ${msg}\n"; } } #end level 4
        } # end level 3
    } # end level 2
} # end level 1


#<<<  do not let perltidy touch this unless -nfs is set
    my @list = (1,
                1, 1,
                1, 2, 1,
                1, 3, 3, 1,
                1, 4, 6, 4, 1,);
#>>>

#<<  test alternate format skipping string
    my @list = (1,
                1, 1,
                1, 2, 1,
                1, 3, 3, 1,
                1, 4, 6, 4, 1,);
#>>

    local $Test::Builder::Level = $Test::Builder::Level + 1; ## no critic (Variables::ProhibitPackageVars)

    ## no critic (ValuesAndExpressions::RequireInterpolationOfMetachars)



# some blank lines follow



=pod
Some pod before __END__ to delete with -dp
=cut


__END__


# text following __END__, not a comment


=pod
Some pod after __END__ to delete with -dp and trim with -trp     
=cut


----------

        'csc' => <<'----------',
        sub message {
            if ( !defined( $_[0] ) ) {
                print("Hello, World\n");
            }
            else {
                print( $_[0], "\n" );
            }
        } ## end sub message

        my $message =sub {
            if ( !defined( $_[0] ) ) {
                print("Hello, World\n");
            }
            else {
                print( $_[0], "\n" );
            }
        };
----------

        'iob' => <<'----------',
return "this is a descriptive error message"
  if $res->is_error
  or not length $data;
----------

        'kis' => <<'----------',
    dbmclose(%verb_delim); undef %verb_delim;
    dbmclose(%expanded); undef %expanded;
----------

        'maths' => <<'----------',
$tmp = $day - 32075 + 1461 * ( $year + 4800 - ( 14 - $month ) / 12 ) / 4 + 367
* ( $month - 2 + ( ( 14 - $month ) / 12 ) * 12 ) / 12 - 3 * ( ( $year + 4900 -
( 14 - $month ) / 12 ) / 100 ) / 4;

return ( $r**$n ) * ( pi**( $n / 2 ) ) / ( sqrt(pi) * factorial( 2 * ( int( $n
/ 2 ) ) + 2 ) / factorial( int( $n / 2 ) + 1 ) / ( 4**( int( $n / 2 ) + 1 ) )
);

$root=-$b+sqrt($b*$b-4.*$a*$c)/(2.*$a);
----------

        'misc_tests' => <<'----------',
for ( @a = @$ap, $u = shift @a; @a; $u = $v ) { ... } # test -sfs 
$i = 1 ;     #  test -sts
$i = 0;    ##  =1;  test -ssc
;;;; # test -ndsm
my ( $a, $b, $c ) = @_;    # test -nsak="my for"
----------

        'wn7' => <<'----------',
                    # do not weld paren to opening one-line non-paren container
                    $Self->_Add($SortOrderDisplay{$Field->GenerateFieldForSelectSQL()});

                    # this will not get welded with -wn
                    f(
                      do { 1; !!(my $x = bless []); }
                    );
----------

        'wn8' => <<'----------',
	    # Former -wn blinkers, which oscillated between two states

	    # fixed RULE 1 only applies to '('
            my $res = eval { { $die_on_fetch, 0 } };

            my $res = eval {
                { $die_on_fetch, 0 }
            };

	    # fixed RULE 2 applies to any inner opening token; this is a stable
	    # state with -wn
            $app->FORM->{'appbar1'}->set_status(
                _("Cannot delete zone $name: sub-zones or appellations exist.")
            );

	    # OLD: fixed RULE 1: this is now a stable state with -wn
	    # NEW (30 jan 2021): do not weld if one interior token
            $app->FORM->{'appbar1'}->set_status(_(
                 "Cannot delete zone $name: sub-zones or appellations exist."));
----------
    };

    ####################################
    # BEGIN SECTION 3: Expected output #
    ####################################
    $rtests = {

        'wn7.wn' => {
            source => "wn7",
            params => "wn",
            expect => <<'#1...........',
                    # do not weld paren to opening one-line non-paren container
                    $Self->_Add(
                        $SortOrderDisplay{
                            $Field->GenerateFieldForSelectSQL()
                        }
                    );

                    # this will not get welded with -wn
                    f(
                        do { 1; !!( my $x = bless [] ); }
                    );
#1...........
        },

        'wn8.def' => {
            source => "wn8",
            params => "def",
            expect => <<'#2...........',
            # Former -wn blinkers, which oscillated between two states

            # fixed RULE 1 only applies to '('
            my $res = eval {
                { $die_on_fetch, 0 }
            };

            my $res = eval {
                { $die_on_fetch, 0 }
            };

            # fixed RULE 2 applies to any inner opening token; this is a stable
            # state with -wn
            $app->FORM->{'appbar1'}->set_status(
                _("Cannot delete zone $name: sub-zones or appellations exist.")
            );

            # OLD: fixed RULE 1: this is now a stable state with -wn
            # NEW (30 jan 2021): do not weld if one interior token
            $app->FORM->{'appbar1'}->set_status(
                _("Cannot delete zone $name: sub-zones or appellations exist.")
            );
#2...........
        },

        'wn8.wn' => {
            source => "wn8",
            params => "wn",
            expect => <<'#3...........',
            # Former -wn blinkers, which oscillated between two states

            # fixed RULE 1 only applies to '('
            my $res = eval { { $die_on_fetch, 0 } };

            my $res = eval {
                { $die_on_fetch, 0 }
            };

            # fixed RULE 2 applies to any inner opening token; this is a stable
            # state with -wn
            $app->FORM->{'appbar1'}->set_status(
                _("Cannot delete zone $name: sub-zones or appellations exist.")
            );

            # OLD: fixed RULE 1: this is now a stable state with -wn
            # NEW (30 jan 2021): do not weld if one interior token
            $app->FORM->{'appbar1'}->set_status(
                _("Cannot delete zone $name: sub-zones or appellations exist.")
            );
#3...........
        },

        'comments.comments5' => {
            source => "comments",
            params => "comments5",
            expect => <<'#4...........',
#!/usr/bin/perl -w
# an initial hash bang line cannot be deleted with -dp
#<<< format skipping of first code can cause an error message in perltidy v20210625
my $rvar = [ [ 1, 2, 3 ], [ 4, 5, 6 ] ];
#>>>
sub length { return length( $_[0] ) }

# a blank will be inserted to prevent forming a hanging side comment
sub macro_get_names {
    #
    # %name = macro_get_names();  (key=macrohandle, value=macroname)
    #
    ##local(%name);  # a static block comment without indentation
    local (%name) = ();

    # a spaced block comment to test -isbc
    for ( 0 .. $#mac_ver ) {

# a very long comment for testing the parameter --nooutdent-long-comments (or -nolc)
        $name{$_} = $mac_ext[ $idx{ $mac_exti[$_] } ];
        $vmsfile =~ s/;[\d\-]*$//;

    }
    %name;
}

@month_of_year = (
    'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct',

    ##  'Dec', 'Nov'   [a static block comment with indentation]
    'Nov', 'Dec'
);

{
    my $IGNORE = 0;

    # A blank line interrupts the hsc's; this is a block comment

}

# side comments at different indentation levels should not normally be aligned
{
    {
        {
            {
                { ${msg} = "Hello World!"; print "My message: ${msg}\n"; }
            }
        }
    }
}

#<<<  do not let perltidy touch this unless -nfs is set
    my @list = (1,
                1, 1,
                1, 2, 1,
                1, 3, 3, 1,
                1, 4, 6, 4, 1,);
#>>>

#<<  test alternate format skipping string
my @list = ( 1, 1, 1, 1, 2, 1, 1, 3, 3, 1, 1, 4, 6, 4, 1, );

#>>

local $Test::Builder::Level = $Test::Builder::Level + 1;

## no critic (ValuesAndExpressions::RequireInterpolationOfMetachars)

# some blank lines follow

=pod
Some pod before __END__ to delete with -dp
=cut

__END__


# text following __END__, not a comment


=pod
Some pod after __END__ to delete with -dp and trim with -trp     
=cut


#4...........
        },

        'braces.braces1' => {
            source => "braces",
            params => "braces1",
            expect => <<'#5...........',
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
#5...........
        },

        'braces.braces2' => {
            source => "braces",
            params => "braces2",
            expect => <<'#6...........',
sub message
{
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
#6...........
        },

        'braces.braces3' => {
            source => "braces",
            params => "braces3",
            expect => <<'#7...........',
sub message
  { if ( !defined( $_[0] ) )
      { print("Hello, World\n");
      }
    else
      { print( $_[0], "\n" );
      }
  }

$myfun = sub {
    print("Hello, World\n");
};

eval {
    my $app = App::perlbrew->new( "install-patchperl", "-q" );
    $app->run();
} or do
  { $error          = $@;
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

$r = do
  { sswitch( $words[ rand @words ] ) {
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

        'braces.def' => {
            source => "braces",
            params => "def",
            expect => <<'#8...........',
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
#8...........
        },

        'csc.csc1' => {
            source => "csc",
            params => "csc1",
            expect => <<'#9...........',
        sub message {
            if ( !defined( $_[0] ) ) {
                print("Hello, World\n");
            } ## end if ( !defined( $_[0] ))
            else {
                print( $_[0], "\n" );
            } ## end else [ if ( !defined( $_[0] ))
        } ## end sub message

        my $message = sub {
            if ( !defined( $_[0] ) ) {
                print("Hello, World\n");
            } ## end if ( !defined( $_[0] ))
            else {
                print( $_[0], "\n" );
            } ## end else [ if ( !defined( $_[0] ))
        };
#9...........
        },

        'csc.csc2' => {
            source => "csc",
            params => "csc2",
            expect => <<'#10...........',
        sub message {
            if ( !defined( $_[0] ) ) {
                print("Hello, World\n");
            }
            else {
                print( $_[0], "\n" );
            }
        }

        my $message = sub {
            if ( !defined( $_[0] ) ) {
                print("Hello, World\n");
            }
            else {
                print( $_[0], "\n" );
            }
        };
#10...........
        },

        'csc.def' => {
            source => "csc",
            params => "def",
            expect => <<'#11...........',
        sub message {
            if ( !defined( $_[0] ) ) {
                print("Hello, World\n");
            }
            else {
                print( $_[0], "\n" );
            }
        } ## end sub message

        my $message = sub {
            if ( !defined( $_[0] ) ) {
                print("Hello, World\n");
            }
            else {
                print( $_[0], "\n" );
            }
        };
#11...........
        },

        'iob.def' => {
            source => "iob",
            params => "def",
            expect => <<'#12...........',
return "this is a descriptive error message"
  if $res->is_error
  or not length $data;
#12...........
        },

        'iob.iob' => {
            source => "iob",
            params => "iob",
            expect => <<'#13...........',
return "this is a descriptive error message"
  if $res->is_error or not length $data;
#13...........
        },

        'kis.def' => {
            source => "kis",
            params => "def",
            expect => <<'#14...........',
    dbmclose(%verb_delim);
    undef %verb_delim;
    dbmclose(%expanded);
    undef %expanded;
#14...........
        },

        'kis.kis' => {
            source => "kis",
            params => "kis",
            expect => <<'#15...........',
    dbmclose(%verb_delim); undef %verb_delim;
    dbmclose(%expanded);   undef %expanded;
#15...........
        },

        'maths.def' => {
            source => "maths",
            params => "def",
            expect => <<'#16...........',
$tmp =
  $day - 32075 +
  1461 * ( $year + 4800 - ( 14 - $month ) / 12 ) / 4 +
  367 * ( $month - 2 + ( ( 14 - $month ) / 12 ) * 12 ) / 12 -
  3 * ( ( $year + 4900 - ( 14 - $month ) / 12 ) / 100 ) / 4;

return ( $r**$n ) *
  ( pi**( $n / 2 ) ) /
  (
    sqrt(pi) *
      factorial( 2 * ( int( $n / 2 ) ) + 2 ) /
      factorial( int( $n / 2 ) + 1 ) /
      ( 4**( int( $n / 2 ) + 1 ) ) );

$root = -$b + sqrt( $b * $b - 4. * $a * $c ) / ( 2. * $a );
#16...........
        },

        'maths.maths1' => {
            source => "maths",
            params => "maths1",
            expect => <<'#17...........',
$tmp
  =$day-32075
  +1461*( $year+4800-( 14-$month )/12 )/4
  +367*( $month-2+( ( 14-$month )/12 )*12 )/12
  -3*( ( $year+4900-( 14-$month )/12 )/100 )/4;

return ( $r**$n )
  *( pi**( $n/2 ) )
  /(
    sqrt(pi)
      *factorial( 2*( int( $n/2 ) )+2 )
      /factorial( int( $n/2 )+1 )
      /( 4**( int( $n/2 )+1 ) ) );

$root=-$b+sqrt( $b*$b-4.*$a*$c )/( 2.*$a );
#17...........
        },

        'maths.maths2' => {
            source => "maths",
            params => "maths2",
            expect => <<'#18...........',
$tmp=
  $day-32075+
  1461*( $year+4800-( 14-$month )/12 )/4+
  367*( $month-2+( ( 14-$month )/12 )*12 )/12-
  3*( ( $year+4900-( 14-$month )/12 )/100 )/4;

return ( $r**$n )*
  ( pi**( $n/2 ) )/
  (
    sqrt(pi)*
      factorial( 2*( int( $n/2 ) )+2 )/
      factorial( int( $n/2 )+1 )/
      ( 4**( int( $n/2 )+1 ) ) );

$root=-$b+sqrt( $b*$b-4.*$a*$c )/( 2.*$a );
#18...........
        },

        'misc_tests.def' => {
            source => "misc_tests",
            params => "def",
            expect => <<'#19...........',
for ( @a = @$ap, $u = shift @a ; @a ; $u = $v ) { ... }    # test -sfs
$i = 1;                                                    #  test -sts
$i = 0;                                                    ##  =1;  test -ssc
;                                                          # test -ndsm
my ( $a, $b, $c ) = @_;                                    # test -nsak="my for"
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

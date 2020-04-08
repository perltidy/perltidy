# Created with: ./make_t.pl

# Contents:
#1 rt132059.def
#2 rt132059.rt132059
#3 signature.def
#4 rperl.def
#5 rperl.rperl
#6 wn7.def
#7 wn7.wn
#8 wn8.def
#9 wn8.wn
#10 pbp6.def
#11 pbp6.pbp
#12 bos.bos
#13 bos.def

# To locate test #13 you can search for its name or the string '#13'

use strict;
use Test;
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
        'bos'      => "-bos",
        'def'      => "",
        'pbp'      => "-pbp -nst -nse",
        'rperl'    => "-l=0",
        'rt132059' => "-dac",
        'wn'       => "-wn",
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

        'bos' => <<'----------',
        $top_label->set_text( gettext("check permissions.") )
          ;
----------

        'pbp6' => <<'----------',
	# These formerly blinked with -pbp
        return $width1*$common_length*(
          $W*atan2(1,$W)
        + $H*atan2(1,$H)
        - $RTHSQPWSQ*atan2(1,$RTHSQPWSQ)
        + 0.25*log(
         ($WSQP1*$HSQP1)/(1+$WSQ+$HSQ)
         *($WSQ*(1+$WSQ+$HSQ)/($WSQP1*$HSQPWSQ))**$WSQ
         *($HSQ*(1+$WSQ+$HSQ)/($HSQP1*$HSQPWSQ))**$HSQ
         )
         )/($W*$pi);

        my $oldSec = ( 60 * $session->{originalStartHour} + $session->{originalStartMin} ) * 60;

----------

        'rperl' => <<'----------',
# Some test cases for RPerl, https://github.com/wbraswell/rperl/
# These must not remain as single lines with default formatting and long lines
sub multiply_return_F { { my number $RETURN_TYPE }; ( my integer $multiplicand, my number $multiplier ) = @ARG; return $multiplicand * $multiplier; }

sub empty_method { { my void::method $RETURN_TYPE }; return 2; }

sub foo_subroutine_in_main { { my void $RETURN_TYPE }; print 'Howdy from foo_subroutine_in_main()...', "\n"; return; }
----------

        'rt132059' => <<'----------',
# Test deleting comments and pod
$1=2;
sub f { # a side comment
 # a hanging side comment

# a block comment
}

=pod
bonjour!
=cut

$i++;
----------

        'signature' => <<'----------',
# git22: Preserve function signature on a single line
# This behavior is controlled by 'sub weld_signature_parens'

sub foo($x, $y="abcd") {
  $x.$y;
}

# do not break after closing do brace
sub foo($x, $y=do{{}}, $z=42, $w=do{"abcd"}) {
  $x.$y.$z;
}

# This signature should get put back on one line
sub t022 (
    $p = do { $z += 10; 222 }, $a = do { $z++; 333 }
) { "$p/$a" }

# anonymous sub with signature
my $subref = sub ( $cat, $id = do { state $auto_id = 0; $auto_id++ } ) {
    ...;
};
----------

        'wn7' => <<'----------',
		    # do not weld paren to opening one-line non-paren container
                    $Self->_Add($SortOrderDisplay{$Field->GenerateFieldForSelectSQL()});

		    # this weld is now okay with -wn
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

	    # fixed RULE 1: this is now a stable state with -wn
            $app->FORM->{'appbar1'}->set_status(_(
                 "Cannot delete zone $name: sub-zones or appellations exist."));
----------
    };

    ####################################
    # BEGIN SECTION 3: Expected output #
    ####################################
    $rtests = {

        'rt132059.def' => {
            source => "rt132059",
            params => "def",
            expect => <<'#1...........',
# Test deleting comments and pod
$1 = 2;

sub f {    # a side comment
           # a hanging side comment

    # a block comment
}

=pod
bonjour!
=cut

$i++;
#1...........
        },

        'rt132059.rt132059' => {
            source => "rt132059",
            params => "rt132059",
            expect => <<'#2...........',
$1 = 2;

sub f {
    
}


$i++;
#2...........
        },

        'signature.def' => {
            source => "signature",
            params => "def",
            expect => <<'#3...........',
# git22: Preserve function signature on a single line
# This behavior is controlled by 'sub weld_signature_parens'

sub foo ( $x, $y = "abcd" ) {
    $x . $y;
}

# do not break after closing do brace
sub foo ( $x, $y = do { {} }, $z = 42, $w = do { "abcd" } ) {
    $x . $y . $z;
}

# This signature should get put back on one line
sub t022 ( $p = do { $z += 10; 222 }, $a = do { $z++; 333 } ) {
    "$p/$a";
}

# anonymous sub with signature
my $subref = sub ( $cat, $id = do { state $auto_id = 0; $auto_id++ } ) {
    ...;
};
#3...........
        },

        'rperl.def' => {
            source => "rperl",
            params => "def",
            expect => <<'#4...........',
# Some test cases for RPerl, https://github.com/wbraswell/rperl/
# These must not remain as single lines with default formatting and long lines
sub multiply_return_F {
    { my number $RETURN_TYPE };
    ( my integer $multiplicand, my number $multiplier ) = @ARG;
    return $multiplicand * $multiplier;
}

sub empty_method {
    { my void::method $RETURN_TYPE };
    return 2;
}

sub foo_subroutine_in_main {
    { my void $RETURN_TYPE };
    print 'Howdy from foo_subroutine_in_main()...', "\n";
    return;
}
#4...........
        },

        'rperl.rperl' => {
            source => "rperl",
            params => "rperl",
            expect => <<'#5...........',
# Some test cases for RPerl, https://github.com/wbraswell/rperl/
# These must not remain as single lines with default formatting and long lines
sub multiply_return_F {
    { my number $RETURN_TYPE };
    ( my integer $multiplicand, my number $multiplier ) = @ARG;
    return $multiplicand * $multiplier;
}

sub empty_method {
    { my void::method $RETURN_TYPE };
    return 2;
}

sub foo_subroutine_in_main {
    { my void $RETURN_TYPE };
    print 'Howdy from foo_subroutine_in_main()...', "\n";
    return;
}
#5...........
        },

        'wn7.def' => {
            source => "wn7",
            params => "def",
            expect => <<'#6...........',
                    # do not weld paren to opening one-line non-paren container
                    $Self->_Add(
                        $SortOrderDisplay{ $Field->GenerateFieldForSelectSQL() }
                    );

                    # this weld is now okay with -wn
                    f( do { 1; !!( my $x = bless [] ); } );
#6...........
        },

        'wn7.wn' => {
            source => "wn7",
            params => "wn",
            expect => <<'#7...........',
                    # do not weld paren to opening one-line non-paren container
                    $Self->_Add(
                        $SortOrderDisplay{ $Field->GenerateFieldForSelectSQL() }
                    );

                    # this weld is now okay with -wn
                    f( do { 1; !!( my $x = bless [] ); } );
#7...........
        },

        'wn8.def' => {
            source => "wn8",
            params => "def",
            expect => <<'#8...........',
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

            # fixed RULE 1: this is now a stable state with -wn
            $app->FORM->{'appbar1'}->set_status(
                _("Cannot delete zone $name: sub-zones or appellations exist.")
            );
#8...........
        },

        'wn8.wn' => {
            source => "wn8",
            params => "wn",
            expect => <<'#9...........',
            # Former -wn blinkers, which oscillated between two states

            # fixed RULE 1 only applies to '('
            my $res = eval { { $die_on_fetch, 0 } };

            my $res = eval { { $die_on_fetch, 0 } };

            # fixed RULE 2 applies to any inner opening token; this is a stable
            # state with -wn
            $app->FORM->{'appbar1'}->set_status(
                _("Cannot delete zone $name: sub-zones or appellations exist.")
            );

            # fixed RULE 1: this is now a stable state with -wn
            $app->FORM->{'appbar1'}->set_status( _(
                "Cannot delete zone $name: sub-zones or appellations exist.") );
#9...........
        },

        'pbp6.def' => {
            source => "pbp6",
            params => "def",
            expect => <<'#10...........',
        # These formerly blinked with -pbp
        return $width1 *
          $common_length *
          (
            $W * atan2( 1, $W ) +
              $H * atan2( 1, $H ) -
              $RTHSQPWSQ * atan2( 1, $RTHSQPWSQ ) +
              0.25 * log(
                ( $WSQP1 * $HSQP1 ) /
                  ( 1 + $WSQ + $HSQ ) *
                  ( $WSQ * ( 1 + $WSQ + $HSQ ) / ( $WSQP1 * $HSQPWSQ ) )
                  **$WSQ *
                  ( $HSQ * ( 1 + $WSQ + $HSQ ) / ( $HSQP1 * $HSQPWSQ ) )**$HSQ
              )
          ) /
          ( $W * $pi );

        my $oldSec =
          ( 60 * $session->{originalStartHour} + $session->{originalStartMin} )
          * 60;

#10...........
        },

        'pbp6.pbp' => {
            source => "pbp6",
            params => "pbp",
            expect => <<'#11...........',
        # These formerly blinked with -pbp
        return
            $width1 * $common_length
            * (
                  $W * atan2( 1, $W )
                + $H * atan2( 1, $H )
                - $RTHSQPWSQ * atan2( 1, $RTHSQPWSQ )
                + 0.25 * log(
                  ( $WSQP1 * $HSQP1 )
                / ( 1 + $WSQ + $HSQ )
                    * ( $WSQ * ( 1 + $WSQ + $HSQ ) / ( $WSQP1 * $HSQPWSQ ) )
                    **$WSQ
                    * ( $HSQ * ( 1 + $WSQ + $HSQ ) / ( $HSQP1 * $HSQPWSQ ) )
                    **$HSQ
                )
            )
            / ( $W * $pi );

        my $oldSec
            = ( 60 * $session->{originalStartHour}
                + $session->{originalStartMin} )
            * 60;

#11...........
        },

        'bos.bos' => {
            source => "bos",
            params => "bos",
            expect => <<'#12...........',
        $top_label->set_text( gettext("check permissions.") )
          ;
#12...........
        },

        'bos.def' => {
            source => "bos",
            params => "def",
            expect => <<'#13...........',
        $top_label->set_text( gettext("check permissions.") );
#13...........
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
        if ($err) {
            print STDERR
"This error received calling Perl::Tidy with '$sname' + '$pname'\n";
            ok( !$err );
        }
        if ($stderr_string) {
            print STDERR "---------------------\n";
            print STDERR "<<STDERR>>\n$stderr_string\n";
            print STDERR "---------------------\n";
            print STDERR
"This error received calling Perl::Tidy with '$sname' + '$pname'\n";
            ok( !$stderr_string );
        }
        if ($errorfile_string) {
            print STDERR "---------------------\n";
            print STDERR "<<.ERR file>>\n$errorfile_string\n";
            print STDERR "---------------------\n";
            print STDERR
"This error received calling Perl::Tidy with '$sname' + '$pname'\n";
            ok( !$errorfile_string );
        }
    }
    else {
        ok( $output, $expect );
    }
}

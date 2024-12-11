# Created with: ./make_t.pl

# Contents:
#1 align32.def
#2 bos.bos
#3 bos.def
#4 comments.comments1
#5 comments.comments2
#6 comments.comments3
#7 comments.comments4
#8 comments.def
#9 long_line.def
#10 long_line.long_line
#11 pbp6.def
#12 pbp6.pbp
#13 rperl.def
#14 rperl.rperl
#15 rt132059.def
#16 rt132059.rt132059
#17 signature.def
#18 ternary4.def
#19 wn7.def

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
        'bos'       => "-bos",
        'comments1' => <<'----------',
# testing --fixed-position-side-comment=40, 
# --ignore-side-comment-lengths,
# --noindent-block-comments,
# --nohanging-side-comments
# --static-side-comments
# --trim-pod
-fpsc=40 -iscl -nibc -nhsc -ssc -trp
----------
        'comments2' => <<'----------',
# testing --minimum-space-to-comment=10, --delete-block-comments, --delete-pod
-msc=10 -dbc -dp
----------
        'comments3' => <<'----------',
--maximum-consecutive-blank-lines=2   # -mbl=2
--indent-spaced-block-comments        # -isbc
--no-format-skipping                  # -nfs
--ignore-perlcritic-comments          # -ipc
----------
        'comments4' => <<'----------',
# testing --keep-old-blank-lines=2 [=all] and 
# --nooutdent-long-comments and 
# --outdent-static-block-comments
# --format-skipping-begin and --format-skipping-end
-kbl=2 -nolc -osbc -fsb='#<{2,}' -fse='#>{2,}'
----------
        'def'       => "",
        'long_line' => "-l=0",
        'pbp'       => "-pbp -nst -nse",
        'rperl'     =>
          "-pbp  -nst --ignore-side-comment-lengths  --converge  -l=0  -q",
        'rt132059' => "-dac",
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

        'align32' => <<'----------',
# align just the last two lines
my $c_sub_khwnd = WindowFromId $k_hwnd, 0x8008;    # FID_CLIENT
ok $c_sub_khwnd, 'have kids client window';
ok IsWindow($c_sub_khwnd), 'IsWindow works on the client';

# parenless calls
mkTextConfig $c, $x, $y, -anchor => 'se', $color;
mkTextConfig $c, $x + 30, $y, -anchor => 's',  $color;
mkTextConfig $c, $x + 60, $y, -anchor => 'sw', $color;
mkTextConfig $c, $x, $y + 30, -anchor => 'e', $color;

permute_test [ 'a', 'b', 'c' ],   '/', '/', [ 'a', 'b', 'c' ];
permute_test [ 'a,', 'b', 'c,' ], '/', '/', [ 'a,', 'b', 'c,' ];
permute_test [ 'a', ',', '#', 'c' ], '/', '/', [ 'a', ',', '#', 'c' ];
permute_test [ 'f_oo', 'b_ar' ], '/', '/', [ 'f_oo', 'b_ar' ];

# issue c093 - broken sub, but align fat commas
use constant UNDEF_ONLY => sub { not defined $_[0] };
use constant EMPTY_OR_UNDEF => sub {
    !@_ or @_ == 1 && !defined $_[0];
};
----------

        'bos' => <<'----------',
        $top_label->set_text( gettext("check permissions.") )
          ;
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

        'long_line' => <<'----------',
# This single line should break into multiple lines, even with -l=0
# sub 'tight_paren_follows' should break the do block
$body = SOAP::Data->name('~V:Fault')->attr( { 'xmlns' => $SOAP::Constants::NS_ENV } )->value( \SOAP::Data->set_value( SOAP::Data->name( faultcode => qualify( $self->namespace => shift(@parameters) ) ), SOAP::Data->name( faultstring => shift(@parameters) ), @parameters ? SOAP::Data->name( detail => do { my $detail = shift(@parameters); ref $detail ? \$detail : $detail } ) : (), @parameters ? SOAP::Data->name( faultactor => shift(@parameters) ) : (), ) );
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

# signature and prototype and attribute
sub foo1 ( $x, $y ) : prototype ( $$ ) : shared { }

sub foo11 ( $thing, % ) { print $thing }

sub animal4 ( $cat, $ = ) {   } # second argument is optional

*share = sub 
( \[$@%] ) { };

# extruded test
sub foo2
  (
  $
  first
  ,
  $
  ,
  $
  third
  )
  {
  return
  "first=$first, third=$third"
  ;
  }

# valid attributes
sub fnord (&\%) : switch(10,foo(7,3)) : expensive;
sub plugh () : Ugly('\(") : Bad;
----------

        'ternary4' => <<'----------',
# some side comments
*{"${callpkg}::$sym"} = 
      $type eq '&' ? \&{"${pkg}::$sym"}    #
    : $type eq '$' ? \${"${pkg}::$sym"}    #
    : $type eq '@' ? \@{"${pkg}::$sym"}
    : $type eq '%' ? \%{"${pkg}::$sym"}    # side comment
    : $type eq '*' ? *{"${pkg}::$sym"}     #
    :   do { require Carp; Carp::croak("Can't export symbol: $type$sym") };
----------

        'wn7' => <<'----------',
                    # do not weld paren to opening one-line non-paren container
                    $Self->_Add($SortOrderDisplay{$Field->GenerateFieldForSelectSQL()});

                    # this will not get welded with -wn
                    f(
                      do { 1; !!(my $x = bless []); }
                    );
----------
    };

    ####################################
    # BEGIN SECTION 3: Expected output #
    ####################################
    $rtests = {

        'align32.def' => {
            source => "align32",
            params => "def",
            expect => <<'#1...........',
# align just the last two lines
my $c_sub_khwnd = WindowFromId $k_hwnd, 0x8008;    # FID_CLIENT
ok $c_sub_khwnd,           'have kids client window';
ok IsWindow($c_sub_khwnd), 'IsWindow works on the client';

# parenless calls
mkTextConfig $c, $x,      $y,      -anchor => 'se', $color;
mkTextConfig $c, $x + 30, $y,      -anchor => 's',  $color;
mkTextConfig $c, $x + 60, $y,      -anchor => 'sw', $color;
mkTextConfig $c, $x,      $y + 30, -anchor => 'e',  $color;

permute_test [ 'a', 'b', 'c' ],      '/', '/', [ 'a', 'b', 'c' ];
permute_test [ 'a,', 'b', 'c,' ],    '/', '/', [ 'a,', 'b', 'c,' ];
permute_test [ 'a', ',', '#', 'c' ], '/', '/', [ 'a', ',', '#', 'c' ];
permute_test [ 'f_oo', 'b_ar' ],     '/', '/', [ 'f_oo', 'b_ar' ];

# issue c093 - broken sub, but align fat commas
use constant UNDEF_ONLY     => sub { not defined $_[0] };
use constant EMPTY_OR_UNDEF => sub {
    !@_ or @_ == 1 && !defined $_[0];
};
#1...........
        },

        'bos.bos' => {
            source => "bos",
            params => "bos",
            expect => <<'#2...........',
        $top_label->set_text( gettext("check permissions.") )
          ;
#2...........
        },

        'bos.def' => {
            source => "bos",
            params => "def",
            expect => <<'#3...........',
        $top_label->set_text( gettext("check permissions.") );
#3...........
        },

        'comments.comments1' => {
            source => "comments",
            params => "comments1",
            expect => <<'#4...........',
#!/usr/bin/perl -w
# an initial hash bang line cannot be deleted with -dp
#<<< format skipping of first code can cause an error message in perltidy v20210625
my $rvar = [ [ 1, 2, 3 ], [ 4, 5, 6 ] ];
#>>>
sub length { return length( $_[0] ) }  # side comment

# hanging side comment
# very longgggggggggggggggggggggggggggggggggggggggggggggggggggg hanging side comment

# a blank will be inserted to prevent forming a hanging side comment
sub macro_get_names {                  #

#
# %name = macro_get_names();  (key=macrohandle, value=macroname)
#
##local(%name);  # a static block comment without indentation
    local (%name) = (); ## a static side comment to test -ssc

# a spaced block comment to test -isbc
    for ( 0 .. $#mac_ver ) {

# a very long comment for testing the parameter --nooutdent-long-comments (or -nolc)
        $name{$_} = $mac_ext[ $idx{ $mac_exti[$_] } ];
        $vmsfile =~ s/;[\d\-]*$//;     # very long side comment; Clip off version number; we can use a newer version as well

    }
    %name;
}

@month_of_year = (
    'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct',
##  'Dec', 'Nov'   [a static block comment with indentation]
    'Nov', 'Dec'
);

{                                      # this side comment will not align
    my $IGNORE = 0;                    # This is a side comment

# This is a hanging side comment
# And so is this

# A blank line interrupts the hsc's; this is a block comment

}

# side comments at different indentation levels should not normally be aligned
{
    {
        {
            {
                { ${msg} = "Hello World!"; print "My message: ${msg}\n"; }
            }                          #end level 4
        }                              # end level 3
    }                                  # end level 2
}                                      # end level 1

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


#4...........
        },

        'comments.comments2' => {
            source => "comments",
            params => "comments2",
            expect => <<'#5...........',
#!/usr/bin/perl -w
#<<< format skipping of first code can cause an error message in perltidy v20210625
my $rvar = [ [ 1, 2, 3 ], [ 4, 5, 6 ] ];
#>>>
sub length { return length( $_[0] ) }          # side comment
                                               # hanging side comment
 # very longgggggggggggggggggggggggggggggggggggggggggggggggggggg hanging side comment

sub macro_get_names {          #
    local (%name) = ();          ## a static side comment to test -ssc

    for ( 0 .. $#mac_ver ) {
        $name{$_} = $mac_ext[ $idx{ $mac_exti[$_] } ];
        $vmsfile =~ s/;[\d\-]*$//
          ; # very long side comment; Clip off version number; we can use a newer version as well

    }
    %name;
}

@month_of_year = (
    'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct',
    'Nov', 'Dec'
);

{          # this side comment will not align
    my $IGNORE = 0;          # This is a side comment
                             # This is a hanging side comment
                             # And so is this

}

{
    {
        {
            {
                { ${msg} = "Hello World!"; print "My message: ${msg}\n"; }
            }          #end level 4
        }          # end level 3
    }          # end level 2
}          # end level 1

#<<<  do not let perltidy touch this unless -nfs is set
    my @list = (1,
                1, 1,
                1, 2, 1,
                1, 3, 3, 1,
                1, 4, 6, 4, 1,);
#>>>

my @list = ( 1, 1, 1, 1, 2, 1, 1, 3, 3, 1, 1, 4, 6, 4, 1, );

local $Test::Builder::Level = $Test::Builder::Level + 1;          ## no critic (Variables::ProhibitPackageVars)


__END__


# text following __END__, not a comment




#5...........
        },

        'comments.comments3' => {
            source => "comments",
            params => "comments3",
            expect => <<'#6...........',
#!/usr/bin/perl -w
# an initial hash bang line cannot be deleted with -dp
#<<< format skipping of first code can cause an error message in perltidy v20210625
my $rvar = [ [ 1, 2, 3 ], [ 4, 5, 6 ] ];

#>>>
sub length { return length( $_[0] ) }    # side comment
                                         # hanging side comment
 # very longgggggggggggggggggggggggggggggggggggggggggggggggggggg hanging side comment

# a blank will be inserted to prevent forming a hanging side comment
sub macro_get_names {    #

#
# %name = macro_get_names();  (key=macrohandle, value=macroname)
#
##local(%name);  # a static block comment without indentation
    local (%name) = ();    ## a static side comment to test -ssc

    # a spaced block comment to test -isbc
    for ( 0 .. $#mac_ver ) {

# a very long comment for testing the parameter --nooutdent-long-comments (or -nolc)
        $name{$_} = $mac_ext[ $idx{ $mac_exti[$_] } ];
        $vmsfile =~ s/;[\d\-]*$//
          ; # very long side comment; Clip off version number; we can use a newer version as well

    }
    %name;
}


@month_of_year = (
    'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct',
    ##  'Dec', 'Nov'   [a static block comment with indentation]
    'Nov', 'Dec'
);


{    # this side comment will not align
    my $IGNORE = 0;    # This is a side comment
                       # This is a hanging side comment
                       # And so is this

    # A blank line interrupts the hsc's; this is a block comment

}

# side comments at different indentation levels should not normally be aligned
{
    {
        {
            {
                { ${msg} = "Hello World!"; print "My message: ${msg}\n"; }
            }    #end level 4
        }    # end level 3
    }    # end level 2
}    # end level 1


#<<<  do not let perltidy touch this unless -nfs is set
my @list = ( 1, 1, 1, 1, 2, 1, 1, 3, 3, 1, 1, 4, 6, 4, 1, );

#>>>

#<<  test alternate format skipping string
my @list = ( 1, 1, 1, 1, 2, 1, 1, 3, 3, 1, 1, 4, 6, 4, 1, );

#>>

local $Test::Builder::Level =
  $Test::Builder::Level + 1;    ## no critic (Variables::ProhibitPackageVars)

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


#6...........
        },

        'comments.comments4' => {
            source => "comments",
            params => "comments4",
            expect => <<'#7...........',
#!/usr/bin/perl -w
# an initial hash bang line cannot be deleted with -dp
#<<< format skipping of first code can cause an error message in perltidy v20210625
my $rvar = [ [ 1, 2, 3 ], [ 4, 5, 6 ] ];
#>>>
sub length { return length( $_[0] ) }    # side comment
                                         # hanging side comment
 # very longgggggggggggggggggggggggggggggggggggggggggggggggggggg hanging side comment

# a blank will be inserted to prevent forming a hanging side comment
sub macro_get_names {    #

    #
    # %name = macro_get_names();  (key=macrohandle, value=macroname)
    #
##local(%name);  # a static block comment without indentation
    local (%name) = ();    ## a static side comment to test -ssc

    # a spaced block comment to test -isbc
    for ( 0 .. $#mac_ver ) {

        # a very long comment for testing the parameter --nooutdent-long-comments (or -nolc)
        $name{$_} = $mac_ext[ $idx{ $mac_exti[$_] } ];
        $vmsfile =~ s/;[\d\-]*$//
          ; # very long side comment; Clip off version number; we can use a newer version as well

    }
    %name;
}



@month_of_year = (
    'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct',
  ##  'Dec', 'Nov'   [a static block comment with indentation]
    'Nov', 'Dec'
);


{    # this side comment will not align
    my $IGNORE = 0;    # This is a side comment
                       # This is a hanging side comment
                       # And so is this

    # A blank line interrupts the hsc's; this is a block comment

}

# side comments at different indentation levels should not normally be aligned
{
    {
        {
            {
                { ${msg} = "Hello World!"; print "My message: ${msg}\n"; }
            }    #end level 4
        }    # end level 3
    }    # end level 2
}    # end level 1


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

local $Test::Builder::Level = $Test::Builder::Level + 1;    ## no critic (Variables::ProhibitPackageVars)

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


#7...........
        },

        'comments.def' => {
            source => "comments",
            params => "def",
            expect => <<'#8...........',
#!/usr/bin/perl -w
# an initial hash bang line cannot be deleted with -dp
#<<< format skipping of first code can cause an error message in perltidy v20210625
my $rvar = [ [ 1, 2, 3 ], [ 4, 5, 6 ] ];
#>>>
sub length { return length( $_[0] ) }    # side comment
                                         # hanging side comment
 # very longgggggggggggggggggggggggggggggggggggggggggggggggggggg hanging side comment

# a blank will be inserted to prevent forming a hanging side comment
sub macro_get_names {    #

    #
    # %name = macro_get_names();  (key=macrohandle, value=macroname)
    #
##local(%name);  # a static block comment without indentation
    local (%name) = ();    ## a static side comment to test -ssc

    # a spaced block comment to test -isbc
    for ( 0 .. $#mac_ver ) {

# a very long comment for testing the parameter --nooutdent-long-comments (or -nolc)
        $name{$_} = $mac_ext[ $idx{ $mac_exti[$_] } ];
        $vmsfile =~ s/;[\d\-]*$//
          ; # very long side comment; Clip off version number; we can use a newer version as well

    }
    %name;
}

@month_of_year = (
    'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct',
    ##  'Dec', 'Nov'   [a static block comment with indentation]
    'Nov', 'Dec'
);

{    # this side comment will not align
    my $IGNORE = 0;    # This is a side comment
                       # This is a hanging side comment
                       # And so is this

    # A blank line interrupts the hsc's; this is a block comment

}

# side comments at different indentation levels should not normally be aligned
{
    {
        {
            {
                { ${msg} = "Hello World!"; print "My message: ${msg}\n"; }
            }    #end level 4
        }    # end level 3
    }    # end level 2
}    # end level 1

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

local $Test::Builder::Level = $Test::Builder::Level + 1;    ## no critic (Variables::ProhibitPackageVars)

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


#8...........
        },

        'long_line.def' => {
            source => "long_line",
            params => "def",
            expect => <<'#9...........',
# This single line should break into multiple lines, even with -l=0
# sub 'tight_paren_follows' should break the do block
$body =
  SOAP::Data->name('~V:Fault')
  ->attr( { 'xmlns' => $SOAP::Constants::NS_ENV } )
  ->value(
    \SOAP::Data->set_value(
        SOAP::Data->name(
            faultcode => qualify( $self->namespace => shift(@parameters) )
        ),
        SOAP::Data->name( faultstring => shift(@parameters) ),
        @parameters
        ? SOAP::Data->name(
            detail => do {
                my $detail = shift(@parameters);
                ref $detail ? \$detail : $detail;
            }
          )
        : (),
        @parameters ? SOAP::Data->name( faultactor => shift(@parameters) ) : (),
    )
  );
#9...........
        },

        'long_line.long_line' => {
            source => "long_line",
            params => "long_line",
            expect => <<'#10...........',
# This single line should break into multiple lines, even with -l=0
# sub 'tight_paren_follows' should break the do block
$body = SOAP::Data->name('~V:Fault')->attr( { 'xmlns' => $SOAP::Constants::NS_ENV } )->value(
    \SOAP::Data->set_value(
        SOAP::Data->name( faultcode   => qualify( $self->namespace => shift(@parameters) ) ),
        SOAP::Data->name( faultstring => shift(@parameters) ),
        @parameters
        ? SOAP::Data->name(
            detail => do { my $detail = shift(@parameters); ref $detail ? \$detail : $detail }
          )
        : (),
        @parameters ? SOAP::Data->name( faultactor => shift(@parameters) ) : (),
    )
);
#10...........
        },

        'pbp6.def' => {
            source => "pbp6",
            params => "def",
            expect => <<'#11...........',
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

#11...........
        },

        'pbp6.pbp' => {
            source => "pbp6",
            params => "pbp",
            expect => <<'#12...........',
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

#12...........
        },

        'rperl.def' => {
            source => "rperl",
            params => "def",
            expect => <<'#13...........',
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
#13...........
        },

        'rperl.rperl' => {
            source => "rperl",
            params => "rperl",
            expect => <<'#14...........',
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
#14...........
        },

        'rt132059.def' => {
            source => "rt132059",
            params => "def",
            expect => <<'#15...........',
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
#15...........
        },

        'rt132059.rt132059' => {
            source => "rt132059",
            params => "rt132059",
            expect => <<'#16...........',
$1 = 2;

sub f {

}


$i++;
#16...........
        },

        'signature.def' => {
            source => "signature",
            params => "def",
            expect => <<'#17...........',
# git22: Preserve function signature on a single line
# This behavior is controlled by 'sub weld_signature_parens'

sub foo( $x, $y = "abcd" ) {
    $x . $y;
}

# do not break after closing do brace
sub foo( $x, $y = do { {} }, $z = 42, $w = do { "abcd" } ) {
    $x . $y . $z;
}

# This signature should get put back on one line
sub t022 ( $p = do { $z += 10; 222 }, $a = do { $z++; 333 } ) { "$p/$a" }

# anonymous sub with signature
my $subref = sub ( $cat, $id = do { state $auto_id = 0; $auto_id++ } ) {
    ...;
};

# signature and prototype and attribute
sub foo1 ( $x, $y ) : prototype ( $$ ) : shared { }

sub foo11 ( $thing, % ) { print $thing }

sub animal4 ( $cat, $ = ) { }    # second argument is optional

*share = sub ( \[$@%] ) { };

# extruded test
sub foo2 ( $first, $, $third ) {
    return "first=$first, third=$third";
}

# valid attributes
sub fnord (&\%) : switch(10,foo(7,3)) : expensive;
sub plugh () : Ugly('\(") : Bad;
#17...........
        },

        'ternary4.def' => {
            source => "ternary4",
            params => "def",
            expect => <<'#18...........',
# some side comments
*{"${callpkg}::$sym"} = $type eq '&' ? \&{"${pkg}::$sym"}    #
  : $type eq '$' ? \${"${pkg}::$sym"}                        #
  : $type eq '@' ? \@{"${pkg}::$sym"}
  : $type eq '%' ? \%{"${pkg}::$sym"}                        # side comment
  : $type eq '*' ? *{"${pkg}::$sym"}                         #
  :   do { require Carp; Carp::croak("Can't export symbol: $type$sym") };
#18...........
        },

        'wn7.def' => {
            source => "wn7",
            params => "def",
            expect => <<'#19...........',
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

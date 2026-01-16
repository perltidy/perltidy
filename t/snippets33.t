# Created with: ./make_t.pl

# Contents:
#1 boct.boct
#2 boct.def
#3 dfsfs.def
#4 dfsfs.dfsfs1
#5 dfsfs.dfsfs2
#6 c226.c226
#7 c226.def
#8 comments.comments6

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
        'boct' => <<'----------',
-boc -boct='f( ;'
----------
        'c226'      => "-xci",
        'comments6' => <<'----------',
# testing --delete-side-comments and --delete-side-comments-exclusion-pattern
-dsc -dscxp='\#\#'
----------
        'def'    => "",
        'dfsfs1' => "-dsc",
        'dfsfs2' => <<'----------',
-dsc
-ndfsfs
----------
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

        'boct' => <<'----------',
my @list = (
    1,
    1, 1,
    1, 2, 1,
);

# test boct='f('
my $val = func(
    1,
    1, 1,
    1, 2, 1,
);

# uncontained commas: test -boct=';'
my $prefix = substr $exception,
      0,
      length $REQUIRED_LENGTH;
----------

        'c226' => <<'----------',
            join(
                ", ",
                map {
                    join( ": ",
                        $feat->settingName( $_->{'type'}, $_->{'setting'} ) )
                  }
                  grep { ( $_->{'enable'} & $subFeatureFlags ) != 0 }
                  @$featureEntries
            );

            return map {
                ref $_
                    ? do {
                        my $dumper = Data::Dumper->new( [$_] );
                        $dumper->Indent(1)->Terse(1);
                        $dumper->Sortkeys(1) if $dumper->can("Sortkeys");
                        $dumper->Dump;
                    }
                    : $_
            } @_;
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

        'dfsfs' => <<'----------',
# test detect-format-skipping-from-start
my @list1; # this will not be deleted with -dsc
   @list1 = (1,
             1, 1,
             1, 2, 1,
             1, 3, 3, 1,
             1, 4, 6, 4, 1,);
#>>>
my @list2; # this will be deleted with -dsc
   @list2 = (1,
             1, 1,
             1, 2, 1,
             1, 3, 3, 1,
             1, 4, 6, 4, 1,);
----------
    };

    ####################################
    # BEGIN SECTION 3: Expected output #
    ####################################
    $rtests = {

        'boct.boct' => {
            source => "boct",
            params => "boct",
            expect => <<'#1...........',
my @list = ( 1, 1, 1, 1, 2, 1, );

# test boct='f('
my $val = func(
    1,
    1, 1,
    1, 2, 1,
);

# uncontained commas: test -boct=';'
my $prefix = substr $exception,
  0,
  length $REQUIRED_LENGTH;
#1...........
        },

        'boct.def' => {
            source => "boct",
            params => "def",
            expect => <<'#2...........',
my @list = ( 1, 1, 1, 1, 2, 1, );

# test boct='f('
my $val = func( 1, 1, 1, 1, 2, 1, );

# uncontained commas: test -boct=';'
my $prefix = substr $exception, 0, length $REQUIRED_LENGTH;
#2...........
        },

        'dfsfs.def' => {
            source => "dfsfs",
            params => "def",
            expect => <<'#3...........',
# test detect-format-skipping-from-start
my @list1; # this will not be deleted with -dsc
   @list1 = (1,
             1, 1,
             1, 2, 1,
             1, 3, 3, 1,
             1, 4, 6, 4, 1,);
#>>>
my @list2;    # this will be deleted with -dsc
@list2 = ( 1, 1, 1, 1, 2, 1, 1, 3, 3, 1, 1, 4, 6, 4, 1, );
#3...........
        },

        'dfsfs.dfsfs1' => {
            source => "dfsfs",
            params => "dfsfs1",
            expect => <<'#4...........',
# test detect-format-skipping-from-start
my @list1; # this will not be deleted with -dsc
   @list1 = (1,
             1, 1,
             1, 2, 1,
             1, 3, 3, 1,
             1, 4, 6, 4, 1,);
#>>>
my @list2;
@list2 = ( 1, 1, 1, 1, 2, 1, 1, 3, 3, 1, 1, 4, 6, 4, 1, );
#4...........
        },

        'dfsfs.dfsfs2' => {
            source => "dfsfs",
            params => "dfsfs2",
            expect => <<'#5...........',
# test detect-format-skipping-from-start
my @list1;
@list1 = ( 1, 1, 1, 1, 2, 1, 1, 3, 3, 1, 1, 4, 6, 4, 1, );

#>>>
my @list2;
@list2 = ( 1, 1, 1, 1, 2, 1, 1, 3, 3, 1, 1, 4, 6, 4, 1, );
#5...........
        },

        'c226.c226' => {
            source => "c226",
            params => "c226",
            expect => <<'#6...........',
            join(
                ", ",
                map {
                    join( ": ",
                        $feat->settingName( $_->{'type'}, $_->{'setting'} ) )
                  }
                  grep { ( $_->{'enable'} & $subFeatureFlags ) != 0 }
                  @$featureEntries
            );

            return map {
                ref $_
                  ? do {
                      my $dumper = Data::Dumper->new( [$_] );
                      $dumper->Indent(1)->Terse(1);
                      $dumper->Sortkeys(1) if $dumper->can("Sortkeys");
                      $dumper->Dump;
                  }
                  : $_
            } @_;
#6...........
        },

        'c226.def' => {
            source => "c226",
            params => "def",
            expect => <<'#7...........',
            join(
                ", ",
                map {
                    join( ": ",
                        $feat->settingName( $_->{'type'}, $_->{'setting'} ) )
                  }
                  grep { ( $_->{'enable'} & $subFeatureFlags ) != 0 }
                  @$featureEntries
            );

            return map {
                ref $_
                  ? do {
                    my $dumper = Data::Dumper->new( [$_] );
                    $dumper->Indent(1)->Terse(1);
                    $dumper->Sortkeys(1) if $dumper->can("Sortkeys");
                    $dumper->Dump;
                  }
                  : $_
            } @_;
#7...........
        },

        'comments.comments6' => {
            source => "comments",
            params => "comments6",
            expect => <<'#8...........',
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
    local (%name) = ();    ## a static side comment to test -ssc

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

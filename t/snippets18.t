# Created with: ./make_t.pl

# Contents:
#1 wn7.wn
#2 wn8.def
#3 wn8.wn
#4 comments.comments5

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
        'comments5' => "-dsc",
        'def'       => "",
        'wn'        => "-wn",
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

        'comments' => <<'----------',
#!/usr/bin/perl -w
# an initial hash bang line cannot be deleted with -dp
sub length { return length($_[0]) }    # side comment
                             # hanging side comment
                             # very longgggggggggggggggggggggggggggggggggggggggggggggggggggg hanging side comment

# side comments following open brace are not currently treated as hanging side comments
sub macro_get_names { #
# 
# %name = macro_get_names();  (key=macrohandle, value=macroname)
#
##local(%name);  # a static block comment
   local(%name)=();
   for (0..$                                          #mac_ver) {
      $name{$_} = $mac_ext[$idx{$mac_exti[$_]}];
   }
   %name;
} 




# side comments at different indentation levels should not normally be aligned
{ { { { { ${msg} = "Hello World!"; print "My message: ${msg}\n"; } } #end level 4
        } # end level 3
    } # end level 2
} # end level 1



# some blank lines follow



=pod
Some pod before __END__ to delete with -dp
=cut


__END__


# text following __END__, not a comment


=pod
Some pod after  __END__ to delete with -dp
=cut


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

	    # fixed RULE 1: this is now a stable state with -wn
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
                        $SortOrderDisplay{ $Field->GenerateFieldForSelectSQL() }
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

            # fixed RULE 1: this is now a stable state with -wn
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

            my $res = eval { { $die_on_fetch, 0 } };

            # fixed RULE 2 applies to any inner opening token; this is a stable
            # state with -wn
            $app->FORM->{'appbar1'}->set_status(
                _("Cannot delete zone $name: sub-zones or appellations exist.")
            );

            # fixed RULE 1: this is now a stable state with -wn
            $app->FORM->{'appbar1'}->set_status( _(
                "Cannot delete zone $name: sub-zones or appellations exist.") );
#3...........
        },

        'comments.comments5' => {
            source => "comments",
            params => "comments5",
            expect => <<'#4...........',
#!/usr/bin/perl -w
# an initial hash bang line cannot be deleted with -dp
sub length { return length( $_[0] ) }

# side comments following open brace are not currently treated as hanging side comments
sub macro_get_names {
    #
    # %name = macro_get_names();  (key=macrohandle, value=macroname)
    #
##local(%name);  # a static block comment
    local (%name) = ();
    for ( 0 .. $#mac_ver ) {
        $name{$_} = $mac_ext[ $idx{ $mac_exti[$_] } ];
    }
    %name;
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

# some blank lines follow

=pod
Some pod before __END__ to delete with -dp
=cut

__END__


# text following __END__, not a comment


=pod
Some pod after  __END__ to delete with -dp
=cut


#4...........
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

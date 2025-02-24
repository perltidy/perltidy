# Created with: ./make_t.pl

# Contents:
#1 kblx.kblx6
#2 kblx.kblx7
#3 kblx.kblx8
#4 git181.def
#5 git181.git181
#6 git32.def

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
        'def'    => "",
        'git181' => "-nasc",
        'kblx6'  => <<'----------',
-nbbc
-blbs=0
-blbp=0
-nbbb
-blao=0
-blbc=0
-kblx='bs' # inverse of -blbs
----------
        'kblx7' => <<'----------',
-nbbc
-blbs=0
-blbp=0
-nbbb
-blao=0
-blbc=0
-kblx='bc' # inverse of -bbc
----------
        'kblx8' => <<'----------',
-nbbc
-blbs=0
-blbp=0
-nbbb
-blao=0
-blbc=0
-kblx='cb' # not an inverse
----------
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

        'git181' => <<'----------',
on develop => sub {
    requires 'Template'           => '0';
    requires 'Test::Perl::Critic' => '0';
    requires 'Test::Pod'          => '1.26'
}

# end
----------

        'git32' => <<'----------',
use Foo qw(myfunction);
sub run {
    myfunction('x');
    my $res = myfunction ? 'X' : 'Y';  print ' ?';
}
----------

        'kblx' => <<'----------',
package A::B;

sub write_line {

    my ( $self, $line ) = @_;

    if ( defined($line) ) {
        $self->write_line($line);
    }
    return;

}

package C::D;

sub dump_verbatim {

    my $self = shift;

    # block comments
    # block comments

    my $rlines = $self->[_rlines_];

    foreach my $line ( @{$rlines} ) {
        my $input_line = $line->{_line_text};
        $self->write_unindented_line($input_line);
    }

    return;

}    ## static side comment

# coment before side comment

# comment after blank and comment
----------
    };

    ####################################
    # BEGIN SECTION 3: Expected output #
    ####################################
    $rtests = {

        'kblx.kblx6' => {
            source => "kblx",
            params => "kblx6",
            expect => <<'#1...........',
package A::B;
sub write_line {

    my ( $self, $line ) = @_;

    if ( defined($line) ) {
        $self->write_line($line);
    }
    return;

}

package C::D;
sub dump_verbatim {

    my $self = shift;

    # block comments
    # block comments

    my $rlines = $self->[_rlines_];

    foreach my $line ( @{$rlines} ) {
        my $input_line = $line->{_line_text};
        $self->write_unindented_line($input_line);
    }

    return;

}    ## static side comment

# coment before side comment

# comment after blank and comment
#1...........
        },

        'kblx.kblx7' => {
            source => "kblx",
            params => "kblx7",
            expect => <<'#2...........',
package A::B;

sub write_line {

    my ( $self, $line ) = @_;

    if ( defined($line) ) {
        $self->write_line($line);
    }
    return;

}

package C::D;

sub dump_verbatim {

    my $self = shift;
    # block comments
    # block comments

    my $rlines = $self->[_rlines_];

    foreach my $line ( @{$rlines} ) {
        my $input_line = $line->{_line_text};
        $self->write_unindented_line($input_line);
    }

    return;

}    ## static side comment
# coment before side comment

# comment after blank and comment
#2...........
        },

        'kblx.kblx8' => {
            source => "kblx",
            params => "kblx8",
            expect => <<'#3...........',
package A::B;

sub write_line {

    my ( $self, $line ) = @_;

    if ( defined($line) ) {
        $self->write_line($line);
    }
    return;

}

package C::D;

sub dump_verbatim {

    my $self = shift;

    # block comments
    # block comments
    my $rlines = $self->[_rlines_];

    foreach my $line ( @{$rlines} ) {
        my $input_line = $line->{_line_text};
        $self->write_unindented_line($input_line);
    }

    return;

}    ## static side comment

# coment before side comment

# comment after blank and comment
#3...........
        },

        'git181.def' => {
            source => "git181",
            params => "def",
            expect => <<'#4...........',
on develop => sub {
    requires 'Template'           => '0';
    requires 'Test::Perl::Critic' => '0';
    requires 'Test::Pod'          => '1.26';
}

# end
#4...........
        },

        'git181.git181' => {
            source => "git181",
            params => "git181",
            expect => <<'#5...........',
on develop => sub {
    requires 'Template'           => '0';
    requires 'Test::Perl::Critic' => '0';
    requires 'Test::Pod'          => '1.26'
}

# end
#5...........
        },

        'git32.def' => {
            source => "git32",
            params => "def",
            expect => <<'#6...........',
use Foo qw(myfunction);

sub run {
    myfunction('x');
    my $res = myfunction ? 'X' : 'Y';
    print ' ?';
}
#6...........
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

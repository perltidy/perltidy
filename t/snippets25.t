# Created with: ./make_t.pl

# Contents:
#1 novalign.def
#2 novalign.novalign1
#3 novalign.novalign2
#4 novalign.novalign3
#5 lp2.def
#6 lp2.lp

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
        'def'       => "",
        'lp'        => "-lp",
        'novalign1' => "-novalign",
        'novalign2' => "-nvsc -nvbc -msc=2",
        'novalign3' => "-nvc",
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

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

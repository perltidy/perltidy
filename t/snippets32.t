# Created with: ./make_t.pl

# Contents:
#1 kblx.kblx6
#2 kblx.kblx7
#3 kblx.kblx8
#4 git181.def
#5 git181.git181
#6 git32.def
#7 git182.def
#8 git182.git182
#9 objectpad.def
#10 objectpad.objectpad
#11 git183.def
#12 git183.git183
#13 here3.def
#14 bocp.bocp1
#15 bocp.def
#16 here4.def
#17 bocp.bocp2
#18 cpbw.cpbw1
#19 cpbw.def

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
        'bocp1' => <<'----------',
-bocp='w'
----------
        'bocp2' => <<'----------',
-bocp='w'
-mft=1
----------
        'cpbw1' => <<'----------',
--cuddled-paren-brace
--cuddled-paren-brace-weld
----------
        'def'    => "",
        'git181' => "-nasc",
        'git182' => <<'----------',
-nwrs='A'
----------
        'git183' => "-lp -l=200 -sfei=L1000",
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
        'objectpad' => <<'----------',
-nwrs='A'
----------
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

        'bocp' => <<'----------',
        my ( $rOpts, $config_file, $rraw_options, $roption_string,
            $rexpansion, $roption_category, $rinteger_option_range );

        is( sprintf( "%#*vo", ":", "Perl" ),
            '0120:0145:0162:0154', 'ASCII sprintf("%#*vo", ":", "Perl")' );

        initialize_warn_variable_types( $wvt_in_args, $num_files,
            $line_range_clipped );
----------

        'cpbw' => <<'----------',
if (   !$NO_UTF
    && defined %unicode_table && length(%unicode_table) > 2 )
{ # side comment
}
----------

        'git181' => <<'----------',
on develop => sub {
    requires 'Template'           => '0';
    requires 'Test::Perl::Critic' => '0';
    requires 'Test::Pod'          => '1.26'
}

# end
----------

        'git182' => <<'----------',
class Thing {
    ADJUST :params (:$thingy, :$stuff) {
        ...;    #doing things
    };
}
----------

        'git183' => <<'----------',
my @options = (
    'bin-count|bins=i' => \$binn =>
      'Valuation histogram: Number of bins',
    'parcel-classifications|p=s' => \@classes =>
      'File of parcel classifications',
    'help|h' => \$help => 'This help ',
);

#<<< id=L1000
my @options = (
    'bin-count|bins=i' => \$binn =>
      'Valuation histogram: Number of bins',
    'parcel-classifications|p=s' => \@classes =>
      'File of parcel classifications',
    'help|h' => \$help => 'This help ',
);
#>>>
----------

        'git32' => <<'----------',
use Foo qw(myfunction);
sub run {
    myfunction('x');
    my $res = myfunction ? 'X' : 'Y';  print ' ?';
}
----------

        'here3' => <<'----------',
my $animal="Mice";
my $state="Blind";
print "${ \<< 'END1'}$ { \<< \"END2\" }$ { \<<END3 }";
Three
END1
$state
END2
$animal
END3
----------

        'here4' => <<'----------',
# Note that the 'OLD' text does not immediately follow the <<OLD target line.
# The basic rule is: at each closing ')' look for any << doc on that same line
# but postpone scanning for the text until the last ')'.
my $string = "Three Little Pigs";
$string =~ s(${\<<OLD})(${\<<NEW}
Cute
NEW
)ex;
Little
OLD
if($string){print $string,"\n"};
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

        'objectpad' => <<'----------',
# Some tests of Object::Pad syntax
class BClass {
    field $data :param;
    method $priv { "data<$data>" }
    method m     { return $self->$priv }
}

class AllTheTypesByBlock {
    field $scalar  { "one" }
    field @array   { "two", "three" }
    field %hash    { four => "five" }
    field $__dummy { $class_in_fieldblock = __CLASS__ }
}
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

        'git182.def' => {
            source => "git182",
            params => "def",
            expect => <<'#7...........',
class Thing {
    ADJUST : params (:$thingy, :$stuff) {
        ...;    #doing things
    };
}
#7...........
        },

        'git182.git182' => {
            source => "git182",
            params => "git182",
            expect => <<'#8...........',
class Thing {
    ADJUST :params (:$thingy, :$stuff) {
        ...;    #doing things
    };
}
#8...........
        },

        'objectpad.def' => {
            source => "objectpad",
            params => "def",
            expect => <<'#9...........',
# Some tests of Object::Pad syntax
class BClass {
    field $data : param;
    method $priv { "data<$data>" }
    method m     { return $self->$priv }
}

class AllTheTypesByBlock {
    field $scalar  { "one" }
    field @array   { "two", "three" }
    field %hash    { four => "five" }
    field $__dummy { $class_in_fieldblock = __CLASS__ }
}
#9...........
        },

        'objectpad.objectpad' => {
            source => "objectpad",
            params => "objectpad",
            expect => <<'#10...........',
# Some tests of Object::Pad syntax
class BClass {
    field $data :param;
    method $priv { "data<$data>" }
    method m     { return $self->$priv }
}

class AllTheTypesByBlock {
    field $scalar  { "one" }
    field @array   { "two", "three" }
    field %hash    { four => "five" }
    field $__dummy { $class_in_fieldblock = __CLASS__ }
}
#10...........
        },

        'git183.def' => {
            source => "git183",
            params => "def",
            expect => <<'#11...........',
my @options = (
    'bin-count|bins=i' => \$binn => 'Valuation histogram: Number of bins',
    'parcel-classifications|p=s' => \@classes =>
      'File of parcel classifications',
    'help|h' => \$help => 'This help ',
);

#<<< id=L1000
my @options = (
    'bin-count|bins=i' => \$binn =>
      'Valuation histogram: Number of bins',
    'parcel-classifications|p=s' => \@classes =>
      'File of parcel classifications',
    'help|h' => \$help => 'This help ',
);
#>>>
#11...........
        },

        'git183.git183' => {
            source => "git183",
            params => "git183",
            expect => <<'#12...........',
my @options = (
    'bin-count|bins=i' => \$binn =>
      'Valuation histogram: Number of bins',
    'parcel-classifications|p=s' => \@classes =>
      'File of parcel classifications',
    'help|h' => \$help => 'This help ',
);

#<<< id=L1000
my @options = (
                'bin-count|bins=i'           => \$binn    => 'Valuation histogram: Number of bins',
                'parcel-classifications|p=s' => \@classes => 'File of parcel classifications',
                'help|h'                     => \$help    => 'This help ',
);
#>>>
#12...........
        },

        'here3.def' => {
            source => "here3",
            params => "def",
            expect => <<'#13...........',
my $animal = "Mice";
my $state  = "Blind";
print "${ \<< 'END1'}$ { \<< \"END2\" }$ { \<<END3 }";
Three
END1
$state
END2
$animal
END3
#13...........
        },

        'bocp.bocp1' => {
            source => "bocp",
            params => "bocp1",
            expect => <<'#14...........',
        my (
            $rOpts,          $config_file, $rraw_options,
            $roption_string, $rexpansion,  $roption_category,
            $rinteger_option_range
        );

        is(
            sprintf( "%#*vo", ":", "Perl" ),
            '0120:0145:0162:0154',
            'ASCII sprintf("%#*vo", ":", "Perl")'
        );

        initialize_warn_variable_types(
            $wvt_in_args, $num_files, $line_range_clipped
        );
#14...........
        },

        'bocp.def' => {
            source => "bocp",
            params => "def",
            expect => <<'#15...........',
        my ( $rOpts, $config_file, $rraw_options, $roption_string,
            $rexpansion, $roption_category, $rinteger_option_range );

        is( sprintf( "%#*vo", ":", "Perl" ),
            '0120:0145:0162:0154', 'ASCII sprintf("%#*vo", ":", "Perl")' );

        initialize_warn_variable_types( $wvt_in_args, $num_files,
            $line_range_clipped );
#15...........
        },

        'here4.def' => {
            source => "here4",
            params => "def",
            expect => <<'#16...........',
# Note that the 'OLD' text does not immediately follow the <<OLD target line.
# The basic rule is: at each closing ')' look for any << doc on that same line
# but postpone scanning for the text until the last ')'.
my $string = "Three Little Pigs";
$string =~ s(${\<<OLD})(${\<<NEW}
Cute
NEW
)ex;
Little
OLD
if ($string) { print $string, "\n" }
#16...........
        },

        'bocp.bocp2' => {
            source => "bocp",
            params => "bocp2",
            expect => <<'#17...........',
        my (
            $rOpts,
            $config_file,
            $rraw_options,
            $roption_string,
            $rexpansion,
            $roption_category,
            $rinteger_option_range
        );

        is(
            sprintf( "%#*vo", ":", "Perl" ),
            '0120:0145:0162:0154',
            'ASCII sprintf("%#*vo", ":", "Perl")'
        );

        initialize_warn_variable_types(
            $wvt_in_args, $num_files, $line_range_clipped
        );
#17...........
        },

        'cpbw.cpbw1' => {
            source => "cpbw",
            params => "cpbw1",
            expect => <<'#18...........',
if (
       !$NO_UTF
    && defined %unicode_table
    && length(%unicode_table) > 2
) {    # side comment
}
#18...........
        },

        'cpbw.def' => {
            source => "cpbw",
            params => "def",
            expect => <<'#19...........',
if (   !$NO_UTF
    && defined %unicode_table
    && length(%unicode_table) > 2 )
{    # side comment
}
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

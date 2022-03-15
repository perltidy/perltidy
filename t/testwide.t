use strict;
use utf8;
use Test;
use Carp;
use FindBin;
BEGIN { unshift @INC, "./" }
BEGIN { plan tests => 3 }
use Perl::Tidy;

my $source = <<'EOM';
%pangrams=("Plain","ASCII",
"Zwölf große Boxkämpfer jagen Vik quer über den Sylter.","DE",
"Jeż wlókł gęś. Uf! Bądź choć przy nim, stań!","PL",
"Любя, съешь щипцы, — вздохнёт мэр, — кайф жгуч.","RU");
EOM

my $expected_output = <<'EOM';
%pangrams = (
             "Plain",                                                  "ASCII",
             "Zwölf große Boxkämpfer jagen Vik quer über den Sylter.", "DE",
             "Jeż wlókł gęś. Uf! Bądź choć przy nim, stań!",           "PL",
             "Любя, съешь щипцы, — вздохнёт мэр, — кайф жгуч.",        "RU"
            );
EOM

my $perltidyrc = <<'EOM';
-gnu -enc=utf8
EOM

my $output;

# The source is in character mode here, so perltidy will not decode.
# So here we do not need to set -eos or -neos
Perl::Tidy::perltidy(
    source      => \$source,
    destination => \$output,
    perltidyrc  => \$perltidyrc,
    argv        => '-nsyn',
);

ok( $output, $expected_output );

Perl::Tidy::perltidy(
    source      => $FindBin::Bin . '/testwide.pl.src',
    destination => \$output,
    perltidyrc  => \$perltidyrc,
    argv        => '-nsyn',
);

# We have to be careful here ...  In this test we are comparing $output to a
# source string which is in character mode (since it is in this file declared
# with 'use utf8'). We need to compare strings which have the same storage
# mode.

# The internal storage mode of $output was character mode (decoded) for
# vesions prior to 20220217.02, but is byte mode (encoded) for the latest
# version of perltidy.

# The following statement will decode $output if it is stored in byte mode,
# and leave it unchanged (and return an error) otherwise.  So this will work
# with all version of perltidy.  See https://perldoc.perl.org/utf8
utf8::decode($output);

ok( $output, $expected_output );

# Test writing encoded output to stdout with the -st flag
# References: RT #133166, RT #133171, git #35
$output = "";
do {

    # Send STDOUT to a temporary file
    use File::Temp ();
    my $fh      = new File::Temp();
    my $tmpfile = $fh->filename;

    # Note that we are not specifying an encoding here. Perltidy should do that.
    local *STDOUT;
    open STDOUT, '>', $tmpfile or die "Can't open tmpfile: $!";

    Perl::Tidy::perltidy(
        source => \$source,
        ##destination => ... we are using -st, so no destination is specified
        perltidyrc => \$perltidyrc,
        argv       => '-nsyn -st',    # added -st
    );
    close STDOUT;

    # Read the temporary file back in. Note that here we need to specify
    # the encoding.
    open TMP, '<', $tmpfile;
    binmode TMP, ":raw:encoding(UTF-8)";
    while ( my $line = <TMP> ) { $output .= $line }
};

ok( $output, $expected_output );


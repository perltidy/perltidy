use strict;
use utf8;
use Test;
use Carp;
use FindBin;
BEGIN {unshift @INC, "./"}
BEGIN {plan tests => 3}
use Perl::Tidy; 


my $source = <<'EOM';
%pangrams=("Plain","ASCII",
"Zwölf große Boxkämpfer jagen Vik quer über den Sylter.","DE",
"Jeż wlókł gęś. Uf! Bądź choć przy nim, stań!","PL",
"Любя, съешь щипцы, — вздохнёт мэр, — кайф жгуч.","RU");
EOM

my $expected_output=<<'EOM';
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

Perl::Tidy::perltidy(
    source      => \$source,
    destination => \$output,
    perltidyrc  => \$perltidyrc,
    argv        => '-nsyn',
);

ok($output, $expected_output);

Perl::Tidy::perltidy(
    source      => $FindBin::Bin . '/testwide.pl.src',
    destination => \$output,
    perltidyrc  => \$perltidyrc,
    argv        => '-nsyn',
);

ok($output, $expected_output);

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
        argv       => '-nsyn -st',  # added -st
    );
    close STDOUT;

    # Read the temporary file back in. Note that here we need to specify
    # the encoding.
    open TMP, '<', $tmpfile;
    binmode TMP, ":raw:encoding(UTF-8)";
    while ( my $line = <TMP> ) { $output .= $line }
};

ok($output, $expected_output);


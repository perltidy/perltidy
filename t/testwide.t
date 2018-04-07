use strict;
use utf8;
use Test;
use Carp;
use FindBin;
BEGIN {unshift @INC, "./"}
BEGIN {plan tests => 2}
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


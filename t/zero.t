use strict;
use Test;
use Carp;
BEGIN {plan tests => 1}
use Perl::Tidy; 

#-----------------------------------------------------------
# test formatting a single character '0' with no line ending
#-----------------------------------------------------------
my $source = '0';
my $perltidyrc = <<'EOM';
-noadd-terminal-newline
EOM

my $output;

Perl::Tidy::perltidy(
    source      => \$source,
    destination => \$output,
    perltidyrc  => \$perltidyrc,
    argv        => '',
);

my $expected_output='0';
ok($output, $expected_output);

use strict;
use warnings;

use Test::More;

plan tests => 4;

use Perl::Tidy;


my ( $source, $expected_output, $name );

my @tests;

{  
  $name = "Function signature using a do block";

  $source = <<'EOM';
sub foo($x, $y = do { 2 } ) {
  42;
}
EOM

  $expected_output = <<'EOM';
sub foo ( $x, $y = do { 2 } ) {
    42;
}
EOM

  push @tests, [ $source, $expected_output, $name ];
}


{
  $name = "Function signature with two args";

  $source = <<'EOM';
sub foo($x, $y) {
  $x + $y;
}
EOM

  $expected_output = <<'EOM';
sub foo ( $x, $y ) {
    $x + $y;
}
EOM

  push @tests, [ $source, $expected_output, $name ];
}


{
  $name = "Function signature with optional arg";

  $source = <<'EOM';
sub foo($x, $y="abcd") {
  $x.$y;
}
EOM

  $expected_output = <<'EOM';
sub foo ( $x, $y = "abcd" ) {
    $x . $y;
}
EOM

  push @tests, [ $source, $expected_output, $name ];
}

{
  $name = "Function signature with multiple optional args and do block";

  $source = <<'EOM';
sub foo($x, $y=do{{}}, $z=42, $w=do{"abcd"}) {
  $x.$y.$z;
}
EOM

  $expected_output = <<'EOM';
sub foo ( $x, $y = do { {} }, $z = 42, $w = do { "abcd" } ) {
    $x . $y . $z;
}
EOM

  push @tests, [ $source, $expected_output, $name ];
}


my $perltidyrc = <<'EOM';
EOM

foreach my $t ( @tests ) {
  my ( $source, $expected_output, $name ) = @$t;

  diag $name;

  my $output;

  Perl::Tidy::perltidy(
      source      => \$source,
      destination => \$output,
      perltidyrc  => \$perltidyrc,
      argv        => '-nsyn',
  );

  is( $output, $expected_output, $name) or diag $output;
}






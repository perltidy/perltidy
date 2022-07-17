# Test that the -D (-DEBUG) flag works
use strict;
use Carp;
use Perl::Tidy;
use Test::More;
my $name = 'DEBUG test';

BEGIN {
    plan tests => 2;
}

my $source = <<'EOM';
my @words = qw(
alpha beta gamma
);
EOM

my $expect = <<'EOM';
my @words = qw(
  alpha beta gamma
);
EOM

my $debug_expect = <<'EOM';
Use -dump-token-types (-dtt) to get a list of token type codes
1: my @words = qw(
1: kkbiiiiiib=bqqq
2: alpha beta gamma
2: qqqqqqqqqqqqqqqq
3: );
3: q;
EOM

my $output;
my $stderr_string;
my $errorfile_string;
my $debug_string;
my $perltidyrc = "";
my $err    = Perl::Tidy::perltidy(
    argv        => '-D -npro',
    perltidyrc  => \$perltidyrc,  # avoid reading unwanted .perltidyrc
    source      => \$source,
    destination => \$output,
    stderr      => \$stderr_string,
    errorfile   => \$errorfile_string,    # not used when -se flag is set
    debugfile   => \$debug_string,
);

if ( $err || $stderr_string || $errorfile_string ) {
    ok(0);
}
else {
    is( $output,       $expect,       $name );
    is( $debug_string, $debug_expect, $name );
}

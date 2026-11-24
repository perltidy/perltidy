# Test that an undefined source string, or undefined lines in a source
# array, are treated as empty instead of causing Perl warnings.
use strict;
use warnings;
use Test::More;
use Perl::Tidy;

plan tests => 4;

foreach my $case (
    [ 'undefined string', \undef,                      q{} ],
    [ 'undefined lines',  [ undef, "my \$x=1;\n", undef ], "my \$x = 1;\n" ],
  )
{
    my ( $name, $source, $expect ) = @{$case};
    my ( $output, $stderr_string, @warnings );
    local $SIG{__WARN__} = sub { push @warnings, $_[0] };
    Perl::Tidy::perltidy(
        argv        => '-npro -nmem',
        source      => $source,
        destination => \$output,
        stderr      => \$stderr_string,
    );
    is( scalar(@warnings), 0, "$name: no warnings" )
      or diag( join( q{}, @warnings ) );
    is( $output, $expect, "$name: output" );
}

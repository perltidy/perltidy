# Test that parameters which follow an abbreviation definition in a
# perltidyrc file are not added to that abbreviation, and that redefining
# an abbreviation reports the line number without Perl warnings.
use strict;
use warnings;
use Test::More;
use Perl::Tidy;

plan tests => 6;

my $source = "if (1) {\nmy \$x = 1;\n}\n";

sub run_perltidy {
    my ( $perltidyrc, $argv ) = @_;
    my ( $output, $stderr_string, @warnings );
    local $SIG{__WARN__} = sub { push @warnings, $_[0] };
    my $err = Perl::Tidy::perltidy(
        argv        => "-nmem $argv",
        perltidyrc  => \$perltidyrc,
        source      => \$source,
        destination => \$output,
        stderr      => \$stderr_string,
    );
    return ( $err, $output, $stderr_string, \@warnings );
}

my $indent8 = "if (1) {\n        my \$x = 1;\n}\n";
my $indent2 = "if (1) {\n  my \$x = 1;\n}\n";

foreach my $case (
    [ 'after one-line abbreviation',   "keycheck { -wsk -wuk }\n-i=8\n" ],
    [ 'after multi-line abbreviation', "keycheck {\n-wsk\n-wuk\n}\n-i=8\n" ],
  )
{
    my ( $name, $perltidyrc ) = @{$case};
    my $output = ( run_perltidy( $perltidyrc, q{} ) )[1];
    is( $output, $indent8, "$name: following parameter is applied" );
}

# The abbreviation must expand to its own parameters only
my $output =
  ( run_perltidy( "keycheck { -wsk }\n-i=8\n", '-i=2 -keycheck' ) )[1];
is( $output, $indent2, 'abbreviation does not include later parameters' );

# Redefining an abbreviation
my ( $err, undef, $stderr_string, $rwarnings ) =
  run_perltidy( "zzq { -i=2 }\n\nzzq { -i=3 }\n", q{} );
is( $err, 1, 'redefinition is an error' );
like(
    $stderr_string,
    qr/redefine alias \(zzq\) in config file .* line 3$/m,
    'redefinition message gives the line number'
);
is( scalar( @{$rwarnings} ), 0, 'redefinition: no Perl warnings' )
  or diag( join( q{}, @{$rwarnings} ) );

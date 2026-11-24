# Test the log message written when -enc=guess (the default) guesses an
# encoding other than utf8: it used to print an empty encoding name.
use strict;
use warnings;
use Encode ();
use Test::More;
use Perl::Tidy;

plan tests => 1;

# UTF-16 with a byte order mark, which Encode::Guess recognizes
my $source = "\xFF\xFE" . Encode::encode( 'UTF-16LE', "my \$x = 1;\n" );
my ( $output, $log, $stderr_string );
Perl::Tidy::perltidy(
    argv        => '-npro -nmem -log',
    source      => \$source,
    destination => \$output,
    logfile     => \$log,
    stderr      => \$stderr_string,
);
like( $log, qr/Guessed encoding 'UTF-16[^']*' is not utf8/,
    'guessed encoding is named in the log' );

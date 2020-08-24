use strict;
use Test::More;
use Carp;
use Perl::Tidy;
BEGIN { plan tests => 1 }

# Check that pod2html is working.
# Added to check for issue RT #133161, in which pod2html 
# was not getting called.

my $source = <<'----------';
=head1 APPENDIX

The rest of the documentation details each of the object
methods. Internal methods are usually preceded with a _

=cut

# Let the code begin...

package Astro::Circus;
use vars qw(@ISA);
use strict;
----------

my $output;
my $params = '-html';
my $stderr_string;
my $errorfile_string;
my $err = Perl::Tidy::perltidy(
    source      => \$source,
    destination => \$output,
    perltidyrc  => \$params,
    argv        => '',                 # for safety; hide any ARGV from perltidy
    stderr      => \$stderr_string,
    errorfile   => \$errorfile_string, # not used when -se flag is set
);
if ( $err==1 || $stderr_string || $errorfile_string || !$output ) {
    print STDERR "Error output received\n";
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
    if ( !$output ) {
        print STDERR "---------------------\n";
        print STDERR "No output produced\n";
        print STDERR "---------------------\n";
        ok($output);
    }
}
else {

    # The html header content can vary with system, so we will
    # just make sure the output looks like html
    if ( $output =~ /<\/html>\s*$/ ) {
        ok( 1, "looks like html" );
    }
    else {
        print STDERR "---------------------\n";
        print STDERR "output does not seem to be html\n";
        print STDERR "---------------------\n";
    }
}

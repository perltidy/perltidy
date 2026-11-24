# Test that a file given twice under different names is formatted only
# once with -b, so that its backup keeps the original.
use strict;
use warnings;
use Cwd qw( getcwd );
use File::Temp qw( tempdir );
use Test::More;
use Perl::Tidy;

plan tests => 3;

my $cwd = getcwd();
my $dir = tempdir( CLEANUP => 1 );
chdir($dir) or die "cannot chdir to '$dir': $!\n";

sub write_file {
    my ( $name, $text ) = @_;
    open( my $fh, '>', $name ) or die "cannot write '$name': $!\n";
    print {$fh} $text;
    close($fh) or die "cannot close '$name': $!\n";
    return;
}

sub read_file {
    my ($name) = @_;
    open( my $fh, '<', $name ) or return;
    local $/ = undef;
    my $text = <$fh>;
    close($fh);
    return $text;
}

my $original = "my   \$x  =  1 ;\n";

# -b with the same file under two names
write_file( 'y.pl', $original );
my $stderr_string;
Perl::Tidy::perltidy(
    argv   => [ '-npro', '-b', 'y.pl', './y.pl' ],
    stderr => \$stderr_string,
);
is( read_file('y.pl.bak'), $original, '-b y.pl ./y.pl: backup is the original' );
is( read_file('y.pl'), "my \$x = 1;\n", '-b y.pl ./y.pl: file is formatted' );

# Without -b, both names are still processed
write_file( 'z.pl', $original );
Perl::Tidy::perltidy(
    argv   => [ '-npro', 'z.pl', './z.pl' ],
    stderr => \$stderr_string,
);
ok( -e 'z.pl.tdy', 'without -b, z.pl ./z.pl: output written' );


chdir($cwd) or die "cannot chdir back to '$cwd': $!\n";

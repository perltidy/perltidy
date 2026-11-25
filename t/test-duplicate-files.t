# Test that a file given twice under different names is formatted only
# once with -b, so that its backup keeps the original, that -opath
# refuses input files whose output files would have the same name, and
# that -o refuses to overwrite its input file given under another name.
use strict;
use warnings;
use Cwd qw( getcwd );
use File::Temp qw( tempdir );
use Test::More;
use Perl::Tidy;

plan tests => 9;

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

# -opath with two input files with the same base name
mkdir('a') or die "cannot create 'a': $!\n";
mkdir('b') or die "cannot create 'b': $!\n";
write_file( 'a/x.pl', "my \$from_a=1;\n" );
write_file( 'b/x.pl', "my \$from_b=2;\n" );
my $err = Perl::Tidy::perltidy(
    argv   => [ '-npro', '-opath=out', 'a/x.pl', 'b/x.pl' ],
    stderr => \$stderr_string,
);
ok( $err, '-opath with the same base name twice: error' );
like( $stderr_string, qr/would have the same output files/,
    '-opath with the same base name twice: message' );
ok( !-e 'out/x.pl.tdy', '-opath with the same base name twice: nothing written' );

# -o with the input file under another name
write_file( 'w.pl', $original );
$err = Perl::Tidy::perltidy(
    argv   => [ '-npro', '-o', './w.pl', 'w.pl' ],
    stderr => \$stderr_string,
);
ok( $err, '-o ./w.pl w.pl: error' );
like( $stderr_string, qr/to modify in-place/, '-o ./w.pl w.pl: message' );
is( read_file('w.pl'), $original, '-o ./w.pl w.pl: input unchanged' );

chdir($cwd) or die "cannot chdir back to '$cwd': $!\n";

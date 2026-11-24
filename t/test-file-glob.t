# Test the expansion of file globs given on the command line (needed on
# systems whose shell does not expand them): only '*', '?' and '[...]' are
# wildcards, and every other character matches itself.
use strict;
use warnings;
use Cwd qw( getcwd );
use File::Temp qw( tempdir );
use Test::More;
use Perl::Tidy;

my @cases = (

    # glob, file name, should match
    [ '*.pl',       'a.pl',     1 ],
    [ '*.pl|*',     'README',   0 ],
    [ 'c++*',       'cat.pl',   0 ],
    [ 'c++*',       'c++.pl',   1 ],
    [ 'x[.pl',      'x[.pl',    1 ],
    [ '[ab].pl',    'a.pl',     1 ],
    [ '[ab].pl',    'c.pl',     0 ],
    [ '[!a].pl',    'a.pl',     0 ],
    [ '[!a].pl',    'b.pl',     1 ],
    [ '*.[pP][lL]', 'x.PL',     1 ],
    [ '?.pl',       'ab.pl',    0 ],
    [ '*.p{l,m}',   'x.p{l,m}', 1 ],
);
plan tests => @cases + 3;

foreach my $case (@cases) {
    my ( $glob, $name, $want ) = @{$case};
    my $re = Perl::Tidy::fileglob_to_re($glob);
    is( ( $name =~ /$re/ ? 1 : 0 ), $want, "'$glob' vs '$name'" );
}

# Run perltidy itself on globs, in a directory with a few files
my $cwd = getcwd();
my $dir = tempdir( CLEANUP => 1 );
chdir($dir) or die "cannot chdir to '$dir': $!\n";
foreach my $file ( 'a.pl', 'README' ) {
    open( my $fh, '>', $file ) or die "cannot write '$file': $!\n";
    print {$fh} "my \$x=1;\n";
    close($fh) or die "cannot close '$file': $!\n";
}
my $stderr_string;
Perl::Tidy::perltidy(
    argv   => [ '-npro', '-b', '*.pl|*' ],
    stderr => \$stderr_string,
);
ok( !-e 'README.bak', "'*.pl|*' does not select other files" );
my $ok = eval {
    Perl::Tidy::perltidy(
        argv   => [ '-npro', '-b', 'x[.pl' ],
        stderr => \$stderr_string,
    );
    1;
};
ok( $ok, "an unmatched '[' does not make perltidy die" );
like( $stderr_string, qr/no matching input files/,
    "an unmatched '[' is reported as no match" );
chdir($cwd) or die "cannot chdir back to '$cwd': $!\n";

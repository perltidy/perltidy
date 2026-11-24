# Test that the input file name is escaped in the frame files written with
# -html -frm: it goes into href and src attributes, and a crafted name could
# otherwise inject markup or scripts into the generated pages.
use strict;
use warnings;
use File::Temp qw( tempdir );
use Test::More;
use Perl::Tidy;

if ( $^O =~ /^(?:MSWin32|cygwin|msys)$/ ) {
    plan skip_all => 'file names with < > " are not allowed on Windows';
}
plan tests => 4;

my $dir  = tempdir( CLEANUP => 1 );
my $name = 'x"><img src=x onerror=alert(1)>.pl';

open( my $fh, '>', "$dir/$name" ) or die "cannot write '$dir/$name': $!\n";
print {$fh} "package Foo;\nsub bar { 1 }\n";
close($fh) or die "cannot close '$dir/$name': $!\n";

my $stderr_string;
my $err = Perl::Tidy::perltidy(
    argv   => [ '-npro', '-html', '-frm', "$dir/$name" ],
    stderr => \$stderr_string,
);
ok( !$err, 'perltidy ran without errors' )
  or diag("stderr: $stderr_string");

foreach my $ext (qw( html toc.html src.html )) {
    my $file = "$dir/$name.$ext";
    my $text = q{};
    if ( open( my $in, '<', $file ) ) {
        local $/ = undef;
        $text = <$in>;
        close($in);
    }

    # The name may appear in an html comment, where perltidy removes any
    # "--", so it cannot end the comment
    $text =~ s/<!--.*?-->//gs;
    unlike( $text, qr/<img/, "no unescaped markup in .$ext" );
}

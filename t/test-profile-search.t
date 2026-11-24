# Test the upward search for a profile given as '.../name' (-pro=.../name or
# $PERLTIDY): it must find a file in a parent directory, stop at the root
# directory, including when it starts there or on Windows, where the root
# is 'C:\' or 'C:/', and not die when the start directory does not exist.
use strict;
use warnings;
use Cwd qw( realpath );
use File::Basename;
use File::Temp qw( tempdir );
use Test::More;
use Perl::Tidy;

plan tests => 7;

# Run some code, but give up after 10 seconds
sub with_timeout {
    my ($code) = @_;
    my @result;
    my $ok = eval {
        local $SIG{ALRM} = sub { die "timeout\n" };
        alarm(10);
        @result = $code->();
        alarm(0);
        1;
    };
    alarm(0);
    return $ok ? ( 'done', @result ) : ( $@ =~ /timeout/ ? 'timeout' : 'died' );
}

# A profile in a parent directory is found
my $dir = realpath( tempdir( CLEANUP => 1 ) );
mkdir("$dir/a") or die "cannot create '$dir/a': $!\n";
mkdir("$dir/a/b") or die "cannot create '$dir/a/b': $!\n";
open( my $fh, '>', "$dir/myprofile" ) or die "cannot write profile: $!\n";
close($fh);
my ( $status, $found ) =
  with_timeout( sub { Perl::Tidy::find_file_upwards( "$dir/a/b", 'myprofile' ) } );
is( $found, "$dir/myprofile", 'profile found in a parent directory' );

# Missing profiles: the search must stop at the root
my $missing = 'no-such-perltidy-profile-' . $$;
( $status, $found ) =
  with_timeout( sub { Perl::Tidy::find_file_upwards( "$dir/a/b", $missing ) } );
ok( $status eq 'done' && !defined($found), 'search stops at the root' );
( $status, $found ) =
  with_timeout( sub { Perl::Tidy::find_file_upwards( '/', $missing ) } );
ok( $status eq 'done' && !defined($found), 'search starting at / stops' );

# Unresolved start directory (Cwd::realpath returned undef)
( $status, $found ) =
  with_timeout( sub { Perl::Tidy::find_file_upwards( undef, $missing ) } );
ok( $status eq 'done' && !defined($found), 'undefined start directory' );

# Windows roots, using the Windows rules of File::Basename
my $fstype = fileparse_set_fstype('MSWin32');
my @windows;
foreach my $start ( 'C:\\Users\\x\\proj', 'C:/Users/x/proj' ) {
    ( $status, $found ) =
      with_timeout( sub { Perl::Tidy::find_file_upwards( $start, $missing ) } );
    push @windows, $status eq 'done' && !defined($found);
}
fileparse_set_fstype($fstype);
ok( $windows[0], 'search stops at C:\\' );
ok( $windows[1], 'search stops at C:/' );

# perltidy reports a missing start directory instead of dying
my ( $source, $output, $stderr_string ) = ("my \$x=1;\n");
($status) = with_timeout(
    sub {
        Perl::Tidy::perltidy(
            argv        => "-nmem -pro=no-such-dir-$$/.../$missing",
            source      => \$source,
            destination => \$output,
            stderr      => \$stderr_string,
        );
    }
);
ok( $status eq 'done' && $stderr_string =~ /cannot find file/,
    'missing start directory is reported' );

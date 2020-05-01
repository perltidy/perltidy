use strict;
use Test;
use Carp;
use Perl::Tidy;

BEGIN {
    plan tests => 2;
}

my $sname = 'atee.t';

my $source = <<'EOM';
# block comment
=pod
some pod
=cut

print "hello world\n";
$xx++; # side comment
EOM


my $expect = <<'EOM';

print "hello world\n";
$xx++;
EOM

my $teefile_expect = <<'EOM';
# block comment
=pod
some pod
=cut
$xx++; # side comment
EOM

# Test capturing the .LOG, .DEBUG, .TEE outputs to strings.
# In this test we delete all comments and pod in the test script and send them
# to a .TEE file also save .DEBUG and .LOG output
my $params = "-dac -tac -D -g";

# Verify correctness of the formatted output and the .TEE output
# (.DEBUG and .LOG have been verified to work but are not checked here because 
# they may change over time, making work for maintaining this test file)

my $output;
my $teefile;
my $debugfile;
my $stderr_string;
my $errorfile_string;
my $logfile_string;
my $debugfile_string;
my $err = Perl::Tidy::perltidy(
    source      => \$source,
    destination => \$output,
    perltidyrc  => \$params,
    argv        => '',                 # for safety; hide any ARGV from perltidy
    stderr      => \$stderr_string,
    errorfile   => \$errorfile_string, # not used when -se flag is set
    teefile     => \$teefile,
    debugfile   => \$debugfile_string,
    logfile     => \$logfile_string,
);

if ( $err || $stderr_string || $errorfile_string ) {
    if ($err) {
        print STDERR "This error received calling Perl::Tidy with '$sname'\n";
        ok( !$err );
    }
    if ($stderr_string) {
        print STDERR "---------------------\n";
        print STDERR "<<STDERR>>\n$stderr_string\n";
        print STDERR "---------------------\n";
        print STDERR "This error received calling Perl::Tidy with '$sname''\n";
        ok( !$stderr_string );
    }
    if ($errorfile_string) {
        print STDERR "---------------------\n";
        print STDERR "<<.ERR file>>\n$errorfile_string\n";
        print STDERR "---------------------\n";
        print STDERR "This error received calling Perl::Tidy with '$sname''\n";
        ok( !$errorfile_string );
    }
}
else {
    ok( $output,  $expect );
    ok( $teefile, $teefile_expect );
}

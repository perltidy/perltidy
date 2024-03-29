#!/usr/bin/perl -wn

# This program was posted on the MacPerl mailing list by 
# Charles Albrecht as one way to get perltidy to work as a filter
# under BBEdit.
# 20240102: slh fixed obvious error found with -duv: ('my' inside BEGIN block)

use Perl::Tidy;

my ($input_string, $output_string);
BEGIN { $input_string = ""; $output_string = ""; }

$input_string .= $_;

END {
    my $err=Perl::Tidy::perltidy(
        source      => \$input_string,
        destination => \$output_string
    );
    if ($err){
        die "Error calling perltidy\n";
    }
    print "$output_string\n";
}

__END__


#!/usr/bin/perl
use strict;
use warnings;
use File::Temp qw{ tempfile };

# Run perltidy --dump-unique-keys on multiple files, and
# just show keys which only appear in one file.
# Requires Perl::Tidy version 20240903.08 or higher

my @files;
my %seen;
foreach my $file (@ARGV) {
    if ( !-e $file ) { die "file '$file' not found\n" }
    next if ( $seen{$file}++ );
    push @files, $file;
}

# TODO:
# - if no files, look for a MANIFEST, or
# - if 1 file, see if it is a MANIFEST, and
#   - get files of the form lib/*.pm
if ( !@files ) { exit 1 }

my ( $fh_tmp, $tmpfile ) = tempfile();
if ( !$fh_tmp ) {
    die "unable to open temporary file $tmpfile\n";
}

# Loop to run perltidy -duk on each file
foreach my $file (@files) {
    my $cmd = "perltidy -npro -duk $file >>$tmpfile";
    my $err = system($cmd);
    if ($err) { die "perltidy return error $err\n" }
}

my $fh;
if ( !open( $fh, '<', $tmpfile ) ) {
    die "cannot open my temp file '$tmpfile'\n";
}

my %word_count;
my @lines;
foreach my $line (<$fh>) {
    my $word;
    if ( $line =~ /^(.*),(\d+)\s*$/ ) {
        $word = $1;
        if ( !defined( $word_count{$word} ) ) {
            $word_count{$word} = 1;
        }
        else {
            $word_count{$word}++;
        }
    }
    push @lines, [ $line, $word ];
}
$fh->close();

my @dups = grep { $word_count{$_} > 1 } keys %word_count;
my %is_dup;
@is_dup{@dups} = (1) x scalar(@dups);

my $output_string = "";
foreach my $item (@lines) {
    my ( $line, $word ) = @{$item};
    next if ( defined($word) && $is_dup{$word} );
    $output_string .= $line;
}

print {*STDOUT} $output_string;

END {
    if ( defined($tmpfile) && -e $tmpfile ) {
        unlink $tmpfile or warn "Could not unlink $tmpfile: $!";
    }
}

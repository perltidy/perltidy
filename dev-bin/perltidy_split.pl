#!/usr/bin/perl
use strict;
use warnings;
use English;
use constant EMPTY_STRING => q();

# Read a perltidy script [default='perltidy.pl'] and remove selected modules
# and insert their 'use' equivalents. This is needed to keep the line limit
# below the maximum allowed by NYTProf.

# Usage:
#   perltidy_split.pl [module1 [module2 .. [ filename.pl
# modules can be listed for replacement [DEFAULT HtmlWriter, Tokenizer]
# optional input file ends in .pl [DEFAULT perltidy.pl]

main();

sub main {

    my $prefix = 'Perl::Tidy::';
    my $filename;
    my @modules;
    foreach my $word (@ARGV) {
        if ( $word =~ /\.pl/ ) { $filename = $word }
        else {
            if ( $word !~ /::/ ) { $word = $prefix . $word }
            push @modules, $word;
        }
    }
    if ( !@modules ) {
        @modules = qw(Perl::Tidy::HtmlWriter Perl::Tidy::Tokenizer);
    }
    if ( !defined($filename) ) { $filename = 'perltidy.pl' }

    my %is_skipped_module;
    @is_skipped_module{@modules} = (1) x scalar(@modules);

    my $wrote_use_text;
    my $string_in  = slurp_to_string($filename);
    my $string_out = EMPTY_STRING;
    my @lines      = split /^/, $string_in;
    my $in_skip    = 0;

    foreach my $line (@lines) {
        if ( $line =~ /^package\s+(.*)\b/ ) {
            if ( $is_skipped_module{$1} ) {
                $in_skip = 1;

                # bump flag to indicate being seen
                $is_skipped_module{$1} = 2;
            }
            else { $in_skip = 0 }
        }
        next if ($in_skip);
        $string_out .= $line;
        if ( !$wrote_use_text && index( $line, 'use Digest::MD5' ) >= 0 ) {
            $wrote_use_text = 1;
            foreach my $module ( keys %is_skipped_module ) {
                $string_out .= "use $module;\n";
            }
        }
    }

    my $error = EMPTY_STRING;
    foreach my $module ( keys %is_skipped_module ) {
        if ( $is_skipped_module{$module} != 2 ) {
            $error .= "Did not see '$module'\n";
        }
    }
    if ($error) { print STDERR $error; exit 1 }

    backup($filename);

    if ( open( my $fh, '>', $filename ) ) {
        $fh->print($string_out);
        $fh->close() or die("Cannot close $filename\n");
        chmod 0755, $filename;
    }
    else {
        die("Cannot open $filename: $OS_ERROR\n");
    }
} ## end sub main

sub backup {
    my ($basename) = @_;
    my $bname;
    if ( -e $basename ) {
        my $ext;
        for ( my $j = 1 ; $j <= 999 ; $j++ ) {
            $ext   = 'ba' . $j;
            $bname = "$basename.$ext";
            next if ( -e $bname || -e $bname . ".gz" );
            system "mv $basename $bname";
            last;
        }
        if ( !$bname ) {
            die "too many backup versions of $basename - move some\n";
        }
    }
    return;
} ## end sub backup

sub slurp_to_string {
    my ($filename) = @_;
    my $buf;
    if ( open( my $fh, '<', $filename ) ) {
        local $INPUT_RECORD_SEPARATOR = undef;
        $buf = <$fh>;
        $fh->close() or Warn("Cannot close $filename\n");
    }
    else {
        die("Cannot open $filename: $OS_ERROR\n");
        return;
    }
    return $buf;
} ## end sub slurp_to_string

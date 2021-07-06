#!/usr/bin/perl -w
use strict;
use warnings;

# This is a utility to stress test perltidy by inserting as many side comments
# into a script as possible.

# Usage:

# 1. Important: work in a new temporary empty directory below a directory full of
# perl scripts.

# 2. Then enter:
#   side_comment_test.pl ../*

# 3. Look at any files named '*.2.ERR' and try to resolve the problem

# 4. When done, remove the temporary directory

# NOTE: A little glitch is that unuaual here doc targets (i.e., lower case
# words) will get side comments.

# If this happens, edit the '.2' file and remove the side comment and run
# perltidy on that file again.

my $cmd;
foreach my $file (@ARGV) {
    my $basename = $file;
    if ( $basename =~ /^(.*)\/([^\/]+)$/ ) { $basename = $2 }
    my $file1 = "$basename.1";
    my $file2 = "$basename.2";
    my $file3 = "$basename.3";

    # Start by extruding the input file into as many lines as possible
    $cmd = "perltidy -extrude <$file -o $file1";
    system($cmd);

    # Skip if starting file has some kind of error
    if ( -e "perltidy.ERR" ) {
        unlink $file1;
        next;
    }

    # Add side comments and see if perltidy indicates an error
    add_side_comments( $file1, $file2 );
    $cmd = "perltidy $file2 -o $file3";
    system($cmd);

    # Clean up if no error
    if ( !-e $file2 . ".ERR" ) {
        unlink $file1;
        unlink $file2;
        unlink $file3;
    }

    # Otherwise, leave the files for analysis
}

sub add_side_comments {
    my ( $ifile, $ofile ) = @_;

    # Given file named $ifile,
    # add as many side comments as possible and write result to $ofile

    my $string1 = get_string($ifile);
    my @lines   = split /\n/, $string1;
    foreach my $line (@lines) {

        # Skip comments, including hash bang and #<< $>> lines
        next if ( $line =~ /^\s*#/ );

        next if ( $line =~ /^(__END__|__DATA__)\s*$/ );

        # Skip end of format
        next if ( $line eq '.' );

        # Optional: Avoid problems involving guessing if / starts a pattern
        next if ( $line eq '/' );

        # Try to skip here targets; see note above
        next if ( $line =~ /^\s*[A-Z_0-9=\.\-]+\s*$/ );
        $line .= "#sc#";
    }
    my $string2 = join "\n", @lines;
    write_file( $ofile, $string2 );
}

sub get_string {
    my ($file) = @_;
    open my $fh, '<', $file or die "cannot open $file: $!\n";
    local $/ = undef;
    my $string = <$fh>;
    close $fh;
    return $string;
}

sub write_file {
    my ( $fname, $string, $msg ) = @_;
    open my $fh, '>', $fname or die "cannot open $fname: $!\n";
    $fh->print($string);
    $fh->close();
    print STDERR "Wrote $fname\n" if ($msg);
    return;
}

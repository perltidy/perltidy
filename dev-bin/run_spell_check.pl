#!/usr/bin/perl
use strict;
use warnings;

# Spell check perltidy files:
#   - Keep a list of previously found unique words for a file in filename.spell
#     and a backup of this in filename.spell.bak
#   - These are local files, excluded from git and MANIFEST
#   - Find all words with the help of perltidy (quotes, comments, pod, here-docs)
#   - Remove any of these already in the .spell file list
#   - Run the remainder through aspell in list mode to find new unknown words
#   - Fix any spelling errors
#   - When no errors remain, add the new words to filename.spell

# Requires 'aspell'

# Usage:
#  run_spell_check.pl [file1 [file2 ...
#    if args given, checks spelling of the listed files
#  run_spell_check.pl
#    if no args, cd's to perltidy git and spell checks files in MANIFEST

use File::Copy;
use File::Temp qw(tempfile);
use Data::Dumper;
use Cwd         qw(getcwd);
use Digest::MD5 qw(md5_hex);
use Encode      ();

use constant EMPTY_STRING => q{};
$| = 1;

main();

sub main {

    my $usage = <<EOM;
Spell check perltidy files
Usage:
  $0 file1 [ file2 [...
     spell check selected files
  $0 
     If no files are given, change directory to the perltidy git head
     and look for MANIFEST and use files lib/.../*.pm
     Ask which of these to process
EOM

    my $git_home    = find_git_home();
    my $rfiles      = [];
    my $file_string = EMPTY_STRING;
    if (@ARGV) {
        $rfiles      = \@ARGV;
        $file_string = join ', ', @{$rfiles};
    }
    else {
        chomp $git_home;
        chdir $git_home;
        my $MANIFEST = "MANIFEST";
        if ( -e $MANIFEST && -f $MANIFEST ) {
            $rfiles = read_MANIFEST($MANIFEST);
            my $num = @{$rfiles};
            print STDERR "Reading $MANIFEST...found $num files\n";
            if ( ifyes("Do you want to select specific files? [Y/N]") ) {
                $rfiles = select_files($rfiles);
            }
        }
    }

    if ( !@{$rfiles} ) { die $usage }

    foreach my $file ( @{$rfiles} ) {
        if ( !-e $file ) { print "$file does not exist\n"; next }
        spell_check($file);
    }
    query("Hit <cr> to continue\n");
    return;
} ## end sub main

sub read_MANIFEST {
    my ($MANIFEST) = @_;

    # scan MANIFEST for existing files of the form 'lib/.../*.pm'
    my $fh;
    if ( !open( $fh, '<', $MANIFEST ) ) {
        die "cannot open '$MANIFEST': $!\n";
    }
    my @files;
    foreach my $line (<$fh>) {
        chomp $line;
        next unless ($line);
        my @parts = split '/', $line;
        if ( $parts[0] eq 'bin' && $parts[-1] eq 'perltidy' ) {
            push @files, $line;
            next;
        }
        if ( $parts[0] ne 'lib' )     { next }
        if ( $parts[-1] !~ /\.pm$/i ) { next }
        if ( -e $line )               { push @files, $line }
    }
    return \@files;
} ## end sub read_MANIFEST

sub select_files {
    my ($rfiles) = @_;
    my $imax = -1;
    foreach my $file ( @{$rfiles} ) {
        $imax++;
        print "$imax $file\n";
    }
    my @ilist;
    my $ans = query("Enter the number(s) of the files to process\n");
    $ans =~ s/,/ /g;
    $ans =~ s/\.\./-/g;
    my @parts = split /\s+/, $ans;
    foreach my $part (@parts) {
        if ( index( $part, '-' ) >= 0 ) {
            my @list = split /-/, $part;
            if ( @list != 2 ) {
                query("Error processing '$part', hit <cr>\n");
                return;
            }
            my $i1 = $list[0];
            my $i2 = $list[1];
            if ( $i1 < 0 || $i2 > $imax || $i2 < $i1 ) {
                query("Error processing '$part', hit <cr>\n");
                return;
            }
            foreach my $ix ( $i1 .. $i2 ) { $ilist[$ix] = 1 }
            next;
        }
        if ( $part =~ /^\d+$/ && $part >= 0 && $part <= $imax ) {
            $ilist[$part] = 1;
        }
        else {
            query("Error processing '$part' with imax=$imax, hit <cr>\n");
            return;
        }
    }
    my @selected;
    foreach my $ix ( 0 .. $imax ) {
        if ( $ilist[$ix] ) {
            push @selected, $rfiles->[$ix];
            print "selected $ix\n";
        }
    }
    return \@selected;
} ## end sub select_files

sub find_git_home {
    my ( $fh_uu, $err_file ) = File::Temp::tempfile();

    # See if we are within the perltidy git
    my $git_home = qx[git rev-parse --show-toplevel 2>$err_file];
    chomp $git_home;
    if ( -e $err_file ) { unlink($err_file) }
    return $git_home;
} ## end sub find_git_home

sub spell_check {
    my ($source) = @_;

    print "Checking $source..";

    if ( !-e $source ) {
        print "file '$source' does not exist; hit <cr>\n";
        return;
    }

    # read a list of words for this file, if any
    my $rwords         = [];
    my $dot_spell_file = "$source.spell";
    if ( -e $dot_spell_file ) {
        $rwords = read_word_file($dot_spell_file);
    }

    # This will hold the words
    my $rdestination = [];

    # spell check $file
    PerlSpell::perlspell(
        _source         => $source,
        _rdestination   => $rdestination,
        _want_quotes    => 1,
        _want_here_text => 1,
        _want_pod       => 1,
        _want_comments  => 1,
        _rknown_words   => $rwords,
    );

    if ( !@{$rdestination} ) {
        print "..no new words found\n";
    }
    else {

        # Make a temporary file of words not in the .spell dictionary
        my ( $fh_uu, $words_file ) = tempfile();
        my $string = join "\n", @{$rdestination};
        write_file( $words_file, $string );

        # This will be the file of words unknown to aspell
        my $list_file = "$source.tmp";
        if ( -e $list_file ) { unlink($list_file) }

        # Run aspell with 'list' option to get words not in global dictionary
        system("aspell list <$words_file >$list_file");
        unlink($words_file);

        # Decide what to do
        if ( !-e $list_file || -z $list_file ) {
            print "..no unknown words found\n";
            return;
        }
        else {
            print "\n";
            handle_unknown_words( $list_file, $dot_spell_file, $rwords );
        }
    }
    return;
} ## end sub spell_check

sub handle_unknown_words {

    my ( $list_file, $dot_spell_file, $rwords ) = @_;

    # Given:
    #   $list_file = the temporary file with new unknown words
    #   $dot_spell_file = the .spell dictionary file for the file being checked
    #   $rwords = ref to the list of words already in the .spell dictionary file
    # Task:
    #   Ask user what to do:
    #     - add the new words to the dictionary if all ok, or
    #     - continue and fix later

    my $string  = slurp_file($list_file);
    my @words   = split /^/, $string;
    my $num_new = @words;
    openurl($list_file);

    while (1) {
        print <<EOM;
Found $num_new unknown words in $list_file. Please select an option:
Please review this list and select one of these options:

M  - ALL OK:  Merge these new words into the '.spell' list for this file,
              save a backup of the previous '.spell' file as '.spell.bak',
              remove the file with new words '$list_file'. 
X  - FIX LATER:
              You can review the new words in '$list_file' later, and
              fix any misspellings in the source, and
              rerun this program and select 'M' if everything looks good.
              NOTE: for suggestions, try 'aspell -c $list_file', answer 'i' 
              to keep from adding words to the local aspell word list.
EOM
        my $ans = queryu(":");
        if ( $ans eq 'M' ) {
            push @words, @{$rwords};
            foreach (@words) { chomp }
            @words = sort { $a cmp $b } @words;
            @words = uniq(@words);
            my $num     = @words;
            my $ostring = join "\n", @words;
            write_file_with_backup( $dot_spell_file, $ostring );
            print "Wrote $num words to $dot_spell_file\n";
            unlink($list_file);
            last;
        }
        elsif ( $ans eq 'X' ) {
            last;
        }
        else {
            print "Unknown response: '$ans', try again\n";
        }
    } ## end while (1)
    return;
} ## end sub handle_unknown_words

sub read_word_file {
    my ($fname) = @_;

    # Read a single file containing words and return a list ref
    #    $rwords_by_file = \@words;

    if ( !-e $fname ) {
        return [];
    }

    my $string = slurp_file($fname);
    my @lines  = split /^/, $string;
    my @words;
    foreach my $line (@lines) {
        chomp $line;

        # one leading word per line; ignore anything following
        if ( $line =~ /^([A-Za-z]+)/ ) { push @words, $1 }
    }
    return \@words;
} ## end sub read_word_file

sub write_file_with_backup {
    my ( $fname, $ostring ) = @_;

    # Write the string $ostring to file $fname
    # Backup $fname if it already exists

    # Backup an existing file
    if ( -e $fname ) {
        my $backup_extension = ".bak";
        my $backup_name      = $fname . $backup_extension;
        if ( -f $backup_name ) {
            unlink($backup_name)
              or die(
"unable to remove previous '$backup_name' for -b option; check permissions: $!\n"
              );
        }

        # backup the old data file
        # use copy for symlinks, move for regular files
        if ( -l $fname ) {
            File::Copy::copy( $fname, $backup_name )
              or die("File::Copy failed trying to backup source: $!");
        }
        else {
            rename( $fname, $backup_name )
              or die("problem renaming $fname to $backup_name: $!\n");
        }
    }
    write_file( $fname, $ostring );
    return;
} ## end sub write_file_with_backup

sub uniq {
    my %seen;
    return grep { !$seen{$_}++ } @_;
}

#########################################################
# utils
#########################################################

sub query {
    my ($msg) = @_;
    print $msg;
    my $ans = <STDIN>;
    chomp $ans;
    return $ans;
} ## end sub query

sub queryu {
    return uc( query(@_) );
}

sub hitcr {
    my ($msg) = @_;
    if ($msg) { $msg .= " Hit <cr> to continue"; }
    else      { $msg = "Hit <cr> to continue" }
    query($msg);
} ## end sub hitcr

sub ifyes {

    # Updated to have default, which should be "Y" or "N"
    my ( $msg, ($default) ) = @_;
    my $count = 0;
  ASK:
    my $ans = query($msg);
    if ( defined($default) ) {
        $ans = $default unless ($ans);
    }
    if    ( $ans =~ /^Y/i ) { return 1 }
    elsif ( $ans =~ /^N/i ) { return 0 }
    else {
        $count++;
        if ( $count > 6 ) { die "error count exceeded in ifyes\n" }
        print STDERR "Please answer 'Y' or 'N'\n";
        goto ASK;
    }
} ## end sub ifyes

sub system_echo {
    my ( $cmd, $quiet ) = @_;
    print "$cmd\n" unless ($quiet);
    system $cmd;
    return;
} ## end sub system_echo

sub slurp_file {
    my ($file) = @_;
    my $fh;
    open( $fh, '<', $file ) or die "cannot open $file\n";
    local $/;
    my $contents = <$fh>;
    close $fh;
    return $contents;
} ## end sub slurp_file

sub write_file {
    my ( $fname, $string, ($msg) ) = @_;
    my $fh;
    open( $fh, '>', $fname ) or die "cannot open $fname: $!\n";
    $fh->print($string);
    $fh->close();
    print STDERR "Wrote $fname\n" if ($msg);
    return;
} ## end sub write_file

sub digest_string {
    my ($buf)  = @_;
    my $octets = Encode::encode( "utf8", $buf );
    my $digest = md5_hex($octets);
    return $digest;
} ## end sub digest_string

sub digest_file {
    my ($file) = @_;
    my $buf    = slurp_file($file);
    my $digest = digest_string($buf);
    return $digest;
} ## end sub digest_file

sub openurl {
    my $url      = shift;
    my $platform = $^O;
    my $cmd;
    if    ( $platform eq 'darwin' ) { $cmd = "open \"$url\""; }    # OS X
    elsif ( $platform eq 'MSWin32' or $platform eq 'msys' ) {
        $cmd = "start \"\" \"$url\"";
    }    # Windows native or MSYS / Git Bash
    elsif ( $platform eq 'cygwin' ) {
        $cmd = "cmd.exe /c start \"\" \"$url \"";
    }    # Cygwin; !! Note the required trailing space.
    else {
        $cmd = "xdg-open \"$url\"";
    } # assume a Freedesktop-compliant OS, which includes many Linux distros, PC-BSD, OpenSolaris, ...
    if ( system($cmd) != 0 ) {
        die
"Cannot locate or failed to open default browser; please open '$url' manually.";
    }
} ## end sub openurl

#####################################################################
#
# The PerlSpell package is an interface to perltidy which accepts a
# source filehandle and returns a 'masked' version of the source as
# a string or array.  It can also optionally return the original file
# as a string or array.
#
# It works by making a callback object with a write_line() method to
# receive tokenized lines from perltidy.  This write_line method
# selectively replaces tokens with either their original text or with a
# benign masking character (such as '#' or 'Q').
#
# Usage:
#
#   PerlSpell::perlspell(
#       _source         => $fh,             # required source
#       _rmasked_file   => \$masked_file,   # required ref to ARRAY or SCALAR
#       _roriginal_file => \$original_file, # optional ref to ARRAY or SCALAR
#       _compression    => $opt_c           # optional
#   );
#
# _source is any source that perltidy will accept, including a
# filehandle or reference to SCALAR or ARRAY
#
# The compression flag may have these values:
#  0 all mask file line numbers and character positions agree with
#    original file (DEFAULT)
#  1 line numbers agree and character positions agree within lines of code
#  2 line numbers agree but character positions do not
#  3 no correspondence between line numbers or character positions
#
#####################################################################

package PerlSpell;
use Carp;
use Perl::Tidy;

my (

    %word_hash,
    $want_quotes,
    $want_here_text,
    $want_pod,
    $want_comments,
);

sub store_text {
    my ($text) = @_;
    my @words  = split /\s+/, $text;
    foreach my $word (@words) {
        next if ( $word !~ /^[A-Za-z]+$/ || length($word) < 2 );
        $word_hash{$word}++;
    }
    return;
} ## end sub store_text

sub finish_formatting {

    # called once after the last line of a file
    my $self         = shift;
    my $rdestination = $self->{_rdestination};
    my $rk           = $self->{_rknown_words};

    my %is_known_word;
    if ($rk) {
        @is_known_word{ @{$rk} } = (1) x scalar( @{$rk} );
    }

    foreach my $word ( keys %word_hash ) {
        next if ( $is_known_word{$word} );
        push @{$rdestination}, "$word, $word_hash{$word}";
    }
    return;
} ## end sub finish_formatting

sub perlspell {

    my %args = (@_);

    $want_quotes    = $args{_want_quotes};
    $want_here_text = $args{_want_here_text};
    $want_pod       = $args{_want_pod};
    $want_comments  = $args{_want_comments};
    %word_hash      = ();

    # run perltidy, which will call $formatter's write_line() for each line
    my $err = perltidy(
        'source'    => $args{_source},
        'formatter' => bless( \%args, __PACKAGE__ ),    # callback object
        'argv'      => "-npro -se",    # -npro : ignore .perltidyrc,
                                       # -se   : errors to STDOUT
    );
    if ($err) {
        die "Error calling perltidy\n";
    }
    return;
} ## end sub perlspell

sub write_line {

    # This is called from perltidy line-by-line
    my ( $self, $line_of_tokens ) = @_;

    my $line_type = $line_of_tokens->{_line_type};
##  my $input_line_number = $line_of_tokens->{_line_number};
    my $input_line  = $line_of_tokens->{_line_text};
    my $rtoken_type = $line_of_tokens->{_rtoken_type};
    my $rtokens     = $line_of_tokens->{_rtokens};

    if ( $line_type eq 'HERE' ) {
        if ($want_here_text) {
            store_text($input_line);
        }
        return;
    }
    if ( $line_type eq 'POD' ) {
        if ($want_pod) {
            store_text($input_line);
        }
        return;
    }
    if ( $line_type ne 'CODE' ) {
        return;
    }

    # Scan CODE for quotes and comments
    chomp $input_line;

    # loop over tokens to construct one masked line
    for ( my $j = 0 ; $j < @$rtoken_type ; $j++ ) {

        # Mask certain token types by replacing them with their type code:
        # type  definition
        # ----  ----------
        # Q     quote or pattern
        # h     << here doc operator
        # #     comment
        #
        if ( $$rtoken_type[$j] eq 'Q' ) {
            if ($want_quotes) {
                store_text( $rtokens->[$j] );
            }
        }

        # Mask a comment
        elsif ( $$rtoken_type[$j] eq '#' ) {
            if ($want_comments) {
                store_text( $rtokens->[$j] );
            }
        }

        #
        else {
            next;
        }
    }
    return;
} ## end sub write_line

#!/usr/bin/perl -w
use strict;
use warnings;
use Data::Dumper;

# This is one of a set of programs for doing random testing of perltidy.  The
# goal is to try to crash perltidy.  These programs have been very helpful in
# finding subtle bugs but they are a work in progress and continually evolving.
# The programs are:

#   random_file_generator.pl  [optional first step]
#   perltidy_random_setup.pl  [this file]
#   perltidy_random_run.pl    [next step]

# This program is interactive and helps setup the files.  It writes a config
# file and a run script for the next program which actually does the runs.

# You should create a temporary directory for this work.

our $rsetup;    # the setup hash
my $config_file   = "config.txt";
my $FILES_file    = "FILES.txt";
my $PROFILES_file = "PROFILES.txt";
my $perltidy      = "./perltidy.pl";
my $rfiles        = [];
my $rprofiles     = [];

# if file 'perltidy.pl' is found here then make that the default
if ( -e './perltidy.pl' ) { $perltidy = './perltidy.pl' }

# always require a separate version of perltidy
# go get a copy if there is none:
# On my system I have a utility 'get_perltidy.pl' which gets the latest
# perltidy.pl with DEVEL_MODE => 1 everywhere
else {
    print STDERR "Attempting to get perltidy.pl in DEVEL_MODE...\n";
    my $fail = system("get_perltidy.pl");
    if ($fail) {
        die "..Failed. Please move a copy of perltidy.pl here first\n";
    }
}

# see if DEVEL_MODE is set, turn it on if not
if ( $perltidy eq "./perltidy.pl" ) {
    check_DEVEL_MODE($perltidy);
}

query(<<EOM);

IMPORTANT: You should start this program in an empty directory that you create
specifically for this test.  After testing you will probably want to delete the
entire directory.  It is useful to create this empty directory just below a
directory full of actual perl scripts which can be read as test input.

You may want to put a special copy of perltidy in this directory for testing,
probably setting all constants DEVEL_MODE => 1.  (You can make this with
the pm2pl script).

If you want to test on random files, you should generate them first in this
directory with 'random_file_generator.pl'.  That is currently a separate
program but will eventually be incorporated into this program.

Hit <cr> to continue, or hit control-C to quit.

EOM

# Defaults
default_config();

if ( -e $config_file ) {
    if ( ifyes( "Read the existing config.txt file? [Y/N]", "Y" ) ) {
        read_config($config_file);
    }
}

if ( -e $FILES_file ) {
    if ( ifyes( "Found $FILES_file, read it ? [Y/N]", "Y" ) ) {
        $rfiles = read_list($FILES_file);
        my $nfiles = @{$rfiles};
        print STDOUT "found $nfiles files\n";
    }
}

if ( !@{$rfiles} ) {
    $rfiles = define_new_files();
}

if ( -e $PROFILES_file ) {
    if ( ifyes( "Found $PROFILES_file, read it ? [Y/N]", "Y" ) ) {
        $rprofiles = read_list($PROFILES_file);
        my $nfiles = @{$rprofiles};
        print STDOUT "found $nfiles profiles\n";
    }
}

if ( !@{$rprofiles} ) {
    make_profiles();
    $rprofiles = filter_profiles($rprofiles);
}

$rsetup->{'syntax_check'} = ifyes( <<EOM, "N" );
Do you want to check syntax with perl -c ?
This will cause any BEGIN blocks in them to execute, which
can introduce a security concern.
Enter 'N' unless you very familiar with the test scripts.
Y/N:
EOM

my $file_info    = get_file_info($rfiles);
my $profile_info = get_profile_info();
my $nprofiles    = @{$rprofiles};
while (1) {
    my $files              = $rsetup->{files};
    my $chain_mode         = $rsetup->{chain_mode};
    my $append_flags       = $rsetup->{append_flags};
    my $do_syntax_check    = $rsetup->{syntax_check};
    my $delete_good_output = $rsetup->{delete_good_output};
    my $perltidy_version   = $rsetup->{perltidy};
    $perltidy_version = "[default]" unless ($perltidy_version);
    print <<EOM;
===Main Menu===
R   - Read a config file
      Files:    $files
$file_info
FR  - Replace Files: replace with all new files
FA  - Add Files: add to or modify current set of files
P   - Profiles: 
$profile_info
C   - Chain mode               : $chain_mode
D   - Delete good output?      : $delete_good_output
S   - Syntax check?            : $do_syntax_check
A   - Append flags             : $append_flags
V   - perltidy Version         : $perltidy_version
Q   - Quit without saving config file
W   - Write config, FILES.txt, PROFILES.txt, GO.sh and eXit
EOM

    my ($ans) = queryu(':');
    if ( $ans eq 'R' ) {
        my $infile = get_input_filename( '', '.txt', $config_file );
        read_config($infile);
    }
    elsif ( $ans eq 'E' ) {
        edit_config();
    }
    elsif ( $ans eq 'FR' ) {
        $rfiles    = define_new_files();
        $rfiles    = filter_files($rfiles);
        $file_info = get_file_info($rfiles);
    }
    elsif ( $ans eq 'FA' ) {
        $rfiles    = add_files($rfiles);
        $file_info = get_file_info($rfiles);
    }
    elsif ( $ans eq 'P' ) {
        make_profiles();
        $rprofiles    = filter_profiles($rprofiles);
        $profile_info = get_profile_info();
    }
    elsif ( $ans eq 'C' ) {
        $chain_mode = get_num("Chaining: 0=no, 1=always,2=random");
        $rsetup->{chain_mode} = $chain_mode;
    }
    elsif ( $ans eq 'A' ) {
        my $str = query("Enter any flags to append");
        $rsetup->{append_flags} = $str;
    }
    elsif ( $ans eq 'D' ) {
        $delete_good_output =
          ifyes( "Delete needless good output files? [Y/N]", "Y" );
        $rsetup->{delete_good_output} = $delete_good_output;
    }
    elsif ( $ans eq 'S' ) {
        $do_syntax_check = ifyes( "Do syntax checking? [Y/N]", "N" );
        $rsetup->{syntax_check} = $do_syntax_check;
    }
    elsif ( $ans eq 'V' ) {
        my $test =
          query(
            "Enter the full path to the perltidy binary, or <cr> for default");
        if ( $test && !-e $test ) {
            next
              unless (
                ifyes("I cannot find that, do you want to use it anyway?") );
        }
        $rsetup->{perltidy} = $test;
    }
    elsif ( $ans eq 'Q' ) {
        last if ( ifyes("Quit without saving? [Y/N]") );
    }
    elsif ( $ans eq 'W' || $ans eq 'X' ) {
        write_config($config_file);
        $rfiles    = filter_files($rfiles);
        $rprofiles = filter_profiles($rprofiles);
        write_list( $FILES_file,    $rfiles );
        write_list( $PROFILES_file, $rprofiles );
        last;
    }
}

write_GO();

sub filter_files {
    my ($rlist) = @_;

    # keep only a unique set
    $rlist = uniq($rlist);

    # only work on regular files with non-zero length
    @{$rlist} = grep { -f $_ && !-z $_ } @{$rlist};

    # and text files
    @{$rlist} = grep { -T $_ } @{$rlist};

    # Ignore .tdy and related files
    @{$rlist} = grep { $_ !~ /\.DEBUG$/ } @{$rlist};
    @{$rlist} = grep { $_ !~ /\.ERR$/ } @{$rlist};
    @{$rlist} = grep { $_ !~ /\.LOG$/ } @{$rlist};
    @{$rlist} = grep { $_ !~ /\bDIAGNOSTICS$/ } @{$rlist};

    # exclude pro{$rlist}
    @{$rlist} = grep { $_ !~ /profile\.[0-9]*/ } @{$rlist};

    # Sort by size
    @{$rlist} =
      map  { $_->[0] }
      sort { $a->[1] <=> $b->[1] }
      map  { [ $_, -e $_ ? -s $_ : 0 ] } @{$rlist};

    return $rlist;
}

sub filter_profiles {
    my ($rlist) = @_;

    # keep only a unique set
    $rlist = uniq($rlist);

    # only work on regular files with non-zero length
    @{$rlist} = grep { -f $_ && !-z $_ } @{$rlist};

    # Sort on numerical extension
    @{$rlist} =
      map  { $_->[0] . "." . $_->[1] }              # basename.extension
      sort { $a->[1] <=> $b->[1] }                  # sort on extension
      map  { [ ( split /\./, $_ ) ] } @{$rlist};    # split into [base,ext]

    return $rlist;
}

sub uniq {
    my ($rlist) = @_;
    my %seen    = ();
    my @uniqu   = grep { !$seen{$_}++ } @{$rlist};
    return \@uniqu;
}

sub define_new_files {

    print <<EOM;
====== Define some files to process =================================
Note that you can generate random files with 'random_file_generator.pl'
If you want to do that, you should exit now, generate them, then come
back.
EOM

    my $rnew_files = [];
    my $glob       = '../*';

  REDO:
    my $ans = query("File glob to get some NEW files to process, <cr>='$glob'");
    $glob = $ans if ($ans);
    return $rnew_files unless ($glob);
    my @files = glob($glob);
    @files      = grep { -f $_ && !-z $_ } @files;
    @files      = grep { $_ !~ /\.tdy$/ } @files;
    @files      = grep { $_ !~ /profile\.[0-9]*/ } @files;
    $rnew_files = uniq( \@files );
    $rnew_files = filter_files($rnew_files);

    while (1) {
        my $nfiles_new    = @{$rnew_files};
        my $total_size_mb = file_size_sum_mb($rnew_files);
        my $file_info     = get_file_info($rnew_files);
        my $ans           = queryu(<<EOM);
$file_info
R  Redo with a different glob
F  Reduce total size by a fraction
Y  Yes .. use these files
EOM
        if ( $ans eq 'R' ) { goto REDO }
        if ( $ans eq 'Y' ) { last }
        if ( $ans eq 'F' ) {
            my $fraction =
              get_num( "Enter a fraction of current size (0-1):", 1 );
            if ( $fraction > 0 && $fraction < 1 ) {
                $rnew_files = reduce_total_file_size( $rnew_files, $fraction );
            }
        }
    }
    $rnew_files = [ sort @{$rnew_files} ];
    return $rnew_files;
}

sub add_files {

    my ($rold_files) = @_;
    if ( !@{$rold_files} ) { return define_new_files() }

    my $rnew_files = [ @{$rold_files} ];

    while (1) {
        my $file_info = get_file_info($rnew_files);
        print <<EOM;
$file_info
G   use Glob to find new files
F   Reduce total size to a fraction
Y   Yes ... use these
Q   Quit: revert to the old set and exit
EOM
        my $ans = queryu(':');
        if ( $ans eq 'G' ) {
            my $glob = '../*';
            my $ans =
              query("File glob to get some NEW files to process, <cr>='$glob'");
            $glob = $ans if ($ans);
            next unless ($glob);
            my @files = glob($glob);
            @files = grep { -f $_ && !-z $_ } @files;
            my $rf = filter_files( \@files );
            my $nf = @{$rf};
            if ( !$nf ) { query("no useful files found; hit <cr>"); next }

            if ( ifyes("Found $nf files with glob; merge them in ? [Y/N]") ) {
                push @{$rnew_files}, @{$rf};
                $rnew_files = uniq($rnew_files);
            }
        }
        elsif ( $ans eq 'F' ) {
            my $fraction =
              get_num( "Enter a fraction of current size (0-1):", 1 );
            if ( $fraction > 0 && $fraction < 1 ) {
                $rnew_files = reduce_total_file_size( $rnew_files, $fraction );
            }
        }
        elsif ( $ans eq 'Y' ) { $rnew_files = $rold_files; last }
        elsif ( $ans eq 'Q' ) { last }
    }
    $rnew_files = [ sort @{$rnew_files} ];
    return $rnew_files;
}

sub file_size_sum_mb {
    my ($rfiles) = @_;
    my $sum_mb = 0;
    foreach ( @{$rfiles} ) {
        my $size_in_mb = ( -s $_ ) / ( 1024 * 1024 );
        $sum_mb += $size_in_mb;
    }
    return $sum_mb;
}

sub reduce_total_file_size {
    my ( $rfiles, $fraction ) = @_;
    my @files =
      map  { $_->[0] }
      sort { $a->[1] <=> $b->[1] }
      map  { [ $_, -e $_ ? -s $_ : 0 ] } @{$rfiles};
    my $sum = 0;
    my @partial_sum;
    foreach (@files) {
        $sum += -s $_;
        push @partial_sum, [ $_, $sum ];
    }
    my $want = $sum * $fraction;
    my @new_files;
    foreach (@partial_sum) {
        my ( $fname, $sum ) = @{$_};
        last if ( $sum > $want );
        push @new_files, $fname;
    }
    return \@new_files;
}

sub get_profile_info {

    my $nprofiles = @{$rprofiles};
    my $profile0  = "(none)";
    my $profileN  = "(none)";
    if ($nprofiles) {
        $profile0 = $rprofiles->[0];
        $profileN = $rprofiles->[-1];
    }
    my $profile_info = <<EOM;
    Number of Files: $nprofiles
    First profile     : $profile0
    Last profile      : $profileN
EOM
    return $profile_info;
}

sub get_file_info {
    my ($rf) = @_;

    my $nfiles = @{$rf};
    my $file0  = "(none)";
    my $fileN  = "(none)";
    if ($nfiles) {
        $file0 = $rf->[0];
        $fileN = $rf->[-1];
    }
    my $total_size_mb = file_size_sum_mb($rf);
    my $file_info     = <<EOM;
    Number of Files: $nfiles
    Total Size, Mb : $total_size_mb
    First file     : $file0
    Last file      : $fileN
EOM
    return $file_info;
}

sub default_config {
    $rsetup = {
        chain_mode         => 2,
        delete_good_output => 1,
        syntax_check       => 0,
        profiles           => $PROFILES_file,
        files              => $FILES_file,
        perltidy           => $perltidy,
        append_flags       => "",
    };
    return;
}

sub write_GO {

    my $runme = "GO.sh";

    if ( -e $runme ) {
        my $bak = "$runme.bak";
        if ( -e $bak ) { unlink $bak }
        system("mv $runme $bak");
    }

    my $fh;
    open( $fh, '>', $runme ) || die "cannot open $runme: $!\n";
    $fh->print(<<'EOM');
#!/bin/sh

# This script can run perltidy with random parameters
# usage: perltidy_random.sh file1 file2 ... N
# where N is the number of random cases
echo "Perltidy random run ..."
echo "NOTE: Create a file named 'stop.now' to force an early exit"
sleep 2
rm nohup.my
unlink $0;
nohup nice -n19 perltidy_random_run.pl >>nohup.my 2>>nohup.my
EOM
    system("chmod +x $runme");
    print STDOUT "Edit $config_file if you want to make any changes\n";
    print STDOUT "then enter ./$runme\n";
}

sub write_config {
    my ($ofile) = @_;
    my $hash = Data::Dumper->Dump( [$rsetup], ["rsetup"] );
    my $fh;
    if ( !open( $fh, '>', $ofile, ) ) {
        print "cannot open $ofile :$!\n";
        return;
    }
    $fh->print("$hash\n");
    $fh->close();
    return;
}

sub read_config {

    my ($ifile) = @_;
    $rsetup = undef;
    do $ifile;

    # be sure the file has correct perl syntax
    my $syntax_check = qx/perl -cw $ifile 2>&1/;
    if ( $syntax_check !~ /syntax OK/ ) {
        print STDERR <<EOM;
-----------------------------------
$syntax_check
-----------------------------------
The above syntax errors are in File '$ifile'
EOM
        die;
    }

    # read the config file
    do $ifile;

    return;
}

sub read_list {
    my ($fname) = @_;
    my $rlist;

    # read a single column list of files
    # remove blank lines and comments
    my $fh;
    if ( !open( $fh, "<", $fname ) ) {
        query("Cannot open $fname: $!\n");
        return $rlist;
    }
    while ( my $line = <$fh> ) {
        $line         =~ s/^\s+//;
        $line         =~ s/\s+$//;
        next if $line =~ /^#/;
        push @{$rlist}, $line;
    }
    $fh->close();
    return $rlist;
}

sub write_list {
    my ( $fname, $rlist ) = @_;

    my $fh;
    if ( !open( $fh, ">", $fname ) ) {
        query("Cannot open $fname: $!\n");
        return;
    }
    foreach my $line ( @{$rlist} ) {
        chomp $line;
        $line .= "\n";
        $fh->print($line);
    }
    $fh->close();
    return;
}

sub query {
    my ($msg) = @_;
    print $msg;
    my $ans = <STDIN>;
    chomp $ans;
    return $ans;
}

sub queryu {
    return uc query(@_);
}

sub ifyes {

    # Updated to have default, which should be "Y" or "N"
    my ( $msg, $default ) = @_;
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
}

sub get_output_filename {
    my ( $msg, $default ) = @_;
    $msg = "Enter filename to write" unless $msg;
  RETRY:
    my $filename;
    if ($default) {
        $filename = query("$msg, <cr>='$default': ");
        $filename = $default if ( !$filename || $filename =~ /^\s*$/ );
    }
    else {
        $filename = query("$msg:");
    }
    if ( -e $filename ) {
        goto RETRY
          unless ( ifyes("file '$filename' exists; Overwrite? [Y/N]") );
    }
    return $filename;
}

sub get_input_filename {
    my ( $msg, $ext, $default ) = @_;
    $msg = "Enter filename to read" unless $msg;
  RETRY:
    my $filename;
    if ($default) {
        $filename = query("$msg, <cr>='$default': ");
        $filename = $default if ( !$filename || $filename =~ /^\s*$/ );
    }
    else {
        $filename = query("$msg:");
    }
    unless ( -e $filename ) {
        return undef if ( $filename eq '.' || $filename eq "" );
        if     ( $filename !~ /\..*/ ) { $filename .= "$ext"; }
        unless ( -e $filename ) {
            print STDERR "$filename does not exist\n";
            goto RETRY if ( ifyes("Try again? [Y/N]") );
            return undef;
        }
    }
    return $filename;
}

sub get_num {
    my ( $msg, $default ) = @_;
    if ( defined($default) ) {
        $msg =~ s/:$//;
        $msg .= " (<cr>=$default):";
    }
    my $ans = query($msg);
    $ans = $default if ( defined($default) && $ans eq "" );
    my $val = eval($ans);
    if ($@) { warn $@; $val = $ans; }
    return $val;
}

{    # make_profiles

    # This will generate N random profiles for perltidy

    # usage:
    #   make_profiles(20)
    #   - to make 20 random profiles

    my @parameters;

    sub get_parameters {

        # get latest parameters from perltidy
        use File::Temp qw(tempfile);
        my ( $fout, $tmpnam ) = File::Temp::tempfile();
        if ( !$fout ) { die "cannot get tempfile\n" }
        my @parameters;
        system "perltidy --dump-long-names >$tmpnam";
        open( IN, "<", $tmpnam ) || die "cannot open $tmpnam: $!\n";
        while ( my $line = <IN> ) {
            next if $line =~ /#/;
            chomp $line;
            push @parameters, $line;
        }
        close IN;
        unlink $tmpnam if ( -e $tmpnam );
        return \@parameters;
    }

    BEGIN {

        # Here is a static list of all parameters current as of v.20200907
        # Created with perltidy --dump-long-names
        # Command line long names (passed to GetOptions)
        #---------------------------------------------------------------
        # here is a summary of the Getopt codes:
        # <none> does not take an argument
        # =s takes a mandatory string
        # :s takes an optional string
        # =i takes a mandatory integer
        # :i takes an optional integer
        # ! does not take an argument and may be negated
        #  i.e., -foo and -nofoo are allowed
        # a double dash signals the end of the options list
        #
        #---------------------------------------------------------------
        @parameters = qw(
          DEBUG!
          add-newlines!
          add-semicolons!
          add-whitespace!
          assert-tidy!
          assert-untidy!
          backlink=s
          backup-and-modify-in-place!
          backup-file-extension=s
          blank-lines-after-opening-block-list=s
          blank-lines-after-opening-block=i
          blank-lines-before-closing-block-list=s
          blank-lines-before-closing-block=i
          blank-lines-before-packages=i
          blank-lines-before-subs=i
          blanks-before-blocks!
          blanks-before-comments!
          block-brace-tightness=i
          block-brace-vertical-tightness-list=s
          block-brace-vertical-tightness=i
          brace-left-and-indent!
          brace-left-and-indent-list=s
          brace-tightness=i
          brace-vertical-tightness-closing=i
          brace-vertical-tightness=i
          break-after-all-operators!
          break-at-old-attribute-breakpoints!
          break-at-old-comma-breakpoints!
          break-at-old-keyword-breakpoints!
          break-at-old-logical-breakpoints!
          break-at-old-method-breakpoints!
          break-at-old-semicolon-breakpoints!
          break-at-old-ternary-breakpoints!
          break-before-all-operators!
          cachedir=s
          character-encoding=s
          check-syntax!
          closing-brace-indentation=i
          closing-paren-indentation=i
          closing-side-comment-else-flag=i
          closing-side-comment-interval=i
          closing-side-comment-list=s
          closing-side-comment-maximum-text=i
          closing-side-comment-prefix=s
          closing-side-comment-warnings!
          closing-side-comments!
          closing-side-comments-balanced!
          closing-square-bracket-indentation=i
          closing-token-indentation=i
          comma-arrow-breakpoints=i
          continuation-indentation=i
          cuddled-block-list-exclusive!
          cuddled-block-list=s
          cuddled-break-option=i
          cuddled-else!
          default-tabsize=i
          delete-block-comments!
          delete-closing-side-comments!
          delete-old-newlines!
          delete-old-whitespace!
          delete-pod!
          delete-semicolons!
          delete-side-comments!
          dump-cuddled-block-list!
          dump-defaults!
          dump-long-names!
          dump-options!
          dump-profile!
          dump-short-names!
          dump-token-types!
          dump-want-left-space!
          dump-want-right-space!
          entab-leading-whitespace=i
          extended-syntax!
          file-size-order!
          fixed-position-side-comment=i
          force-read-binary!
          format-skipping!
          format-skipping-begin=s
          format-skipping-end=s
          format=s
          frames!
          fuzzy-line-length!
          hanging-side-comments!
          help
          html!
          html-bold-bareword!
          html-bold-colon!
          html-bold-comma!
          html-bold-comment!
          html-bold-here-doc-target!
          html-bold-here-doc-text!
          html-bold-identifier!
          html-bold-keyword!
          html-bold-label!
          html-bold-numeric!
          html-bold-paren!
          html-bold-pod-text!
          html-bold-punctuation!
          html-bold-quote!
          html-bold-semicolon!
          html-bold-structure!
          html-bold-subroutine!
          html-bold-v-string!
          html-color-background=s
          html-color-bareword=s
          html-color-colon=s
          html-color-comma=s
          html-color-comment=s
          html-color-here-doc-target=s
          html-color-here-doc-text=s
          html-color-identifier=s
          html-color-keyword=s
          html-color-label=s
          html-color-numeric=s
          html-color-paren=s
          html-color-pod-text=s
          html-color-punctuation=s
          html-color-quote=s
          html-color-semicolon=s
          html-color-structure=s
          html-color-subroutine=s
          html-color-v-string=s
          html-entities!
          html-italic-bareword!
          html-italic-colon!
          html-italic-comma!
          html-italic-comment!
          html-italic-here-doc-target!
          html-italic-here-doc-text!
          html-italic-identifier!
          html-italic-keyword!
          html-italic-label!
          html-italic-numeric!
          html-italic-paren!
          html-italic-pod-text!
          html-italic-punctuation!
          html-italic-quote!
          html-italic-semicolon!
          html-italic-structure!
          html-italic-subroutine!
          html-italic-v-string!
          html-line-numbers
          html-linked-style-sheet=s
          html-pre-only
          html-src-extension=s
          html-table-of-contents!
          html-toc-extension=s
          htmlroot=s
          ignore-old-breakpoints!
          ignore-side-comment-lengths!
          indent-block-comments!
          indent-closing-brace!
          indent-columns=i
          indent-spaced-block-comments!
          iterations=i
          keep-interior-semicolons!
          keep-old-blank-lines=i
          keyword-group-blanks-after=i
          keyword-group-blanks-before=i
          keyword-group-blanks-delete!
          keyword-group-blanks-inside!
          keyword-group-blanks-list=s
          keyword-group-blanks-repeat-count=i
          keyword-group-blanks-size=s
          keyword-paren-inner-tightness-list=s
          keyword-paren-inner-tightness=i
          libpods=s
          line-up-parentheses!
          logfile!
          logfile-gap:i
          logical-padding!
          long-block-line-count=i
          look-for-autoloader!
          look-for-hash-bang!
          look-for-selfloader!
          maximum-consecutive-blank-lines=i
          maximum-fields-per-table=i
          maximum-line-length=i
          memoize!
          minimum-space-to-comment=i
          no-profile
          nohtml-style-sheets
          non-indenting-brace-prefix=s
          non-indenting-braces!
          noprofile
          nospace-after-keyword=s
          notidy
          nowant-left-space=s
          nowant-right-space=s
          npro
          one-line-block-nesting=i
          one-line-block-semicolons=i
          opening-anonymous-sub-brace-on-new-line!
          opening-brace-always-on-right!
          opening-brace-on-new-line!
          opening-hash-brace-right!
          opening-paren-right!
          opening-square-bracket-right!
          opening-sub-brace-on-new-line!
          outdent-keyword-list=s
          outdent-keywords!
          outdent-labels!
          outdent-long-comments!
          outdent-long-quotes!
          outdent-static-block-comments!
          outfile=s
          output-file-extension=s
          output-line-ending=s
          output-path=s
          paren-tightness=i
          paren-vertical-tightness-closing=i
          paren-vertical-tightness=i
          pass-version-line!
          perl-syntax-check-flags=s
          pod2html!
          podflush
          podheader!
          podindex!
          podpath=s
          podquiet!
          podrecurse!
          podroot=s
          podverbose!
          preserve-line-endings!
          profile=s
          quiet!
          recombine!
          short-concatenation-item-length=i
          show-options!
          space-after-keyword=s
          space-backslash-quote=i
          space-for-semicolon!
          space-function-paren!
          space-keyword-paren!
          space-prototype-paren=i
          space-terminal-semicolon!
          square-bracket-tightness=i
          square-bracket-vertical-tightness-closing=i
          square-bracket-vertical-tightness=i
          stack-closing-block-brace!
          stack-closing-hash-brace!
          stack-closing-paren!
          stack-closing-square-bracket!
          stack-opening-block-brace!
          stack-opening-hash-brace!
          stack-opening-paren!
          stack-opening-square-bracket!
          standard-error-output!
          standard-output!
          starting-indentation-level=i
          static-block-comment-prefix=s
          static-block-comments!
          static-side-comment-prefix=s
          static-side-comments!
          stylesheet
          sub-alias-list=s
          tabs!
          tee-block-comments!
          tee-pod!
          tee-side-comments!
          tight-secret-operators!
          timestamp!
          title=s
          trim-pod!
          trim-qw!
          use-unicode-gcstring!
          valign!
          variable-maximum-line-length!
          version
          vertical-tightness-closing=i
          vertical-tightness=i
          want-break-after=s
          want-break-before=s
          want-left-space=s
          want-right-space=s
          warning-output!
          weld-nested-containers!
          whitespace-cycle=i
        );

        # We can use the above list, but
        # normally we want to update to the latest parameters
        my $UPDATE_PARAMETERS = 1;

        if ($UPDATE_PARAMETERS) {
            my $rparameters_current = get_parameters();
            @parameters = @{$rparameters_current};
            print STDERR "Updating perltidy parameters....\n";
        }
    }

    sub make_profiles {
        my $nfiles_old = @{$rprofiles};
        my $case       = 0;
        if ( $nfiles_old > 0 ) {
            my $profile_info = get_profile_info();
            print $profile_info;
            print "There are already $nfiles_old existing files";
            while (1) {
                my $ans = queryu(<<EOM);
A   Add new files to existing profiles
D   Delete some files ...
R   Replace all existing files with new files
X   eXit, keeping existing profiles as is
EOM
                if    ( $ans eq 'X' ) { return }
                elsif ( $ans eq 'A' ) {
                    foreach my $fname ( @{$rprofiles} ) {
                        if ( $fname =~ /\.([\d]+$)/ ) {
                            if ( $1 > $case ) { $case = $1 }
                        }
                    }
                    last;
                }
                elsif ( $ans eq 'D' ) {
                    my $num = get_num( "Number to keep:", $nfiles_old );
                    if ( $num > $nfiles_old || $num <= 0 ) {
                        query("Sorry, must keep 0 to $nfiles_old, hit <cr>");
                    }
                    else {
                        @{$rprofiles} = @{$rprofiles}[ 0 .. $num - 1 ];
                        return;
                    }
                }
                elsif ( $ans eq 'R' ) { @{$rprofiles} = []; last }
            }
        }
        my $max_cases =
          get_num( "Number of new random profiles to generate", 10000 );
        for ( 1 .. $max_cases ) {
            $case += 1;
            my $profile = "profile.$case";

            # Make the profile

            # use default parameters on first case. That way we can check
            # if a file produces an error output
            my $rrandom_parameters;
            if ( $case == 1 ) {
                $rrandom_parameters = [""];
            }

            # Case 2 creates the smallest possible output file size
            if ( $case == 2 ) {
                $rrandom_parameters = ["-dsm -dac -i=0 -ci=0 -it=2 -mbl=0"];
            }

            # Case 3 checks extrude from mangle (case 2)
            if ( $case == 3 ) {
                $rrandom_parameters = ["--extrude"];
            }

            # Case 4 checks mangle again from extrude (
            if ( $case == 4 ) {
                $rrandom_parameters = ["--mangle"];
            }

            # From then on random parameters are generated
            if ( $case > 5 ) {
                $rrandom_parameters = get_random_parameters();
            }
            my $fh;
            open( $fh, ">", $profile ) || die "cannot open $profile: $!\n";
            foreach ( @{$rrandom_parameters} ) {
                $fh->print("$_\n");
            }
            $fh->close();
            push @{$rprofiles}, $profile;
        }
    }

    sub get_random_parameters {

        # return a set of random parameters for perltidy
        my @random_parameters;

        my %flag_types = (
            '!'  => 'BINARY FLAG',
            '=s' => 'STRING',
            '=i' => 'INTEGER',
            ':i' => 'OPTIONAL INTEGER',
            ':s' => 'OPTIONAL STRING',
        );

        my @random_words = qw(bannanas sub subaru train 1x =+ !);

        my @token_types = qw#
          A b C G L R f h Q k t w i q n p m F pp mm U j J Y Z v { } ( ) [ ] ; +
          - / * | % ! x ~ = \ ? : . < > ^ & .. :: << >> ** && .. || // -> => +=
          -= .= %= &= |= ^= *= <> <= >= == =~ !~ != ++ -- /= x= ~~ ~. |. &. ^.
          ... **= <<= >>= &&= ||= //= <=> !~~ &.= |.= ^.= <<~ <<>> CORE::
          #;
        push @token_types, '#';
        push @token_types, ',';

        my @valign_list = qw#
          = **= += *= &= <<= &&= -= /= |= >>= ||= //= .= %= ^= x=
          { ( ? : , ; => && || ~~ !~~ =~ !~ // <=> ->
          if unless and or err for foreach while until
          #;

        my @valign_exclude_all = qw( * * );

        my @operators =
          qw(% + - * / x != == >= <= =~ !~ < > | & = **= += *= &= <<= &&= -= /= |= >>= ||= //= .= %= ^= x=);
        my @keywords = qw(my our local do while if  garbage1 34 );
        my @colors   = qw(
          ForestGreen
          SaddleBrown
          magenta4
          IndianRed3
          DeepSkyBlue4
          MediumOrchid3
          black
          white
          red

          bubba
        );

        my %option_range = (
            'format'             => [ 'tidy', 'html' ],    #, 'user' ],
            'output-line-ending' => [ 'dos',  'win', 'mac', 'unix' ],

            'space-backslash-quote'         => [ 0, 2 ],
            'block-brace-tightness'         => [ 0, 2 ],
            'keyword-paren-inner-tightness' => [ 0, 2 ],
            'brace-tightness'               => [ 0, 2 ],
            'paren-tightness'               => [ 0, 2 ],
            'square-bracket-tightness'      => [ 0, 2 ],

            'block-brace-vertical-tightness'            => [ 0, 2 ],
            'brace-vertical-tightness'                  => [ 0, 2 ],
            'brace-vertical-tightness-closing'          => [ 0, 2 ],
            'paren-vertical-tightness'                  => [ 0, 2 ],
            'paren-vertical-tightness-closing'          => [ 0, 2 ],
            'square-bracket-vertical-tightness'         => [ 0, 2 ],
            'square-bracket-vertical-tightness-closing' => [ 0, 2 ],
            'vertical-tightness'                        => [ 0, 2 ],
            'vertical-tightness-closing'                => [ 0, 2 ],

            'break-before-hash-brace'                => [ 0, 3 ],
            'break-before-square-bracket'            => [ 0, 3 ],
            'break-before-paren'                     => [ 0, 3 ],
            'break-before-hash-brace-and-indent'     => [ 0, 2 ],
            'break-before-square-bracket-and-indent' => [ 0, 2 ],
            'break-before-paren-and-indent'          => [ 0, 2 ],

            'closing-brace-indentation'          => [ 0, 3 ],
            'closing-paren-indentation'          => [ 0, 3 ],
            'closing-square-bracket-indentation' => [ 0, 3 ],
            'closing-token-indentation'          => [ 0, 3 ],

            'closing-side-comment-else-flag' => [ 0, 2 ],
            'comma-arrow-breakpoints'        => [ 0, 5 ],

            'keyword-group-blanks-before' => [ 0, 2 ],
            'keyword-group-blanks-after'  => [ 0, 2 ],

            'space-prototype-paren' => [ 0, 2 ],
            'break-after-labels'    => [ 0, 2 ],

            # Arbitrary limits to keep things readable
            'blank-lines-after-opening-block'  => [ 0, 4 ],
            'blank-lines-before-closing-block' => [ 0, 3 ],
            'blank-lines-before-packages'      => [ 0, 3 ],
            'blank-lines-before-subs'          => [ 0, 3 ],

            'maximum-consecutive-blank-lines' => [ 0, 4 ],
            'minimum-space-to-comment'        => [ 0, 10 ],

            'indent-columns'           => [ 0, 10 ],
            'continuation-indentation' => [ 0, 10 ],
            'default-tabsize'          => [ 0, 8 ],
            'entab-leading-whitespace' => [ 0, 8 ],

            # always iterate
            'iterations' => [ 6, 10 ],

            'want-break-after'   => \@operators,
            'want-break-before'  => \@operators,
            'want-left-space'    => \@operators,
            'want-right-space'   => \@operators,
            'nowant-left-space'  => \@operators,
            'nowant-right-space' => \@operators,

            'keep-old-breakpoints-after'  => \@token_types,
            'keep-old-breakpoints-before' => \@token_types,

            #'keyword-group-blanks-list=s
            'keyword-group-blanks-size' => [ 0, 2, 4, 7, 10, 2.8, 1.8 ],

            # TODO: FILL thESE with multiple random keywords
            'space-after-keyword'   => \@keywords,
            'nospace-after-keyword' => \@keywords,

            'valign-exclusion-list' => \@valign_exclude_all,
            'valign-inclusion-list' => \@valign_list,

            'html-color-background'      => \@colors,
            'html-color-bareword'        => \@colors,
            'html-color-colon'           => \@colors,
            'html-color-comma'           => \@colors,
            'html-color-comment'         => \@colors,
            'html-color-here-doc-target' => \@colors,
            'html-color-here-doc-text'   => \@colors,
            'html-color-identifier'      => \@colors,
            'html-color-keyword'         => \@colors,
            'html-color-label'           => \@colors,
            'html-color-numeric'         => \@colors,
            'html-color-paren'           => \@colors,
            'html-color-pod-text'        => \@colors,
            'html-color-punctuation'     => \@colors,
            'html-color-quote'           => \@colors,
            'html-color-semicolon'       => \@colors,
            'html-color-structure'       => \@colors,
            'html-color-subroutine'      => \@colors,
            'html-color-v-string'        => \@colors,
        );

        my %is_multiword_list = (
            'want-break-after'            => 1,
            'want-break-before'           => 1,
            'want-left-space'             => 1,
            'want-right-space'            => 1,
            'nowant-left-space'           => 1,
            'nowant-right-space'          => 1,
            'space-after-keyword'         => 1,
            'nospace-after-keyword'       => 1,
            'keep-old-breakpoints-after'  => 1,
            'keep-old-breakpoints-before' => 1,
            'valign-exclusion-list'       => 0,
            'valign-inclusion-list'       => 1,
        );

        ###################################################################
        # Most of these have been tested and are best skipped because
        # they produce unwanted output or perhaps cause the program to
        # just quit early.  Parameters can be added and removed from the
        # list to customize testing.  'format' was added because html is
        # not so interesting, but can be removed for html testing.
        ###################################################################
        my @q = qw(
          DEBUG
          assert-tidy
          assert-untidy
          backup-and-modify-in-place
          backup-file-extension
          backup-method
          character-encoding
          dump-cuddled-block-list
          dump-defaults
          dump-long-names
          dump-options
          dump-profile
          dump-short-names
          dump-token-types
          dump-want-left-space
          dump-want-right-space
          format
          format-skipping-begin
          format-skipping-end
          help
          html
          logfile
          logfile-gap
          look-for-hash-bang
          maximum-file-size-mb
          notidy
          outfile
          output-file-extension
          output-file-extension
          output-line-ending
          output-path
          quiet
          standard-error-output
          standard-output
          starting-indentation-level
          tee-block-comments
          tee-pod
          tee-side-comments
          version
          delete-pod
          tabs
          entab-leading-whitespace
          recombine
          code-skipping-begin
          code-skipping-end
        );

        my %skip;
        @skip{@q} = (1) x scalar(@q);

        foreach my $parameter (@parameters) {
            my ( $name, $flag, $type ) = ( "", "", "" );
            $parameter =~ s/\s+$//;
            if ( $parameter =~ /^([\w\-]+)([^\s]*)/ ) {
                $name = $1;
                $flag = $2;
                $flag = "" unless $flag;
                $type = $flag_types{$flag} if ($flag);

                next if $skip{$name};

                # Skip all pattern lists
                if ( $flag =~ /s$/ ) {
                    if (   $name =~ /-(list|prefix)/
                        || $name =~ /character-encoding/ )
                    {
                        next unless ( $name =~ /^valign-/ );
                    }
                }
                my $rrange = $option_range{$name};
                ##print "$parameter => $name  $flag $type\n";
                my $line = "";
                if ( $flag eq '!' ) {
                    my $xx     = int( rand(1) + 0.5 );
                    my $prefix = $xx == 0 ? 'no' : "";
                    $line = "--$prefix$name";
                }
                elsif ( $flag eq '=s' ) {
                    my $string;
                    if ( !$rrange ) { $rrange = \@random_words }
                    my $imax  = @{$rrange} - 1;
                    my $count = 1;
                    if ( $is_multiword_list{$name} ) {
                        $count = $imax / 2 + 1;
                        if ( $count > 10 ) { $count = 10 }
                    }
                    foreach my $i ( 1 .. $count ) {
                        my $index = int( rand($imax) + 0.5 );
                        if ( $i > 1 ) { $string .= ' ' }
                        $string .= $rrange->[$index];
                    }
                    $string = "'$string'";
                    $line   = "--$name=$string";
                }
                elsif ( $flag eq '=i' ) {
                    my $int;
                    if ( !$rrange ) {
                        $rrange = [ 0, 100 ];
                    }

                    # Two items are assumed to be a range
                    if ( @{$rrange} == 2 ) {
                        my ( $imin, $imax ) = @{$rrange};
                        my $frac = rand(1);
                        $int = $imin + $frac * ( $imax - $imin );
                        $int = int( $int + 0.5 );
                    }

                    # Otherwise, assume a list
                    else {
                        my $ix    = @{$rrange} - 1;
                        my $index = int( rand($ix) + 0.5 );
                        $int = $rrange->[$index];
                    }
                    $line = "--$name=$int";
                }
                else {
                    my $xx = int( rand(1) + 0.5 );
                    next unless $xx;
                    $line = "--$name";
                }

                # Now randomly pick and omit flags
                push @random_parameters, $line;
            }
        }
        return \@random_parameters;
    }
}

sub check_DEVEL_MODE {
    my ($fname) = @_;
    my $fh;
    my $changed_count;
    my @output;
    open( $fh, '<', $fname )
      or die "can't open '$fname' : $!\n";
    while (<$fh>) {
        my $line = $_;
        if ( $line =~
            /^(\s*use\s+constant\s+(?:DEVEL)_[A-Z]+\s*)=>\s*(-?\d*);(.*)$/ )
        {
            if ( $2 != '1' ) {
                $changed_count++;
                $line = <<EOM;
$1=> 1;$3
EOM
            }
        }
        push @output, $line;
    }
    $fh->close();

    if ( $changed_count && @output ) {
        my $fh_out;
        open( $fh_out, '>', $fname )
          or die "can't open '$fname' : $!\n";
        foreach (@output) {
            $fh_out->print($_);
        }
        $fh_out->close();
        print STDERR "Changed to DEVEL_MODE => 1\n";
    }
}

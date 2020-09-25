#!/usr/bin/perl -w
use strict;
use warnings;
use Data::Dumper;

# This program sets up a run of perltidy with random parameters and files.
# This is an interactive program which writes a config file and a run script
# for the actual run.

our $rsetup;    # the setup hash
my $config_file   = "config.txt";
my $FILES_file    = "FILES.txt";
my $PROFILES_file = "PROFILES.txt";
my $rfiles    = [];
my $rprofiles = [];

# Run this in a temporary directory to setup the actual run
query(<<EOM);

Be sure you should be in a temporary directory which can be deleted when
this is finished. Hit <cr>.

EOM

# Defaults
default_config();

if ( -e $config_file ) {
    if ( ifyes("Read the existing config.txt file? [Y/N]") ) {
        read_config($config_file);
    }
}

if ( -e $FILES_file ) {
    if (ifyes("Found $FILES_file, read it ? [Y/N]") ) {
        $rfiles = read_list($FILES_file);
        my $nfiles=@{$rfiles};
        print STDOUT "found $nfiles files\n";
    }
}

if ( !@{$rfiles} ) {
    define_files();
    $rfiles = filter_files($rfiles);
}

if ( -e $PROFILES_file ) {
    if (ifyes("Found $PROFILES_file, read it ? [Y/N]") ) {
        $rprofiles = read_list($PROFILES_file);
        my $nfiles=@{$rprofiles};
        print STDOUT "found $nfiles profiles\n";
    }
}

if ( !@{$rprofiles} ) {
    make_profiles();
    $rprofiles = filter_profiles($rprofiles);
}

$rsetup->{'syntax_check'} = ifyes(<<EOM);
Do you want to check syntax with perl -c ?
This will cause any BEGIN blocks in them to execute, which
can introduce a security concern.
Enter 'N' unless you very familiar with the test scripts.
Y/N:
EOM

my $file_info=get_file_info();
my $profile_info = get_profile_info();
my $nprofiles = @{$rprofiles};
while (1) {
    my $files              = $rsetup->{files};
    my $chain_mode         = $rsetup->{chain_mode};
    my $do_syntax_check    = $rsetup->{syntax_check};
    my $delete_good_output = $rsetup->{delete_good_output};
    print <<EOM;
===Main Menu===
R   - Read a config file
F   - Files:    $files
$file_info
P   - Profiles: 
$profile_info
C   - Chain mode               : $chain_mode
D   - Delete good output?      : $delete_good_output
S   - Syntax check?            : $do_syntax_check
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
    elsif ( $ans eq 'F' ) {
            define_files();
            $rfiles = filter_files($rfiles);
            $file_info=get_file_info()
    }
    elsif ( $ans eq 'P' ) {
            make_profiles();
            $rprofiles = filter_profiles($rprofiles);
            $profile_info = get_profile_info();
    }
    elsif ( $ans eq 'C' ) {
        $chain_mode = get_num("Chaining: 0=no, 1=always,2=random");
        $rsetup->{chain_mode} = $chain_mode;
    }
    elsif ( $ans eq 'D' ) {
        $delete_good_output = ifyes("Delete needless good output files? [Y/N]");
        $rsetup->{delete_good_output} = $delete_good_output;
    }
    elsif ( $ans eq 'S' ) {
        $do_syntax_check = ifyes("Do syntax checking? [Y/N]");
        $rsetup->{syntax_check} = $do_syntax_check;
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

    # Ignore .tdy {$rlist}
    @{$rlist} = grep { $_ !~ /\.tdy$/ } @{$rlist};

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

sub define_files {

    $file_info = get_file_info();

    # TODO: add option to generate random files now
    # TODO: add option to shorten a list
    print <<EOM;
====== Define some files to process =================================
$file_info

Note that you can generate random files with 'random_file_generator.pl'
If you want to do that, you should exit now, generate them, then come
back.
EOM
    my $nfiles_old = @{$rfiles};
    if ($nfiles_old) {
        if ( ifyes("Use these files as is? [Y/N]") ) {
            return;
        }
    }

    my $glob = '../*';
    my $ans  = query("File glob of files to process, <cr>='$glob'");
    $glob = $ans if ($ans);
    return unless ($glob);
    my @files = glob($glob);
    @files = grep { -f $_ && !-z $_ } @files;
    @files = grep { $_ !~ /\.tdy$/ } @files;
    @files = grep { $_ !~ /profile\.[0-9]*/ } @files;
    my $nfiles_new = @files;
    print "Found $nfiles_new files\n";
    return unless @files;

    if ( $nfiles_old > 0 ) {
        print "There are already $nfiles_old existing files";
        while (1) {
            my $ans = queryu(<<EOM);
A   Add new files to existing files
R   Replace existing files with new files
X   eXit, keeping existing files as is
EOM
            if    ( $ans eq 'X' ) { return }
            elsif ( $ans eq 'A' ) { last }
            elsif ( $ans eq 'R' ) { @{$rfiles} = []; last }

        }
    }
    push @{$rfiles}, @files;
    $rfiles = uniq($rfiles);
    $rfiles = [ sort @{$rfiles} ];
    return;
}

sub get_profile_info {

    my $nprofiles     = @{$rprofiles};
    my $profile0 = "(none)";
    my $profileN = "(none)";
    if ($nprofiles) {
        $profile0 = $rprofiles->[0];
        $profileN = $rprofiles->[-1];
    }
    my $profile_info  = <<EOM;
    Number of Files: $nprofiles
    First profile     : $profile0
    Last profile      : $profileN
EOM
    return $profile_info;
}

sub get_file_info {

    my $nfiles     = @{$rfiles};
    my $file0 = "(none)";
    my $fileN = "(none)";
    if ($nfiles) {
        $file0 = $rfiles->[0];
        $fileN = $rfiles->[-1];
    }
    my $file_info  = <<EOM;
    Number of Files: $nfiles
    First file     : $file0
    Last file      : $fileN
EOM
    return $file_info;
}

sub default_config {
    $rsetup = {
        chain_mode         => 1,
        delete_good_output => 1,
        syntax_check       => 0,
        profiles           => $PROFILES_file,
        files              => $FILES_file,
    };
    return;
}

sub write_GO {

    my $runme = "GO.sh";
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
    print STDOUT "now enter ./$runme\n";
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
            chomp $line, push @parameters, $line;
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
            my $profile_info  = get_profile_info();
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
                            if ( $1 > $case) { $case= $1 }
                        }
                    }
                    last;
                }
                elsif ( $ans eq 'D' ) {
                    my $num=get_num("Number to keep:",$nfiles_old);
                    if ($num > $nfiles_old || $num <=0 ) {
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
            get_num( "Number of new random profiles to generate", 50);
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
                $rrandom_parameters = ["--mangle -dsc -dac -i=0 -ci=0 -it=2"];
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
            'format' => [ 'tidy', 'html' ],    #, 'user' ],
            'output-line-ending' => [ 'dos', 'win', 'mac', 'unix' ],

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

            'want-break-after'   => \@operators,
            'want-break-before'  => \@operators,
            'want-left-space'    => \@operators,
            'want-right-space'   => \@operators,
            'nowant-left-space'  => \@operators,
            'nowant-right-space' => \@operators,

            #'keyword-group-blanks-list=s
            'keyword-group-blanks-size' => [ 0, 2, 4, 7, 10, 2.8, 1.8 ],

            # TODO: FILL thESE with multiple random keywords
            'space-after-keyword'   => \@keywords,
            'nospace-after-keyword' => \@keywords,

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
            'want-break-after'      => 1,
            'want-break-before'     => 1,
            'want-left-space'       => 1,
            'want-right-space'      => 1,
            'nowant-left-space'     => 1,
            'nowant-right-space'    => 1,
            'space-after-keyword'   => 1,
            'nospace-after-keyword' => 1,
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
                        next;
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

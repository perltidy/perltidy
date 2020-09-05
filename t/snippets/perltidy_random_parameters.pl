#!/usr/bin/perl -w
use strict;
use warnings;

# This program was written to stress-test perltidy by running it repeatedly
# with random parameters.  Testing perltidy on random scripts has always been
# easy. You just collect a bunch of scripts in a directory and use

#  perltidy *

# But it is harder to test a variety of parameters, looking for problems with
# their interactions.  That is what this script does.
# I typically run it in the background from a bash script, something like this
# nohup nice -n19 perltidy_random_parameters.pl $filename $number

# TODO:
# - This currently runs the perltidy binary.  Add an option to run call the
#   module directly.
# - Add additional garbage strings
# - The parameters are hardwired but could be obtained directly from perltidy
#   so that they are always up to date.

my $usage = <<EOM;
Run perltidy repeatedly on a selected file with randomly generated parameters:

    perltidy_random_parameters ifile Num

ifile is the name of a perl script to be formatted
Num is the number of times, default 1000;

You can stop the run any time by creating a file "stop.now"
EOM

my $ifile          = $ARGV[0];
my $ifile_original = $ifile;
my $max_cases      = $ARGV[1];
if ( !$ifile || $max_cases <= 0 ) { die "$usage" }
my $stop_file = 'stop.now';
if ( -e $stop_file ) { unlink $stop_file }
my $ifile_size = -s $ifile;

# Set this flag to have each run format the output of the previous
# run instead of formatting the original file.
my $CHAIN_MODE = 1;

my $case                  = "000";
my $error_count           = 0;
my $missing_ofile_count   = 0;
my $missing_chkfile_count = 0;
my ( $ofile_size_min,   $ofile_size_max );
my ( $ofile_case_min,   $ofile_case_max );
my ( $chkfile_size_min, $chkfile_size_max );
my ( $chkfile_case_min, $chkfile_case_max );
my $error_flag    = 0;
my $restart_count = 0;

for ( 1 .. $max_cases ) {
    $case += 1;
    print STDERR "\n-----\nCase: $case\n";
    print STDOUT "\n-----\nCase: $case\n";
    my $profile            = "profile.$case";
    my $rrandom_parameters = get_random_parameters();
    open OUT, ">", $profile || die "cannot open $profile: $!\n";
    foreach ( @{$rrandom_parameters} ) {
        print OUT "$_\n";
    }
    close OUT;
    my $ofile   = "ofile.$case";
    my $chkfile = "chkfile.$case";
    system "perltidy < $ifile > $ofile -pro=$profile";
    my $efile   = "perltidy.ERR";
    my $logfile = "perltidy.LOG";
    if ( -e $efile )   { rename $efile,   "ERR.$case" }
    if ( -e $logfile ) { rename $logfile, "LOG.$case" }

    if ( !-e $ofile ) {
        print STDERR "**Warning** missing output $ofile\n";
        $missing_ofile_count++;
        $error_flag = 1;
    }

    else {
        my $ofile_size = -s $ofile;
        if ( !defined($ofile_size_min) ) {
            $ofile_size_min = $ofile_size_max = $ofile_size;
            $ofile_case_min = $ofile_case_max = $ofile;
        }
        else {
            if ( $ofile_size < $ofile_size_min ) {
                $ofile_size_min = $ofile_size;
                $ofile_case_min = $ofile;
            }
            if ( $ofile_size > $ofile_size_max ) {
                $ofile_size_max = $ofile_size;
                $ofile_case_max = $ofile;
            }
        }
    }

    # run perltidy on the output to see if it can be reformatted
    # without errors
    system "perltidy < $ofile > $chkfile";
    my $err;
    if ( -e $efile ) {
        rename $efile, "$chkfile.ERR";
        print STDERR "**Error reformatting** see $chkfile.ERR\n";
        $error_count++;
        $err = 1;
    }
    if ( !-e $chkfile ) {
        print STDERR "**Warning** missing checkfile output $chkfile\n";
        $missing_chkfile_count++;
        $err = 1;
    }
    else {
        my $chkfile_size = -s $chkfile;
        if ( !defined($chkfile_size_min) ) {
            $chkfile_size_min = $chkfile_size_max = $chkfile_size;
            $chkfile_case_min = $chkfile_case_max = $chkfile;
        }
        else {
            if ( $chkfile_size < $chkfile_size_min ) {
                $chkfile_size_min = $chkfile_size;
                $chkfile_case_min = $chkfile;
            }
            if ( $chkfile_size > $chkfile_size_max ) {
                $chkfile_size_max = $chkfile_size;
                $chkfile_case_max = $chkfile;
            }
        }
    }

    if ($CHAIN_MODE) {
        $ifile = $err ? $ifile_original : $ofile;
        $restart_count++ if ($err);
    }

    if ( -e $stop_file ) {
        print STDERR "$stop_file seen; exiting\n";
    }
}

# Report results
my $mode =
  $CHAIN_MODE ? "chain mode, $restart_count restarts" : 'non-chain mode';
print STDERR <<EOM;
Run Summary:
$ifile_original: starting input file
running in $mode
$error_count files had errors when reformatted
$missing_ofile_count output files were missing 
$missing_chkfile_count check output files were missing

Size of test file: $ifile_size
EOM

print STDERR <<EOM if ( defined($ofile_size_min) );
Minimum output size: $ofile_size_min for case $ofile_case_min
Maximum output size: $ofile_size_max for case $ofile_case_max
EOM

print STDERR <<EOM if ( defined($chkfile_size_min) );
Minimum rerun size: $chkfile_size_min for case $chkfile_case_min
Maximum rerun size: $chkfile_size_max for case $chkfile_case_max
EOM

print STDERR <<EOM;

Be sure to search for 'unitialized' in STDERR
EOM

sub get_random_parameters {

    # return a set of random parameters for perltidy
    my @random_parameters;

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
    my @parameters = qw(
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

    my %flag_types = (
        '!'  => 'BINARY FLAG',
        '=s' => 'STRING',
        '=i' => 'INTEGER',
        ':i' => 'OPTIONAL INTEGER',
        ':s' => 'OPTIONAL STRING',
    );

    my @random_words = qw(bannanas sub train apples);
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

        # TODO: FILL thESE with multiple random operators
        'want-break-after'   => \@operators,    #['+', '-', '*', '=', '.'],
        'want-break-before'  => \@operators,    #['+', '-', '*'],
        'want-left-space'    => \@operators,    #['+', '-', '*'],
        'want-right-space'   => \@operators,    #['+', '-', '*'],
        'nowant-left-space'  => \@operators,    #['+', '-', '*'],
        'nowant-right-space' => \@operators,    #['+', '-', '*'],

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
    # Some are best skipped, otherwise the program will just quit early
    # Parameters can be added and removed from the list to customize
    # testing.
    # 'format' was added because html is not so interesting, but can be
    # removed.
    ###################################################################
    my @q = qw(
      DEBUG
      quiet
      logfile
      logfile-gap
      backup-and-modify-in-place
      backup-file-extension
      character-encoding
      format-skipping-begin
      format-skipping-end
      outfile
      output-file-extension
      output-path
      output-file-extension
      output-line-ending
      standard-output
      standard-error-output
      html
      notidy
      format
      starting-indentation-level
      tee-block-comments
      tee-pod
      tee-side-comments
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

sub queryu {
    return uc query(@_);
}

sub query {
    my ($msg) = @_;
    print $msg;
    my $ans = <STDIN>;
    chomp $ans;

    #my $val=$ans;
    return $ans;
}

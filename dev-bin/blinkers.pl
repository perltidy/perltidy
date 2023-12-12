#!/usr/bin/perl
use strict;
use warnings;

# This program assists in debugging blinking states

# TODO:
# - warn user when minimizing very large files
# - fully implement the rename option
# - add help info

use File::Copy;
use File::Temp qw(tempfile);
use Data::Dumper;
use Cwd         qw(getcwd);
use Digest::MD5 qw(md5_hex);
use Encode      ();

use constant EMPTY_STRING => q{};

# This database is used in minimization and initialized in BEGIN block at the
# end:
my $rfrequency_hash;

$| = 1;

# The perltidy git home directory is only needed for the 'rename' feature.
my $git_home = EMPTY_STRING;
find_git_home();
my $know_git_home = $git_home;

my $perltidy         = 'perltidy';
my $perltidy_version = EMPTY_STRING;
set_perltidy();

my $rdirs      = [];
my $dir_string = EMPTY_STRING;
set_dirs();

my $rcode = {
    'TIDY' => \&set_perltidy,
    'DIRS' => \&set_dirs,
    'RET'  => \&retest_blinkers,
    'MIN'  => \&minimize_profiles,
    'GIT'  => \&ask_git_home,
    'REN'  => \&rename_blinkers,
};

main();

sub main {

    while (1) {
        print <<EOM;
----------------------------
Main Menu - Case Insensitive
----------------------------

dirs  - directories: $dir_string
tidy  - use perltidy version: $perltidy v$perltidy_version
git   - git home: $git_home
ret   - retest check/update Version Number
min   - minimize profile(s)
ren   - rename a set of directories
q,x   - eXit

EOM
        my $ans = queryu(":");
        if ( defined( $rcode->{$ans} ) ) {
            $rcode->{$ans}->();
        }
        elsif ( $ans eq 'Q' || $ans eq 'X' ) {
            return;
        }
        else {
            hitcr("$ans: not a command");
        }
    }
    return;
}

sub set_perltidy {

    # define the version of perltidy to use

    use Cwd qw(getcwd);
    use Cwd 'abs_path';

    while (1) {
        my $ans = query("Enter the file with perltidy, <cr>='perltidy':");
        last unless ($ans);
        if ( -e $ans && -x $ans ) {
            $perltidy = abs_path($ans);
            last;
        }
        hitcr("cannot find an executable file named '$ans'");
    }
    $perltidy_version = get_version($perltidy);
    print STDERR "got '$perltidy version $perltidy_version'\n";
    return;
}

sub get_version {
    my ($my_perltidy) = @_;

    # get the perltidy version
    my ( $fh, $tmpname ) = File::Temp::tempfile();
    my $cmd = "$my_perltidy -v >$tmpname";
    system($cmd);
    $fh->close();
    if ( !open( $fh, '<', $tmpname ) ) {
        print STDERR "cannot open $tmpname: $!\n";
        return;
    }
    my $line = <$fh>;
    chomp $line;
    my ( $part1, $version ) = split /,\s+v/, $line;
    $fh->close();
    if ( -e $tmpname ) { unlink $tmpname }
    return $version;
}

sub set_dirs {

    # define the directories with blinking cases to process

    my $str = query("Enter directory glob pattern, or enter <cr> for all");
    if ( !$str ) { $str = '*' }
    my @list = glob($str);
    $rdirs = [];
    foreach (@list) {
        if ( -d $_ ) { push @{$rdirs}, $_ }
    }
    if ( !@{$rdirs} ) { push @{$rdirs}, '.'; }
    $dir_string = join " ", @{$rdirs};
    if ( length( $dir_string . 40 ) ) {
        my $numd = @{$rdirs};
        $dir_string = substr( $dir_string, 0, 40 );
        $dir_string .= "...[$numd dirs]";
    }
    return;
}

sub find_git_home {
    my ( $fh, $err_file ) = File::Temp::tempfile();

    # See if we are within the perltidy git
    $git_home = qx[git rev-parse --show-toplevel 2>$err_file];
    chomp $git_home;
    if ( -e $err_file ) { unlink $err_file }
    return;
}

sub ask_git_home {

    # Only allow user to change git_home if it is not known automatically
    if ( !$know_git_home ) {
        $git_home = query(<<EOM);
We are not within the perltidy git. If you want to rename folders,
you will need to enter the path to the perltidy git home directory:
EOM
        chomp $git_home;
    }
    return;
}

sub retest_blinkers {

    # test one or more directories with blinking profiles and check
    # if still blinking

    # put directories in these categories:
    my @unknown;         # state 0
    my @blinking;        # state 1
    my @not_blinking;    # state 2

    my $starting_dir = getcwd();

    foreach my $dir ( @{$rdirs} ) {
        next unless ( -d $dir );
        chdir $dir;
        print ">>>Entering $dir\n";
        my $rhash = {
            perltidy       => $perltidy,
            dir            => $dir,
            blinking       => undef,
            infile         => undef,
            profile        => undef,
            error_message  => EMPTY_STRING,
            basename       => 'testy',
            max_iterations => 5,
        };
        find_starting_files($rhash);
        if ( !defined( $rhash->{infile} ) || !defined( $rhash->{profile} ) ) {
            my $error_message = $rhash->{error_message};
            print STDERR "starting files not found:\n";
            print STDERR $error_message;
            push @unknown, $dir;
            next;
        }

        my $state = blinker_test($rhash);
        if    ( !defined($state) ) { push @unknown,      $dir }
        elsif ( $state == 0 )      { push @not_blinking, $dir }
        elsif ( $state == 1 )      { push @blinking,     $dir }
        chdir $starting_dir;    ##BOOGA
    }

    chdir $starting_dir;

    my $report = "check.out";
    my $backup = $report . ".bak";
    if ( -e $report ) {
        if ( -e $backup ) { unlink $backup }
        rename( $report, $backup );
    }

    my $fh;
    if ( !open( $fh, '>', $report ) ) {
        print "cannot open $report :$!\n";
    }

    $fh->print("retest results using '$perltidy'\n");

    if (@unknown) {
        my $num = @unknown;
        my $msg = <<EOM;
$num dirs UNKNOWN (maybe errors)
(@unknown)
EOM
        print $msg;
        $fh->print($msg) if ($fh);
    }

    if (@blinking) {
        my $num = @blinking;
        my $msg = <<EOM;

$num dirs STILL blinking:
@blinking
EOM
        print $msg;
        $fh->print($msg) if ($fh);
    }

    if (@not_blinking) {
        my $num = @not_blinking;
        my $msg = <<EOM;

$num dirs NOT blinking:
@not_blinking
EOM
        print $msg;
        $fh->print($msg) if ($fh);
    }
}

sub find_starting_files {

    # Find the initial input file and profile in the current directory
    my ($rhash) = @_;
    $rhash->{infile}        = undef;
    $rhash->{outfile}       = undef;
    $rhash->{error_message} = EMPTY_STRING;

    my @files = glob("*");
    my ( $profile,  $txtfile,  $ifile );
    my ( @profiles, @txtfiles, @ifiles );
    my @unknown;
    foreach my $file (@files) {
        next if ( $file =~ /^perltidy/ );
        next if ( $file =~ /\.(ERR|LOG)/ );

        #print "file=$file\n";
        if ( $file =~ /^profile\.\d+$/ ) {
            $profile = $file;
            push @profiles, $file;
        }
        elsif ( $file =~ /\.txt$/ ) {
            $txtfile = $file;
            push @txtfiles, $file;
        }
        elsif ( $file =~ /^ofile/ || $file =~ /\.p[lm]/i ) {
            $ifile = $file;
            push @ifiles, $file;
        }
        else { push @unknown, $file }
    }
    if ( @profiles != 1 ) {
        my $num = @profiles;
        $rhash->{error_message} = "Number of profiles found: $num\n";
    }
    else {
        $rhash->{profile} = $profile;
    }
    if ( @ifiles != 1 ) {
        my $num = @ifiles;
        $rhash->{error_message} = "Number of perl files found: $num\n";
    }
    else {
        $rhash->{infile} = $ifiles[0];
    }
    return;
}

sub blinker_test {

    my ($rhash) = @_;

    my $ifile       = $rhash->{infile};
    my $pfile       = $rhash->{profile};
    my $my_perltidy = $rhash->{perltidy};
    return unless ( $pfile && $ifile && $my_perltidy );

    my $is_blinker;

    # overwritten
    my $dfile = "tmp34.txt";
    my $cmd;
    my $count      = 0;
    my $ofile_last = $ifile;
    my $ofile;
    my $imax = 4;
    my @ofiles;
    my %digest_seen;
    my $digest_in = digest_file($ifile);
    $digest_seen{$digest_in} = 0;

    # Option changes:
    # -it=1   (just do 1 iteration)
    # -nopt   (do not write a logfile)
    # -irc=1  (do not check integer ranges):
    my $my_opts = "-it=1 -nopt";
    if ( $perltidy_version >= 20230912.05 ) {
        $my_opts .= " -irc=1";
    }
    foreach my $i ( 0 .. $imax ) {
        $ofile = "temp.$i";
        push @ofiles, $ofile;
        $count++;
        $cmd = "$my_perltidy $ofile_last -o $ofile -pro=$pfile $my_opts";
        system($cmd);
        return unless ( -e $ofile );

        # diff method for finding blinkers
        # TODO: just do diff for blinking states
        if ( $i > 0 ) {
            $cmd = "diff $ofile_last $ofile >$dfile";
            system($cmd);
            return unless -e $dfile;    ## error of some kind
            $is_blinker = -z $dfile ? 0 : 1;
            if ( !$is_blinker ) {
                if ( -e $dfile ) { unlink $dfile }
                last;
            }
        }

        # md5 digest method for finding blinkers
        my $digest = digest_file($ofile);
        if ( defined( $digest_seen{$digest} ) ) {
            $is_blinker = $digest_seen{$digest} == $i - 1 ? 0 : 1;
        }
        $digest_seen{$digest} = $i;

        $ofile_last = $ofile;
    }

    if ($is_blinker) {

        # The last blinker will leave the following files:
        # tmp.3 tmp.4 tmp34.diff
        my $qfile = "tmp34.diff";
        if ( -e $qfile ) { unlink $qfile }
        rename $dfile,               $qfile;
        rename $ofiles[ $imax - 1 ], "tmp.3" if ( $imax > 0 );
        rename $ofiles[$imax],       "tmp.4" if ( $imax > 1 );
    }

    # clean up
    foreach my $file (@ofiles) {
        if ( -e $file ) { unlink $file }
    }
    return $is_blinker;
}

sub minimize_profiles {

    # reduce the number of parameters to the minimum possible which
    # still produces a blinking state.

    my $my_perltidy  = $perltidy;
    my $starting_dir = getcwd();

    foreach my $dir ( @{$rdirs} ) {
        next unless ( -d $dir );
        chdir $dir;
        my $rhash = {
            perltidy      => $my_perltidy,
            dir           => $dir,
            blinking      => undef,
            infile        => undef,
            profile       => undef,
            error_message => EMPTY_STRING,
        };
        find_starting_files($rhash);
        my $ifile = $rhash->{infile};
        my $pfile = $rhash->{profile};
        if ( !defined($ifile) || !defined($pfile) ) {
            print "Cannot find starting files in dir '$dir'\n";
        }
        profile_simplify( $ifile, $pfile, $my_perltidy );
        chdir $starting_dir;
    }
    return;
}

sub profile_simplify {
    my ( $ifile, $pfile, $my_perltidy ) = @_;
    my $dir = getcwd();

    print ">>>ifile=$ifile, pfile=$pfile, dir=$dir\n";
    if ( !-e $pfile ) {
        query("Cannot find $pfile for this case: please run 'prep'");
        return;
    }

    my @tmp_list;
    open( PRO, "<", $pfile ) || die "cannot open $pfile: $!\n";
    foreach my $line (<PRO>) {
        chomp $line;
        next if ( $line =~ /^#/ );
        next unless $line;
        my $key = "";
        if ( $line =~ /^-*?(\w[\w-]+\b)/ ) { $key = $1 }
        my $freq = $rfrequency_hash->{$key};
        if ( !defined($freq) ) {
            $freq = 0;
        }
        push @tmp_list, [ $line, $freq ];
    }
    close(PRO);
    my @profile_start =
      map { $_->[0] }
      sort { $b->[1] <=> $a->[1] } @tmp_list;

    my ( $rprofile_keep, $err_msg ) =
      profile_simplify_inner_loop( \@profile_start, $ifile, $my_perltidy );

    if ($err_msg) {
        query( $err_msg . "hit <CR>" );
        return;
    }

    # Make the final set
    @{$rprofile_keep} = reverse @{$rprofile_keep};
    my $pfile_min = "test.pro";
    if ( -e $pfile_min ) { rename $pfile_min, "$pfile_min.bak" }
    param_to_file( $pfile_min, $rprofile_keep );
    return;
}

sub profile_simplify_inner_loop {
    my ( $rprofile_start, $ifile, $my_perltidy ) = @_;
    my $rprofile_keep = [];

    my $num_start = @{$rprofile_start};

    # The basic idea: we want to find the minimum number of parameters
    # that will cause blinking. Typically, we start with around 150
    # parameters and find that only about 6 are needed to cause the
    # blinking.

    # The brute force way is to try removing one parameter at a time and
    # see if blinking continues. If blinking stops, then we must keep that
    # parameter, and otherwise we can eliminate it.  This brute force
    # method is a slow process (several cpu minutes).

    # The following method runs in about 1/5 of the brute force
    # time using a set of about three chunk sizes (see the table below):

    # - Start with the parameters sorted so that the most likely to cause
    #   blinking occur at the end of the list.  We know this from past work.
    # - Testing is done by removing chunks of parameters and testing if
    #   blinking stops.  Keep testing until we have tested all parametes:
    # - On the first test, use the full set to verify blinking;
    #   - exit with error if not blinking.
    #   - otherwise, switch to maximum chunk size and continue.
    # - On each subsequent test:
    #   - If blinking does not stop:
    #       - remove the chunk of parameters and Increase the chunk size
    #         as much as possible with $upshift->() and continue.
    #   - If blinking stops:
    #       - if chunk size is >0, reduce the chunk size to the next lower
    #         value ($downshift()) and try again.
    #       - if chunk size is 1, keep the parameter and increase the
    #         chunk size to max with $upshift->() and continue

    # Table of chunk sizes; see c229 for notes.
    # All of these are okay; it can be useful to try different tables
    # to see if the minimized parameter sets are unique. For example,
    # sets b1462 and b1463 both started with the same starting case,
    # but b1463 had a slightly different blinking mechanism.
    # my @chunk_table = ( 1, 5, 20, 40 );        # works well
    # my @chunk_table = ( 1, 3, 6, 12, 24, 48);  # works well
    my @chunk_table = ( 1, 4, 12, 36 );

    # This controls the maximum chunk size as a function of the number
    # of remaining parameters. Values of 2 to 3 work well.
    my $ratio = 2;    ## or 3

    my $err_msg            = "";
    my $npass              = 0;
    my $non_blinking_count = 0;
    my $imax               = @chunk_table - 1;
    my $num_left           = @{$rprofile_start};

    # First pass uses zero chunk size to verify blinking
    my $chunk   = 0;
    my $i_chunk = 0;

    my $upshift = sub {

        # Shift up to the largest possible chunk
        $i_chunk = 0;
        $chunk   = 1;
        foreach my $i ( 1 .. $imax ) {
            last if ( $chunk_table[$i] * $ratio > $num_left );
            $i_chunk = $i;
            $chunk   = $chunk_table[$i_chunk];
        }
    };

    my $downshift = sub {

        # Shift down 1 notch
        if ( $i_chunk > 0 ) {
            $i_chunk -= 1;
            $chunk = $chunk_table[$i_chunk];
        }
    };

    # main loop to test each parameter and keep only parameters
    # needed to cause blinking:
    while (1) {

        $num_left = @{$rprofile_start};
        last if ( !$num_left );

        $npass++;

        # remove $chunk parameters and check for blinking
        my $imax_start   = $#{$rprofile_start};
        my @profile_test = (
            @{$rprofile_keep},
            ( @{$rprofile_start} )[ 0 .. $imax_start - $chunk ]
        );

        my $pname = "tmp.pro";
        param_to_file( $pname, \@profile_test );

        my $rhash = {
            infile   => $ifile,
            profile  => $pname,
            perltidy => $my_perltidy,
        };

        my $is_blinking = blinker_test($rhash);

        my $keep_size = @{$rprofile_keep};
        print <<EOM;
pass $npass chunk=$chunk keep is $keep_size, remaining=$num_left blink=$is_blinking;
EOM

        if ($is_blinking) {

            # a zero $chunk size indicates first pass
            if ( !$chunk ) {
                $upshift->();
                next;
            }

            # otherwise, remove $chunk params and continue
            if ( $chunk >= @{$rprofile_start} ) {
                @{$rprofile_start} = ();
            }
            else {
                splice @{$rprofile_start}, -$chunk;
            }
            next;
        }

        # Not blinking: one of the parameters in the chunk is a keeper
        else {

            $non_blinking_count++;

            # For large chunk; shift down and try again
            if ( $chunk > 1 ) {
                $downshift->();
                next;
            }

            # For a single non-blinking parameter ..
            elsif ( $chunk == 1 ) {

                # Move this parameter from 'start' to 'keep'
                push @{$rprofile_keep}, pop @{$rprofile_start};
                $upshift->();
                next;
            }

            # Zero chunk indicates first pass; non-blinking is ERROR
            else {
                $err_msg = "Not blinking with all starting parameters\n";
                last;
            }
        }
    }
    print
"$npass passes with $num_start items and table=@chunk_table and ratio=$ratio\n";
    return ( $rprofile_keep, $err_msg, $npass );
}

sub param_to_file {
    my ( $pname, $rprofile ) = @_;
    open( OUT, ">", $pname ) || die "cannot open $pname: $!\n";
    foreach my $line ( @{$rprofile} ) {
        chomp $line;
        print OUT $line . "\n";
    }
    close OUT;
    return;
}

sub rename_blinkers {
    my $data      = $git_home . '/dev-bin/run_convergence_tests.pl.data';
    my $last_name = find_last_name($data);
    if ( !$last_name ) {
        hitcr("Could not find the previous case\n");
        return;
    }

    # For now, just give the next name
    # TODO: actually make the name change ( see blinker_rename.pl)
    my $next_name = ++$last_name;
    hitcr("The next available case name is: '$next_name'\n");

    return;
}

sub find_last_name {
    my ($data) = @_;

    # Find the last blinker case number in the file $data
    # We ignore cases not beginning with 'b'

    if ( !-e $data ) {
        print STDERR "Cannot find $data\n";
        return;
    }
    my $string = slurp_file($data);
    my @lines  = split /^/, $string;
    my $max_case;

    #==> b001.in <==
    foreach my $line (@lines) {
        if ( $line =~ /^\s*==>\s*b(\d+)\.in\s*<==\s*$/ ) {
            if ( !defined($max_case) || $max_case < $1 ) { $max_case = $1 }
        }
    }

    if ( defined($max_case) ) { $max_case = 'b' . $max_case }
    return $max_case;
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
}

sub queryu {
    return uc query(@_);
}

sub hitcr {
    my ($msg) = @_;
    if ($msg) { $msg .= " Hit <cr> to continue"; }
    else      { $msg = "Hit <cr> to continue" }
    query($msg);
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

sub system_echo {
    my ( $cmd, $quiet ) = @_;
    print "$cmd\n" unless ($quiet);
    system $cmd;
    return;
}

sub slurp_file {
    my ($file) = @_;
    open my $fh, '<', $file or die "cannot open $file\n";
    local $/;
    my $contents = <$fh>;
    close $fh;
    return $contents;
}

sub digest_string {
    my ($buf)  = @_;
    my $octets = Encode::encode( "utf8", $buf );
    my $digest = md5_hex($octets);
    return $digest;
}

sub digest_file {
    my ($file) = @_;
    my $buf    = slurp_file($file);
    my $digest = digest_string($buf);
    return $digest;
}

BEGIN {

    # This database was made with 'run_convergence_tests.pl -s'
    # It shows the frequency that a parameter appears in a blinker issue and
    # is used to optimize searches for minimum parameter sets.
    $rfrequency_hash = {
        'add-trailing-commas'                    => 24,
        'blank-lines-after-opening-block'        => 14,
        'blank-lines-before-closing-block'       => 12,
        'blank-lines-before-packages'            => 7,
        'blank-lines-before-subs'                => 6,
        'block-brace-tightness'                  => 13,
        'block-brace-vertical-tightness'         => 19,
        'brace-left-and-indent'                  => 14,
        'brace-tightness'                        => 19,
        'brace-vertical-tightness'               => 23,
        'brace-vertical-tightness-closing'       => 14,
        'break-after-all-operators'              => 18,
        'break-at-old-comma-breakpoints'         => 19,
        'break-at-old-method-breakpoints'        => 14,
        'break-at-old-semicolon-breakpoints'     => 6,
        'break-before-all-operators'             => 49,
        'break-before-hash-brace'                => 42,
        'break-before-hash-brace-and-indent'     => 37,
        'break-before-paren'                     => 124,
        'break-before-paren-and-indent'          => 92,
        'break-before-square-bracket'            => 54,
        'break-before-square-bracket-and-indent' => 38,
        'check-syntax'                           => 7,
        'closing-brace-indentation'              => 14,
        'closing-paren-indentation'              => 10,
        'closing-side-comment-else-flag'         => 9,
        'closing-side-comment-interval'          => 15,
        'closing-side-comment-maximum-text'      => 14,
        'closing-side-comment-warnings'          => 8,
        'closing-side-comments'                  => 5,
        'closing-square-bracket-indentation'     => 10,

        #        'closing-token-indentation'                 => 15,  # alias
        'comma-arrow-breakpoints'                   => 48,
        'continuation-indentation'                  => 303,
        'cuddled-block-list-exclusive'              => 6,
        'cuddled-break-option'                      => 15,
        'cuddled-else'                              => 8,
        'default-tabsize'                           => 12,
        'delete-block-comments'                     => 7,
        'delete-closing-side-comments'              => 7,
        'delete-old-whitespace'                     => 52,
        'delete-side-comments'                      => 11,
        'delete-trailing-commas'                    => 23,
        'entab-leading-whitespace'                  => 11,
        'extended-continuation-indentation'         => 163,
        'extended-line-up-parentheses'              => 62,
        'extrude'                                   => 1,
        'fixed-position-side-comment'               => 16,
        'ignore-old-breakpoints'                    => 135,
        'ignore-side-comment-lengths'               => 11,
        'indent-closing-brace'                      => 12,
        'indent-columns'                            => 322,
        'indent-spaced-block-comments'              => 12,
        'keep-interior-semicolons'                  => 11,
        'keep-old-blank-lines'                      => 19,
        'keep-old-breakpoints-after'                => 13,
        'keep-old-breakpoints-before'               => 8,
        'keyword-group-blanks-after'                => 9,
        'keyword-group-blanks-before'               => 12,
        'keyword-group-blanks-delete'               => 9,
        'keyword-group-blanks-inside'               => 5,
        'keyword-group-blanks-repeat-count'         => 17,
        'keyword-group-blanks-size'                 => 18,
        'keyword-paren-inner-tightness'             => 16,
        'line-up-parentheses'                       => 227,
        'long-block-line-count'                     => 17,
        'maximum-consecutive-blank-lines'           => 15,
        'maximum-fields-per-table'                  => 23,
        'maximum-file-size-mb'                      => 16,
        'maximum-level-errors'                      => 19,
        'maximum-line-length'                       => 530,
        'maximum-unexpected-errors'                 => 19,
        'minimum-space-to-comment'                  => 12,
        'noadd-semicolons'                          => 3,
        'noadd-whitespace'                          => 168,
        'noblanks-before-blocks'                    => 6,
        'noblanks-before-comments'                  => 7,
        'nobreak-after-all-operators'               => 8,
        'nobreak-at-old-attribute-breakpoints'      => 12,
        'nobreak-at-old-comma-breakpoints'          => 7,
        'nobreak-at-old-keyword-breakpoints'        => 9,
        'nobreak-at-old-logical-breakpoints'        => 7,
        'nobreak-at-old-method-breakpoints'         => 10,
        'nobreak-at-old-semicolon-breakpoints'      => 8,
        'nobreak-at-old-ternary-breakpoints'        => 12,
        'nobreak-before-all-operators'              => 6,
        'noclosing-side-comment-warnings'           => 6,
        'noclosing-side-comments'                   => 10,
        'noclosing-side-comments-balanced'          => 11,
        'nocuddled-block-list-exclusive'            => 8,
        'nodelete-block-comments'                   => 7,
        'nodelete-closing-side-comments'            => 7,
        'nodelete-old-newlines'                     => 4,
        'nodelete-semicolons'                       => 7,
        'nodelete-side-comments'                    => 6,
        'noextended-line-up-parentheses'            => 1,
        'noextended-syntax'                         => 8,
        'noformat-skipping'                         => 6,
        'noframes'                                  => 5,
        'nofuzzy-line-length'                       => 12,
        'nohanging-side-comments'                   => 7,
        'noignore-old-breakpoints'                  => 10,
        'noignore-side-comment-lengths'             => 8,
        'noindent-block-comments'                   => 6,
        'noindent-closing-brace'                    => 6,
        'noindent-spaced-block-comments'            => 6,
        'nokeep-interior-semicolons'                => 7,
        'noline-up-parentheses'                     => 5,
        'nological-padding'                         => 10,
        'nolook-for-autoloader'                     => 8,
        'nolook-for-selfloader'                     => 11,
        'nomemoize'                                 => 10,
        'nonon-indenting-braces'                    => 9,
        'noopening-anonymous-sub-brace-on-new-line' => 10,
        'noopening-brace-always-on-right'           => 10,
        'noopening-brace-on-new-line'               => 7,
        'noopening-hash-brace-right'                => 9,
        'noopening-paren-right'                     => 9,
        'noopening-square-bracket-right'            => 9,
        'noopening-sub-brace-on-new-line'           => 8,
        'nooutdent-keywords'                        => 7,
        'nooutdent-labels'                          => 7,
        'nooutdent-long-comments'                   => 6,
        'nooutdent-long-quotes'                     => 12,
        'nooutdent-static-block-comments'           => 9,
        'nopass-version-line'                       => 18,
        'norecombine'                               => 49,
        'nospace-after-keyword'                     => 21,
        'nospace-for-semicolon'                     => 17,
        'nospace-function-paren'                    => 19,
        'nospace-keyword-paren'                     => 23,
        'nospace-terminal-semicolon'                => 17,
        'nostack-closing-block-brace'               => 24,
        'nostack-closing-hash-brace'                => 27,
        'nostack-closing-paren'                     => 31,
        'nostack-closing-square-bracket'            => 25,
        'nostack-opening-hash-brace'                => 29,
        'nostack-opening-paren'                     => 30,
        'nostack-opening-square-bracket'            => 23,
        'nostatic-block-comments'                   => 21,
        'notight-secret-operators'                  => 27,
        'notrim-pod'                                => 21,
        'notrim-qw'                                 => 31,
        'novalign'                                  => 30,
        'novalign-code'                             => 2,
        'novalign-side-comments'                    => 1,
        'novariable-maximum-line-length'            => 26,
        'nowant-left-space'                         => 32,
        'nowant-right-space'                        => 36,
        'one-line-block-nesting'                    => 19,
        'one-line-block-semicolons'                 => 18,
        'opening-anonymous-sub-brace-on-new-line'   => 18,
        'opening-brace-always-on-right'             => 24,
        'opening-brace-on-new-line'                 => 12,
        'opening-hash-brace-right'                  => 12,
        'opening-paren-right'                       => 22,
        'opening-square-bracket-right'              => 14,
        'opening-sub-brace-on-new-line'             => 11,
        'outdent-keywords'                          => 15,
        'outdent-static-block-comments'             => 10,
        'paren-tightness'                           => 44,
        'paren-vertical-tightness'                  => 69,
        'paren-vertical-tightness-closing'          => 54,
        'pbp'                                       => 1,
        'short-concatenation-item-length'           => 41,
        'show-options'                              => 20,
        'space-after-keyword'                       => 42,
        'space-backslash-quote'                     => 20,
        'space-function-paren'                      => 55,
        'space-keyword-paren'                       => 32,
        'space-prototype-paren'                     => 13,
        'space-terminal-semicolon'                  => 46,
        'square-bracket-tightness'                  => 37,
        'square-bracket-vertical-tightness'         => 31,
        'square-bracket-vertical-tightness-closing' => 38,
        'stack-closing-block-brace'                 => 25,
        'stack-closing-hash-brace'                  => 25,
        'stack-closing-paren'                       => 25,
        'stack-closing-square-bracket'              => 25,
        'stack-opening-hash-brace'                  => 24,
        'stack-opening-paren'                       => 29,
        'stack-opening-square-bracket'              => 28,
        'static-side-comments'                      => 21,
        'tabs'                                      => 2,
        'tight-secret-operators'                    => 22,
        'trim-pod'                                  => 29,
        'use-unicode-gcstring'                      => 25,
        'valign-inclusion-list'                     => 1,
        'variable-maximum-line-length'              => 176,
        'vertical-tightness'                        => 92,
        'vertical-tightness-closing'                => 67,
        'want-break-after'                          => 19,
        'want-break-before'                         => 40,
        'want-left-space'                           => 14,
        'want-right-space'                          => 14,
        'want-trailing-commas'                      => 24,
        'warning-output'                            => 3,
        'weld-fat-comma'                            => 1,
        'weld-nested-containers'                    => 183,
        'whitespace-cycle'                          => 7,
    };
}

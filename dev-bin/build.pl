#!/usr/bin/perl
use strict;
use warnings;
use Perl::Tidy;
use File::Copy;
use File::Temp qw(tempfile);
$| = 1;

# a script to help make a new version of perltidy

# First cd to the git root directory, so that all paths are then given from the
# git root
my $git_home = qx[git rev-parse --show-toplevel];
chomp $git_home;
chdir $git_home;

# Here are the main packages I used to setup a Ubuntu 16.04 system
# for Perl development:
#
#  sudo apt-get install openssh-server
#  sudo apt-get install markdown
#  sudo apt-get install ispell
#
#  # Perl Modules:
#  sudo cpan -i App::cpanminus
#  cpanm Perl::Critic
#  cpanm Tidy::All
#  cpanm Perl::::MinimumVersion  (has perlver)
#  cpanm App::CPANTS::Lint       (has cpants_lint.pl)
#  sudo cpan App::perlbrew
#  perlbrew init
#  sudo apt-get install git
#  git config --global user.name "Steve Hancock"
#  git config --global user.email perltidy@users.sourceforge.net

# TODO:
# add a perlver step
# add a browse the tar file step

my $logfile   = "dev-bin/build.log";
my $changelog = "CHANGES.md";
my $fh_log;

# These are the main steps, in approximate order, for making a new version
# Note: Since perl critic is in the .tidyallrc, a separate 'PC' step is not
# needed
my $rsteps = [qw( CHK CONV TOK V PC TIDY T CL DOCS MANIFEST DIST)];

my $rstatus = {};
foreach my $step ( @{$rsteps} ) { $rstatus->{$step} = 'TBD' }

my $rcode = {
    'CHK' => sub {
        openurl("local-docs/Release-Checklist.md")
          unless $rstatus->{CHK} eq 'OK';
        $rstatus->{CHK} = 'OK';
    },
    'V'        => \&update_version_number,
    'PC'       => \&run_perl_critic,
    'TIDY'     => \&run_tidyall,
    'CONV'     => \&run_convergence_tests,
    'TOK'      => \&run_tokenizer_tests,
    'MANIFEST' => \&make_manifest,
    'T'        => \&make_tests,
    'DOCS'     => \&make_docs,
    'DIST'     => \&make_dist,
    'CL'       => sub { openurl($changelog);        $rstatus->{CL}   = 'OK' },
    'LOG'      => sub { openurl($logfile);          $rstatus->{LOG}  = 'OK' },
    'HTML'     => sub { openurl("docs/index.html"); $rstatus->{HTML} = 'OK' },
};

open( $fh_log, ">", $logfile ) or die "cannot open log file $logfile: $!\n";
main();
$fh_log->close();

sub main {
    while (1) {
##A        - run All Steps...
        print <<EOM;
-------------------------------------------
Perltidy Build Main Menu - Case Insensitive
-------------------------------------------

chk      - view release CHecKlist          status: $rstatus->{'CHK'}
v        - check/update Version Number     status: $rstatus->{'V'}
tidy     - run tidyall (tidy & critic)     status: $rstatus->{'TIDY'}
pc       - run PerlCritic (critic only)    status: $rstatus->{'PC'}
conv     - run convergence tests           status: $rstatus->{'CONV'}
tok      - run tokenizer tests             status: $rstatus->{'TOK'}
manifest - make MANIFEST                   status: $rstatus->{'MANIFEST'}
t        - make Tests			   status: $rstatus->{'T'}
cl       - review/edit CHANGES.md          status: $rstatus->{'CL'}
docs     - check and process POD & html    status: $rstatus->{'DOCS'}
dist     - make a Distribution tar.gz      status: $rstatus->{'DIST'}
log      - view Log file
html     - view html files

q,x   - eXit

EOM
        my $ans = queryu(":");
        if ( defined( $rcode->{$ans} ) ) {
            $rcode->{$ans}->();
        }
        elsif ( $ans eq 'Q' || $ans eq 'X' ) {
            return;
        }
    }
    return;
}

sub post_result {
    my ($fout) = @_;

    # copy contents of a text file to log and display it
    my $fh;
    if ( !open( $fh, '<', $fout ) ) {
        hitcr("Strange: cannot open '$fout': $!.");
        return;
    }
    my @lines = <$fh>;
    foreach my $line (@lines) { $fh_log->print($line) }
    $fh->close();
    openurl("$fout");
    hitcr();
    return;
}

sub run_tidyall {
    my $fout = "tmp/tidyall.out";
    $rstatus->{'TIDY'} = 'TBD';

    # running with any .perltidyrc file
    my $cmd = "tidyall -a >$fout";
    system_echo($cmd);

    my $fh;
    if ( !open( $fh, '<', $fout ) ) {
        hitcr("Strange: cannot open '$fout': $!.");
        return;
    }
    my @lines = <$fh>;
    foreach my $line (@lines) { $fh_log->print($line) }

    # FIXME: haven't tried to look for errors yet
    my @errors;

    #my @errors = grep { !/source OK\s*$/ } @lines;

    $fh->close();
    if ( !@errors ) {
        $rstatus->{'TIDY'} = 'OK';
        $rstatus->{'PC'}   = 'TBD';
        hitcr("Source OK.");
        return;
    }
    openurl("$fout");
    return;
}

sub run_convergence_tests {
    my $fout = "tmp/run_convergence_tests.out";
    $rstatus->{'CONV'} = 'TBD';

    # running with any .perltidyrc file
    my $cmd = "./dev-bin/run_convergence_tests.pl >$fout 2>>$fout";
    system_echo($cmd);

    my $fh;
    if ( !open( $fh, '<', $fout ) ) {
        hitcr("Strange: cannot open '$fout': $!.");
        return;
    }
    my @lines = <$fh>;
    foreach my $line (@lines) { $fh_log->print($line) }
    my $error = $lines[-1] !~ /OK/;

    $fh->close();
    if ( !$error ) {
        $rstatus->{'CONV'} = 'OK';
        hitcr("Convergence check OK.");
        return;
    }
    openurl("$fout");
    return;
}

sub run_tokenizer_tests {
    my $fout = "tmp/run_tokenizer_tests.out";
    $rstatus->{'TOK'} = 'TBD';

    # running with any .perltidyrc file
    my $cmd = "./dev-bin/run_tokenizer_tests.pl >$fout 2>>$fout";
    system_echo($cmd);

    my $fh;
    if ( !open( $fh, '<', $fout ) ) {
        hitcr("Strange: cannot open '$fout': $!.");
        return;
    }
    my @lines = <$fh>;
    foreach my $line (@lines) { $fh_log->print($line) }
    my $error = $lines[-1] !~ /OK/;

    $fh->close();
    if ( !$error ) {
        $rstatus->{'TOK'} = 'OK';
        hitcr("Tokenizer check OK.");
        return;
    }
    openurl("$fout");
    return;
}

sub run_perl_critic {
    my $pcoutput = "tmp/perlcritic.out";
    $rstatus->{'PC'} = 'TBD';

    # running with parameters in .perlcritic
    my $cmd = "perlcritic lib/Perl/ >tmp/perlcritic.out";
    system_echo($cmd);
    my $fh;
    if ( !open( $fh, '<', $pcoutput ) ) {
        hitcr("Strange: cannot open '$pcoutput': $!.");
        return;
    }
    my @lines  = <$fh>;
    my @errors = grep { !/source OK\s*$/ } @lines;
    foreach my $line (@lines) { $fh_log->print($line) }
    $fh->close();

    if ( !@errors ) {
        $rstatus->{'PC'} = 'OK';
        hitcr("Source OK.");
        return;
    }
    openurl("$pcoutput");
    return;
}

sub make_tests {

    my $result;
    $result = sys_command("perl Makefile.PL");
    $result = sys_command("make");
    unless ( -e "Makefile" ) { query("Makefile missing..hit <cr"); return }
    $result = sys_command("make test");
    print $result;
    $rstatus->{'T'} = $result =~ 'Result: PASS' ? 'OK' : 'TBD';
    hitcr();
    return $rstatus->{'T'};
}

sub make_docs {

# Need to figure out if make fails.  For now I'm looking for 'Stop' as in
# this error:
# make: *** No rule to make target 'tutorial.pod', needed by 'tutorial.html'.  Stop.

    my @errors;
    foreach my $file (
        qw(
        lib/Perl/Tidy.pod
        bin/perltidy
        )
      )
    {
        my $errfile = "tmp/podchecker.err";
        my $result  = sys_command("podchecker $file 2>$errfile");

        #if ( $result) {
        my $fh;
        open( $fh, '<', $errfile ) || die "cannot open $errfile: $!\n";
        my $saw_error;
        foreach my $line (<$fh>) {
            $fh_log->print($line);
            if ( $line =~ /error/i ) {
                $saw_error = 1;
            }
        }
        $fh->close();
        push @errors, $file if ($saw_error);

    }
    if (@errors) {
        local $" = ') (';
        print "These file(s) had errors: (@errors)\n";
        hitcr("See the log file");
        $rstatus->{'DOCS'} = 'TBD';
        return;
    }

    # finish up
    my $result = sys_command("(cd docs; make)");
    print $result;
    my $status = $result =~ /Stop\./i ? 'TBD' : 'OK';
    $rstatus->{'DOCS'} = $status;
    hitcr();
    return;
}

sub make_manifest {

    my $fout   = "tmp/manifest.out";
    my $result = sys_command("make manifest >$fout 2>$fout");
    my $status = "OK";
    $rstatus->{'MANIFEST'} = $status;
    post_result($fout);
    return;
}

sub make_dist {
    my $result;

    if ( $rstatus->{'T'} !~ /^OK$/ ) {
        make_tests();
    }
    if ( $rstatus->{'T'} !~ /^OK$/ ) {
        hitcr("Problem with tests .. no .tar.gz.");
    }
    $result = sys_command("make dist");
    print $result;

    my ( $tar_gz_file, $created_VERSION );
    if ( $result =~ /Created (Perl-Tidy-(.*)\.tar\.gz)$/ ) {
        $tar_gz_file     = $1;
        $created_VERSION = $2;
    }
    else {
        hitcr("can't find the .tar.gz");
        return;
    }
    if ( !-e $tar_gz_file ) {
        hitcr("Strange, can't find '$tar_gz_file'");
        return;
    }

    $rstatus->{'DIST'} = 'OK';

    # Make a zip for new releases
    my $default = $created_VERSION =~ /\./ ? "N" : "Y";
    if ( ifyes( "OK. Make a .zip too? [Y/N], <cr>=$default", $default ) ) {
        make_zip($tar_gz_file);
    }

    if (
        ifyes(
            "run cpants_lint.pl to check $tar_gz_file? [Y/N], <cr>='Y'", "Y"
        )
      )
    {
        my $fout = "tmp/cpants_lint.out";
        if ( -e $fout ) { unlink $fout }
        my $cmd = "cpants_lint.pl $tar_gz_file >$fout 2>$fout";
        system_echo($cmd);
        post_result($fout);
    }
    return;
}

sub make_zip {

    my ($tar_gz_file) = @_;
    my $dir_name = $tar_gz_file;
    $dir_name =~ s/\.tar\.gz//;

    my $command;

    # clean out any old build in /tmp
    my $result = sys_command("rm -rf /tmp/$dir_name");

    # copy the file
    $result = sys_command("cp $tar_gz_file /tmp");

    # untar it
    $command = "(cd /tmp; tar xvfz $tar_gz_file;)";
    $result  = sys_command("$command");

    # zip it up
    my $zip_name = $dir_name . ".zip";
    $command = "(cd /tmp; zip -r -y -m -T -9 $zip_name $dir_name ;)";
    $result  = sys_command($command);

    # move it
    $result = sys_command("mv /tmp/$zip_name .");
    return;
}

sub update_version_number {

    my $reported_VERSION = $Perl::Tidy::VERSION;
    my $lib_path         = "lib/Perl/";
    my $bin_path         = "bin/";
    my @sources          = (
        $lib_path . "Tidy.pm",
        $lib_path . "Tidy.pod",
        $bin_path . "perltidy",
    );
    push @sources, "CHANGES.md";
    my @more = qw(
      Tidy/Debugger.pm
      Tidy/Diagnostics.pm
      Tidy/FileWriter.pm
      Tidy/Formatter.pm
      Tidy/HtmlWriter.pm
      Tidy/IOScalar.pm
      Tidy/IOScalarArray.pm
      Tidy/IndentationItem.pm
      Tidy/Logger.pm
      Tidy/Tokenizer.pm
      Tidy/VerticalAligner.pm
      Tidy/VerticalAligner/Alignment.pm
      Tidy/VerticalAligner/Line.pm
    );

    foreach my $module (@more) {
        push @sources, $lib_path . $module;
    }

    my $saw_pod = scan_for_pod(@sources);
    return if ($saw_pod);

    # I have removed this one; it was useful in development
    # CS  Check that Selected files have the current VERSION

  RETRY:
    print <<EOM;

A Release VERSION is an integer, the approximate YYMMDD of the release.
A Development VERSION is (Last Release).(Development Number)

The Development Number is a 2 digit number starting at 01 after a release is
continually bumped along at significant points during development.

The VERSION reported by Perl::Tidy.pm is '$reported_VERSION'
What would you like to do?

CA  Check that All files have the current VERSION
BV  Bump VERSION number by 0.01
RV  Make new Release Version (from date)
q   nothing; return to Main Menu
EOM

    my $ans = queryu(":");
    if ( $ans eq 'BV' ) {
        my $new_VERSION = get_new_development_version($reported_VERSION);
        next if ( $new_VERSION == $reported_VERSION );
        if ( ifyes("New version will be: '$new_VERSION'. OK? [Y/N]") ) {
            my $ok = update_all_sources( $new_VERSION, @sources );
            $rstatus->{'V'} = $ok ? 'OK' : 'TBD';
        }
        return;
    }
    elsif ( $ans eq 'RV' ) {
        my $new_VERSION = get_new_release_version($reported_VERSION);
        next if ( $new_VERSION == $reported_VERSION );
        if ( ifyes("New version will be: '$new_VERSION'. OK? [Y/N]") ) {
            my $ok = update_all_sources( $new_VERSION, @sources );
            $rstatus->{'V'} = $ok ? 'OK' : 'TBD';
        }
        return;
    }
    elsif ( $ans eq 'CA' ) {
        my $new_VERSION = $reported_VERSION;
        my $ok          = update_all_sources( $new_VERSION, @sources );
        $rstatus->{'V'} = $ok ? 'OK' : 'TBD';
        return;
    }

    # I have left this as a hidden menu item for testing
    # but it is not on the menu because it would be confusing
    elsif ( $ans eq 'CS' ) {
        my $new_VERSION = $reported_VERSION;
        my @check       = grep { ifyes("Check $_? [Y/N]") } @sources;
        update_all_sources( $new_VERSION, @check );
        return;
    }
    elsif ( $ans eq 'Q' || $ans eq 'X' ) {
        return;
    }
    goto RETRY if ( ifyes("?? I didn't get that, try again? [Y/N]") );
    return;
}

sub get_new_development_version {
    my ($reported_VERSION) = @_;
    my $new_VERSION        = $reported_VERSION;
    my @parts              = split /\./, $reported_VERSION;
    if ( @parts == 1 ) {

        # first development after release
        $parts[1] = "01";
    }
    elsif ( @parts == 2 ) {

        # bumping development version
        my $dv = $parts[1];
        if ( $dv !~ /^\d\d$/ ) {
            query("development version: '$dv'. Hit <Cr>");
            return;
        }
        if ( $dv == 99 ) {
            query(
"development version: '$dv' is maxed out. Do a release. Hit <Cr>"
            );
            return;
        }
        $parts[1]++;
    }
    else {
        query("Sorry: cannot interpret starting VERSION number\n");
        return;
    }

    $new_VERSION = join '.', @parts;
    return $new_VERSION;
}

sub get_new_release_version {
    my ($reported_VERSION) = @_;
    my $new_VERSION = $reported_VERSION;
    my ( $day, $month, $year ) = (localtime)[ 3, 4, 5 ];
    $year  += 1900;
    $month += 1;
    $month       = sprintf "%02d", $month;
    $day         = sprintf "%02d", $day;
    $new_VERSION = "$year$month$day";

    if ( !ifyes("Suggest VERSION $new_VERSION, OK [Y/N]") ) {
        $new_VERSION = query("Enter release VERSION:");
    }
    return $new_VERSION;
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

sub update_all_sources {
    my ( $new_VERSION, @sources ) = @_;

    # preliminary checks
    if ( !$new_VERSION ) {
        return;
    }
    foreach my $source_file (@sources) {

        if ( !-f $source_file ) {
            print <<EOM;
Sorry, the following source file is not a file
$source_file
Please fix this to be the actual file and rerun
EOM
            return;
        }

        if ( -l $source_file ) {
            print <<EOM;
Sorry, the following file is a symlink
$source_file
I don't want to edit links; Please fix this to point to the actual file and rerun
EOM
            return;
        }
    }

    my @unchanged;
    my @changed;
    while ( my $source_file = shift @sources ) {
        print "\nworking on $source_file...\n";
        if ( update_VERSION( $new_VERSION, $source_file ) ) {
            print "updated $source_file\n";
            push @changed, $source_file;
        }
        else {
            push @unchanged, $source_file;
        }
    }

    local $" = ') (';
    print <<EOM;
Changed:   (@changed);
unchanged: (@unchanged);
EOM
    if ( grep { $_ =~ /Tidy\.pm/ } @changed ) {

        my $runme = "RUNME_after_release.sh";
        make_tag_script( $new_VERSION, $runme );

        print <<EOM;
Since you changed Tidy.pm, you should add it and other unstaged files
to the repository, then do a 'git commit' and then tag. Something like 

git status
git add -A  [or whatever]
git commit
git tag -a $new_VERSION;

You should also push any new tags:

git push origin --tags

To avoid error, I put this last command in a script $runme 
EOM
        hitcr();
    }
    return 1;
}

sub scan_for_pod {

    # perltidy .pm files should not contain pod text.  The reason is that pod
    # is frequently used as a debugging aid to temporarily remove blocks of
    # code. Mixing pod for debugging and pod for documentation would be
    # confusing.  Any pod markers left in a .pm file are probably leftovers
    # from debugging and need to be removed.
    my (@sources) = @_;

    foreach my $source_file (@sources) {

        if ( !-f $source_file ) {
            print <<EOM;
Sorry, the following source file is not a file
$source_file
Please fix this to be the actual file and rerun
EOM
            return;
        }

        if ( -l $source_file ) {
            print <<EOM;
Sorry, the following file is a symlink
$source_file
I don't want to edit links; Please fix this to point to the actual file and rerun
EOM
            return;
        }
    }

    my @files_with_pod;
    while ( my $source_file = shift @sources ) {
        next unless ( $source_file =~ /\.pm$/ );
        my $result = qx/grep "^=cut" $source_file/;
        if ($result) {
            push @files_with_pod, $source_file;
        }
    }

    my $saw_pod = @files_with_pod;
    print <<EOM;
-------------------------------------------------------------------
Scanning for pod in .pm files...
EOM

    if ($saw_pod) {
        local $" = ') (';
        query(<<EOM);
Found files with pod text: (@files_with_pod);
The convention in perltidy is not to have pod code in '.pm' files.
Please remove the pod text before continuing, hit <cr> to continue.
-------------------------------------------------------------------
EOM
    }
    else {
        print <<EOM;
OK - no pod text found in .pm files
-------------------------------------------------------------------
EOM
    }

    return $saw_pod;
}

sub make_tag_script {
    my ( $new_VERSION, $runme ) = @_;
    if ( open( RUN, ">$runme" ) ) {
        print RUN <<EOM;
#!/bin/sh
git tag -a $new_VERSION
git push origin --tags
unlink \$0;
EOM
    }

    close RUN;
    system("chmod 0755 $runme");
}

sub update_VERSION {
    my ( $new_VERSION, $source_file ) = @_;

    # returns changed version line if successful
    # returns nothing if failure

    my $old_VERSION_line;
    my $new_VERSION_line;

    # write to a temporary
    my $tmpfile = $source_file . ".tmp";
    my $ftmp;
    if ( !open( $ftmp, '>', $tmpfile ) ) {
        query("cannot open $tmpfile: $!\n");
        return;
    }
    if ( !$ftmp ) { query("Could not get a temporary file"); return }
    my $fh;
    if ( !open( $fh, '<', $source_file ) ) {
        query("cannot open $source_file: $!\n");
        return;
    }
    my $in_pod;
    my $is_md_file  = $source_file eq 'CHANGES.md';
    my $is_pod_file = !$is_md_file && $source_file !~ /\.pm/;
    while ( my $line = <$fh> ) {

        # Look for and turn off any of the form:
        # DEVEL_MODE, DIAGNOSTICS, DEBUG_XXX, VERIFY_XXX, TEST_XXX, EXPLAIN_XXX
        if ( $line =~
/^(\s*use\s+constant\s+(?:DEBUG|DEVEL|DIAGNOSTICS|EXPLAIN|VERIFY|TEST)_[A-Z]+\s*)=>\s*(-?\d*);(.*)$/
          )
        {
            if ( $2 != 0 ) {
                $line = <<EOM;
$1 => 0;$3
EOM
            }
        }

        # finish writing after the change
        if ($old_VERSION_line) {
            $ftmp->print($line);
            next;
        }

        # looking for VERSION in pod
        if ($is_pod_file) {
            $in_pod = $in_pod ? $line !~ /^=cut/ : $line =~ /^=/;
            if ($in_pod) {

                # perltidy and Tidy.pod have lines like this
                if ( $line =~ /(This man page documents.*version\s*)(.*)/ ) {

                    $old_VERSION_line = $line;
                    chomp $old_VERSION_line;
                    $new_VERSION_line = $1 . $new_VERSION;
                    $line             = $new_VERSION_line . "\n";
                }

                # ChangeLog.pod has a line like this:
                # =head2 2018 xx xx
                elsif ( $line =~ /=head2 \d\d\d\d/ ) {
                    $old_VERSION_line = $line;
                    chomp $old_VERSION_line;
                    my $spaced_new_VERSION = $new_VERSION;
                    if ( $spaced_new_VERSION =~ /(\d\d\d\d)(\d\d)(\d\d.*)/ ) {
                        $spaced_new_VERSION = "$1 $2 $3";
                    }
                    $new_VERSION_line = "=head2 $spaced_new_VERSION";
                    $line             = $new_VERSION_line . "\n";
                }
            }
        }

        # looking for VERSION in markdown
        elsif ($is_md_file) {

            # CHANGES.md has a line like this:
            # ## 2018 xx xx
            if ( $line =~ /## \d\d\d\d/ ) {
                $old_VERSION_line = $line;
                chomp $old_VERSION_line;
                my $spaced_new_VERSION = $new_VERSION;
                if ( $spaced_new_VERSION =~ /(\d\d\d\d)(\d\d)(\d\d.*)/ ) {
                    $spaced_new_VERSION = "$1 $2 $3";
                }
                $new_VERSION_line = "## $spaced_new_VERSION";
                $line             = $new_VERSION_line . "\n";
            }
        }

        # looking for version in module
        else {

            # Looking for something simple like this, with or without quotes,
            # with semicolon and no sidecomments:
            #                     $VERSION   =   "20180202.245"  ;
            #                 our $VERSION   =    20104202       ;
            if ( $line =~
                /^((our)?\s*\$VERSION\s*=\s*\'?)  ([^'#]+)   (\'?) \s* ;/x )
            {
                $old_VERSION_line = $line;
                chomp $old_VERSION_line;
                $new_VERSION_line = $1 . $new_VERSION . $4 . ";";
                $line             = $new_VERSION_line . "\n";
            }
        }
        $ftmp->print($line);
    }

    $ftmp->close();

    # Report results
    if ( !$old_VERSION_line ) {
        query("could not find old VERSION in file!");
        unlink $tmpfile;
        return;
    }

    print <<EOM;
OLD line: $old_VERSION_line
NEW line: $new_VERSION_line
EOM
    if ( $old_VERSION_line eq $new_VERSION_line ) {
        query("OK. Lines are the same. Nothing to do here.");
        unlink $tmpfile;
        return;
    }
    if ( ifyes("OK. Continue and make this change? [Y/N]") ) {
        my $input_file_permissions = ( stat $source_file )[2] & oct(7777);
        if ($input_file_permissions) {

            # give output script same permissions as input script, but
            # be sure it is user-writable
            chmod( $input_file_permissions | oct(600), $tmpfile );
        }
        rename( $tmpfile, $source_file )
          or query("problem renaming $tmpfile to $source_file:  $!\n");
        return $new_VERSION_line;
    }

    unlink $tmpfile;
    return;
}

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
}

sub system_echo {
    my ( $cmd, $quiet ) = @_;
    print "$cmd\n" unless ($quiet);
    system $cmd;
    return;
}

sub sys_command {
    my $cmd = shift;
    print ">>> $cmd\n";

    $fh_log->print(">>> $cmd");
    my $result = qx{$cmd};
    if ($result) {
        $fh_log->print($result);

        #print LOGFILE $result }
    }

    return $result;
}
__END__

OLD SCRIPT FOLLOWS, FOR REFERENCE
#!/usr/bin/perl -w
use strict;
#
# This script creates the perltidy distribution files
# TBD:
#  - add this stuff to CVS
my $result;
my $VERSION;
my $DEBIAN;  # undefined, no longer doing debian

# -----------------------------------------------------------------------
# go through the CHECKLIST
# -----------------------------------------------------------------------
system ("less CHECKLIST");
print STDOUT "Continue? [Y/N]\n";
my $ans=<STDIN>;
exit -1 unless ($ans =~ /^[Yy]/); 
open LOGFILE, "> makedist.out";

# -----------------------------------------------------------------------
# Build the distribution files
# -----------------------------------------------------------------------
#
# copy the 'perltidy' script over, removing the built-in path
$result = sys_command("./nodist/fix_perltidy <../src/perltidy >bin/perltidy");

# update the doc files
$result = sys_command("(cd ../docs; make)");

# use MakeMaker to build the distribution
$result = sys_command("perl Makefile.PL");
$result = sys_command("make");
$result = sys_command("make test");
unless ( -e "Makefile" ) { die "Makefile missing..\n" }
$result = sys_command("make dist");

# get the VERSION from the output
if ( $result =~ /Perl-Tidy-(\d+)/ ) {
    $VERSION=$1;
    print "version is : $VERSION\n";
}
my $dir_name = "Perl-Tidy-$VERSION";
my $tar_gz_file = "$dir_name" . ".tar.gz";
my $tgz_file = "$dir_name" . ".tgz";
my $command;

# clean out any old build in /tmp
$result = sys_command("rm -rf /tmp/$dir_name");

# move the file
$result = sys_command("mv $tar_gz_file /tmp");

# untar it
$command = "(cd /tmp; tar xvfz $tar_gz_file;)";
$result = sys_command("$command");

# -----------------------------------------------------------------------
# set file permissions
# -----------------------------------------------------------------------
# fix permissions to be:
# 0644 - text files
# 0755 - directories and executables

# first walk through the manifest and set all to 0644
open MANIFEST, "< MANIFEST" or die "cannot open MANIFEST: $!\n";
while (my $line=<MANIFEST>) {
	$line=~s/^\s+//;

	# remove excess text from lines
	my $file=split /\s+/, $line;
        sys_command("chmod 0644 /tmp/$dir_name/$file");
}

# then go back and set binaries to 0755
sys_command("chmod 0755 /tmp/$dir_name/pm2pl");
sys_command("chmod 0755 /tmp/$dir_name/bin/*");
if ($DEBIAN) {
	sys_command("chmod 0755 /tmp/$dir_name/debian/rules");
}

# -----------------------------------------------------------------------
# rebuild the tar file, which will get correct permissions
# -----------------------------------------------------------------------
$command = " (cd /tmp; rm $tar_gz_file; tar cvf - $dir_name | gzip -9 > $tar_gz_file ;)";
$result  = sys_command("$command");

# -----------------------------------------------------------------------
# make the .zip file
# -----------------------------------------------------------------------
#
##?# OLD: change line endings for windows
##?# $command .= " flipall $dir_name -m; cd $dir_name;";
##?# my @dirs = qw( bin lib docs t examples);
##?# TESTING foreach (@dirs) { $command .= " flipall $_ -m;" }
##?
##?# this works
##?# perl -Mopen=OUT,:crlf -pi.bak -e0 filename
##?# $command .= " cd .. ; zip -r -y -m -T -9 $zip_name $dir_name ; ";

# zip it up
my $zip_name = $dir_name . ".zip";
$command = "(cd /tmp; zip -r -y -m -T -9 $zip_name $dir_name ;)";
$result = sys_command($command);

###$command = " (cd /tmp ; zip -r -y -m -T -9 $zip_name $dir_name ; ";
##3#$command .= ")";
##print STDERR "$command", "\n";
##exit 1;

# -----------------------------------------------------------------------
# Make the debian package
# TODO: go through and fix error messages
# -----------------------------------------------------------------------
if ($DEBIAN) {
    my $debian_package  = 'perltidy';
    my $debian_dir_name = "$debian_package-$VERSION";
    $command = "(cd /tmp ; mkdir deb ; cd deb ; tar xvfz ../$tar_gz_file;)";
    $result  = sys_command($command);
    $command = "(cd /tmp/deb; mv $dir_name $debian_dir_name)";
    $result  = sys_command($command);
    $command =
      "(cd /tmp/deb/$debian_dir_name ; dpkg-buildpackage -uc -d -rfakeroot ;)";
    $result = sys_command($command);
    my $deb_name = $debian_package . '_' . $VERSION . '-1_all.deb';
    $result = sys_command("mv /tmp/deb/$deb_name /tmp");
    $result = sys_command("rm -rf /tmp/deb");
    $result = sys_command("mv /tmp/$deb_name ../archive");
}

# -----------------------------------------------------------------------
# move the files over to the archive area
# -----------------------------------------------------------------------
$result = sys_command("mv /tmp/$tar_gz_file ../archive");
$result = sys_command("mv /tmp/$zip_name ../archive");

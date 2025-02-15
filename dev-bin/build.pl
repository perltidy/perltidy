#!/usr/bin/perl
use strict;
use warnings;
use Perl::Tidy;
use File::Copy;
use File::Temp qw(tempfile);
use File::Basename;
use English;

use constant EMPTY_STRING => q{};
use constant SPACE        => q{ };
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
my $source_VERSION = $Perl::Tidy::VERSION;

# These are the main steps, in approximate order, for making a new version
# Note: Since perl critic is in the .tidyallrc, a separate 'PC' step is not
# needed
my $rsteps =
  [qw( CHK CONV TOK SCAN MAN V YEAR PC TIDY T CL DOCS MANIFEST DIST )];

my $rstatus = {};
foreach my $step ( @{$rsteps} ) { $rstatus->{$step} = 'TBD' }

my $rcode = {
    'CHK' => sub {
        openurl("local-docs/Release-Checklist.md")
          unless $rstatus->{CHK} eq 'OK';
        $rstatus->{CHK} = 'OK';
    },
    'SCAN'     => \&scan_for_bad_characters,
    'MAN'      => \&check_man_pages,
    'V'        => \&update_version_number,
    'YEAR'     => \&update_copyright_date,
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
        print <<EOM;
-------------------------------------------
Perltidy Build Main Menu - Case Insensitive
-------------------------------------------

chk      - view release CHecKlist          status: $rstatus->{'CHK'}
scan     - scan text for problems          status: $rstatus->{'SCAN'}
man      - check man pages                 status: $rstatus->{'MAN'}
v        - check/update Version Number     status: $rstatus->{'V'}
year     - check/update copyright Year     status: $rstatus->{'YEAR'}
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
        else {
            ## unknown response
        }
    } ## end while (1)
    return;
} ## end sub main

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
} ## end sub post_result

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
} ## end sub run_tidyall

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
} ## end sub run_convergence_tests

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
} ## end sub run_tokenizer_tests

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
} ## end sub run_perl_critic

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
} ## end sub make_tests

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
        my $errfile   = "tmp/podchecker.err";
        my $result_uu = sys_command("podchecker $file 2>$errfile");

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
} ## end sub make_docs

sub make_manifest {

    my $fout   = "tmp/manifest.out";
    my $fdiff  = "tmp/manifest.diff";
    my $result = sys_command("make manifest >$fout 2>$fout");
    post_result($fout);
    if ( -e $fdiff ) { unlink $fdiff }
    $result = system("diff MANIFEST.bak MANIFEST >$fdiff");
    if ( !-e $fdiff || -z $fdiff) {
        query("No changes to MANIFEST; hit <cr>\n");
    }
    else {
        post_result($fdiff);
        query("MANIFEST has changed, please check; hit <cr>\n");
    }
    my $status = "OK";
    $rstatus->{'MANIFEST'} = $status;
    return;
} ## end sub make_manifest

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

    query(<<EOM);
Next Steps:
- Untar and examine the contents of the .tar.gz file in /tmp
- Do a test install on perl-5.8.1 with these commands:
   perlbrew
   use perl-5.8.1
   perl Makefile.pl
   ...
- Start random testing with the new version
- Compare results on a variety of test cases with previous CPAN release
- Use NYTProf to check the efficiency of any new coding
hit <cr> to continue
EOM

    if (
        ifyes(
            "run cpants_lint.pl to check $tar_gz_file? [Y/N], <cr>='Y'", "Y"
        )
      )
    {
        my $fout = "tmp/cpants_lint.out";
        if ( -e $fout ) { unlink($fout) }
        my $cmd = "cpants_lint.pl $tar_gz_file >$fout 2>$fout";
        system_echo($cmd);
        post_result($fout);
    }
    return;
} ## end sub make_dist

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
} ## end sub make_zip

sub get_modules {

    # TODO: these could be obtained from MANIFEST
    my @modules = qw(
      lib/Perl/Tidy.pm
      lib/Perl/Tidy/Debugger.pm
      lib/Perl/Tidy/Diagnostics.pm
      lib/Perl/Tidy/FileWriter.pm
      lib/Perl/Tidy/Formatter.pm
      lib/Perl/Tidy/HtmlWriter.pm
      lib/Perl/Tidy/IOScalar.pm
      lib/Perl/Tidy/IOScalarArray.pm
      lib/Perl/Tidy/IndentationItem.pm
      lib/Perl/Tidy/Logger.pm
      lib/Perl/Tidy/Tokenizer.pm
      lib/Perl/Tidy/VerticalAligner.pm
      lib/Perl/Tidy/VerticalAligner/Alignment.pm
      lib/Perl/Tidy/VerticalAligner/Line.pm
    );
    return \@modules;
} ## end sub get_modules

sub scan_for_bad_characters {

    $rstatus->{'SCAN'} = 'TBD';
    my $rmodules = get_modules();
    my $saw_pod  = scan_for_pod($rmodules);

    print <<EOM;
Scanning for tabs, non-ascii, and line-ending spaces ...
EOM

    my $errors = EMPTY_STRING;
    foreach my $file ( @{$rmodules} ) {
        my $string = slurp_to_string($file);

        # Non-ascii characters in perltidy modules slows down formatting
        my $line_list = get_non_ascii_lines( $string, 5 );
        if ($line_list) {
            $errors .= "$file has non-ascii characters at $line_list\n";
        }

        # Tabs and line-ending spaces are sometimes left by an editor.
        # Usually formatting removes them, but not if they are in some
        # kind of quoted text.
        $line_list = get_tab_lines( $string, 5 );
        if ($line_list) {
            $errors .= "$file has tab characters at $line_list\n";
        }
        $line_list = get_line_ending_space_lines( $string, 5 );
        if ($line_list) {
            $errors .= "$file has line-ending spaces at $line_list\n";
        }
    }
    if ($errors) {
        print $errors;
        ## Note: One way to locate tabs and ending spaces in File.pm is
        ## grep -P '\t' File.pm
        ## grep -P '\s$' File.pm
    }
    else {
        print "..none found\n";

    }
    if ( $saw_pod || $errors ) {
        query("hit <cr>\n");
    }
    else {
        $rstatus->{'SCAN'} = 'OK';
        query("OK, hit <cr>\n");
    }
    return;
} ## end sub scan_for_bad_characters

sub get_non_ascii_lines {

    my ( $string, ($max) ) = @_;

    # Return a string with a list of lines having non-ascii characters

    # Given:
    #   $string = the text of a file
    #   $max = optional maximum number of lines to show (default 20)
    if ( $string !~ /[^[:ascii:]]/ ) { return }

    if ( !defined($max) || $max <= 0 ) { $max = 20 }
    my $msg   = EMPTY_STRING;
    my $lno   = 0;
    my $count = 0;
    my @lines = split /^/, $string;
    foreach my $line (@lines) {
        $lno++;
        if ( $line =~ /[^[:ascii:]]/ ) {
            if ( $count > $max ) { $msg .= " ..."; last }
            $msg .= " $lno";
        }
    }
    return $msg;
} ## end sub get_non_ascii_lines

sub get_tab_lines {

    my ( $string, ($max) ) = @_;

    # Return a string with a list of lines having tab characters

    # Given:
    #   $string = the text of a file
    #   $max = optional maximum number of lines to show (default 20)
    if ( $string !~ /\t/ ) { return }

    if ( !defined($max) || $max <= 0 ) { $max = 20 }
    my $msg   = EMPTY_STRING;
    my $lno   = 0;
    my $count = 0;
    my @lines = split /^/, $string;
    foreach my $line (@lines) {
        $lno++;
        if ( $line =~ /\t/ ) {
            if ( $count > $max ) { $msg .= " ..."; last }
            $msg .= " $lno";
        }
    }
    return $msg;
} ## end sub get_tab_lines

sub get_line_ending_space_lines {

    my ( $string, ($max) ) = @_;

    # Return a string with a list of lines ending in a space

    # Given:
    #   $string = the text of a file
    #   $max = optional maximum number of lines to show (default 20)
    if ( $string !~ /([^\s]) $/m ) { return }

    if ( !defined($max) || $max <= 0 ) { $max = 20 }
    my $msg   = EMPTY_STRING;
    my $lno   = 0;
    my $count = 0;
    my @lines = split /^/, $string;
    foreach my $line (@lines) {
        $lno++;
        chomp $line;
        if ( $line =~ /\s$/ ) {
            if ( $count > $max ) { $msg .= " ..."; last }
            $msg .= " $lno";
        }
    }
    return $msg;
} ## end sub get_line_ending_space_lines

sub check_man_page_width {
    my ($file) = @_;

    # Show man pages wider than 80 characters. Sometimes this is unavoidable,
    # such as when illustrating a side comment > 80 chars, but these should
    # be kept to a minimum.
    return if ( !defined($file) || !-e $file );

    my ( $fh1, $tmpfile1 ) = File::Temp::tempfile();
    my $cmd1 = "pod2man $file >$tmpfile1";
    system_echo($cmd1);
    $fh1->close();
    my ( $fh2, $tmpfile2 ) = File::Temp::tempfile();
    my $cmd2 = "MANWIDTH=80; export MANWIDTH; man $tmpfile1 >$tmpfile2";
    system_echo($cmd2);
    $fh2->close();
    my $string = slurp_to_string($tmpfile2);
    my @lines  = split /^/, $string;
    my $long_lines;
    my $count = 0;

    foreach my $line (@lines) {
        $line =~ s/\s+$//;
        next if ( length($line) <= 80 );
        $count++;
        if ( $count <= 8 ) { $long_lines .= $line . "\n" }
    }
    if ($count) {
        print "File '$file' has $count lines >80 chars:\n";
        print $long_lines;
    }
    if ( -e $tmpfile1 ) { unlink($tmpfile1) }
    if ( -e $tmpfile2 ) { unlink($tmpfile2) }
    return;
} ## end sub check_man_page_width

sub update_table_of_negated_switches {
    my ($source_file) = @_;
    print "\nChecking table of negated switches...\n";

    # Make the list of negated switches
    my $tmp_pod = 'tmp/negated_switches.pod';
    my $cmd     = "t/snippets/dump_negated_switches.pl >$tmp_pod";
    if ( system($cmd) ) {
        query("could not dump negated switches to $tmp_pod\n");
        return;
    }
    my $part2_new = slurp_to_string($tmp_pod);

    # Get the header for this section.  It should be as follows, but is
    # extracted here to allow change.
    # my $switches_header = '=head1 SWITCHES WHICH MAY BE NEGATED';
    my @lines           = split /^/, $part2_new;
    my $switches_header = $lines[0];
    $switches_header =~ s/\s*//;

    # Read the pod file
    my $string = slurp_to_string($source_file);

    # Split the pod into three sections, with the middle section being
    # the section of negated switches.
    my $pos0 = 0;
    my $pos1 = index( $string, $switches_header );
    if ( $pos1 < 0 ) { query("cannot find first header\n"); return }
    my $pos2 = index( $string, '=head1', $pos1 + 1 );
    if ( $pos2 < $pos1 ) { query("cannot find second header\n"); return }
    my $part1 = substr( $string, $pos0, $pos1 );
    my $part2 = substr( $string, $pos1, $pos2 - $pos1 );
    my $part3 = substr( $string, $pos2 );

    # Any changes??
    if ( $part2_new eq $part2 ) {
        print "OK: Table of negated switches is up to date\n";
    }
    else {
        print "Table of negated switches needs to be updated..\n";
        my $string_new = $part1 . $part2_new . $part3;
        my $tmpfile    = "tmp/perltidy";
        spew_string_to_file( $string_new, $tmpfile );
        my $fdiff  = "tmp/perltidy.diff";
        my $result = sys_command("diff $source_file $tmpfile >$fdiff");
        if ($result) {
            query("Strange; Error running diff ...\n");
            return;
        }
        else {
            post_result($fdiff);
        }
        if ( ifyes("Update $source_file with the diffs shown? [Y/N]") ) {
            my $backup_file = $source_file . ".bak";
            if ( -e $backup_file ) {
                File::Copy::copy( $backup_file, "/tmp" )
                  or die(
                    "File::Copy failed trying to backup $backup_file: $OS_ERROR"
                  );
            }
            File::Copy::copy( $source_file, $backup_file )
              or
              die("File::Copy failed trying to backup $source_file: $OS_ERROR");
            rename( $tmpfile, $source_file )
              or die("problem renaming $tmpfile to $source_file:  $!\n");
            print
              "Moved $source_file to $backup_file and updated $source_file\n";
        }
        else {
            query(
"TBD: The new section is '$tmp_pod' and the updated pod file is '$tmpfile'\n"
            );
        }
    }
    return;
} ## end sub update_table_of_negated_switches

sub check_man_pages {

    $rstatus->{'MAN'} = 'TBD';
    my $man1 = "bin/perltidy";
    my $man3 = "lib/Perl/Tidy.pod";
    foreach my $file ( $man1, $man3 ) {
        print "Checking $file..\n";
        check_man_page_width($file);
    }
    update_table_of_negated_switches($man1);
    query("Man page scan complete, hit <cr>\n");
    $rstatus->{'MAN'} = 'OK';
    return;
} ## end sub check_man_pages

sub update_version_number {

    my $lib_path = "lib/Perl/";
    my $bin_path = "bin/";
    my @sources  = (
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

    my $saw_pod = scan_for_pod( \@sources );
    return if ($saw_pod);

    # I have removed this one; it was useful in development
    # CS  Check that Selected files have the current VERSION

  RETRY:
    print <<EOM;

A Release VERSION is an integer, the approximate YYYYMMDD of the release.
A Development VERSION is (Last Release).(Development Number)

The Development Number is a 2 digit number starting at 01 after a release is
continually bumped along at significant points during development.

The VERSION reported by Perl::Tidy.pm is '$source_VERSION'
What would you like to do?

CA  Check that All files have the current VERSION
BV  Bump VERSION number by 0.01
RV  Make new Release Version (from date)
q   nothing; return to Main Menu
EOM

    my $ans = queryu(":");
    if ( $ans eq 'BV' ) {
        my $new_VERSION = get_new_development_version();
        next if ( $new_VERSION == $source_VERSION );
        if ( ifyes("New version will be: '$new_VERSION'. OK? [Y/N]") ) {
            my $ok = update_all_sources( $new_VERSION, @sources );
            $rstatus->{'V'} = $ok ? 'OK' : 'TBD';
        }
        return;
    }
    elsif ( $ans eq 'RV' ) {
        my $new_VERSION = get_new_release_version();
        next if ( $new_VERSION == $source_VERSION );
        if ( ifyes("New version will be: '$new_VERSION'. OK? [Y/N]") ) {
            my $ok = update_all_sources( $new_VERSION, @sources );
            $rstatus->{'V'} = $ok ? 'OK' : 'TBD';
        }
        return;
    }
    elsif ( $ans eq 'CA' ) {
        my $new_VERSION = $source_VERSION;
        my $ok          = update_all_sources( $new_VERSION, @sources );
        $rstatus->{'V'} = $ok ? 'OK' : 'TBD';
        return;
    }

    # I have left this as a hidden menu item for testing
    # but it is not on the menu because it would be confusing
    elsif ( $ans eq 'CS' ) {
        my $new_VERSION = $source_VERSION;
        my @check       = grep { ifyes("Check $_? [Y/N]") } @sources;
        update_all_sources( $new_VERSION, @check );
        return;
    }
    elsif ( $ans eq 'Q' || $ans eq 'X' ) {
        return;
    }
    else {
        ## unknown response
    }
    goto RETRY if ( ifyes("?? I didn't get that, try again? [Y/N]") );
    return;
} ## end sub update_version_number

sub get_new_development_version {
    my $new_VERSION = $source_VERSION;
    my @parts       = split /\./, $source_VERSION;
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
} ## end sub get_new_development_version

sub get_new_release_version {
    my $new_VERSION = $source_VERSION;
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
} ## end sub get_new_release_version

sub update_copyright_date {

    # check/update copyright date in
    # bin/perltidy: 1 spot
    # lib/Perl/Tidy.pm: 2 places

    $rstatus->{'YEAR'} = 'TBD';
    my $reported_year = substr( $source_VERSION, 0, 4 );
    if ( !$reported_year || $reported_year !~ /\d\d\d\d/ ) {
        query("Cannot find reported year in $source_VERSION\n");
        return;
    }

    my $file_Tidy     = "lib/Perl/" . "Tidy.pm";
    my $file_perltidy = "bin/perltidy";
    foreach my $source_file ( $file_perltidy, $file_Tidy ) {
        my $string     = slurp_to_string($source_file);
        my $old_string = $string;
        $string =~
s/2000-(\d\d\d\d) by Steve Hancock/2000-$reported_year by Steve Hancock/g;
        if ( $string ne $old_string ) {
            my $diff_msg = compare_string_buffers( $old_string, $string, 3 );
            print <<EOM;
===================================================
Copyright year needs to be updated in $source_file:
$diff_msg
===================================================
EOM
            if ( ifyes("OK. Continue and make this change? [Y/N]") ) {
                my $tmpfile = $source_file . ".tmp";
                spew_string_to_file( $string, $tmpfile );
                my $input_file_permissions =
                  ( stat $source_file )[2] & oct(7777);
                if ($input_file_permissions) {

                    # give output file same permissions as input file, but
                    # be sure it is user-writable
                    chmod( $input_file_permissions | oct(600), $tmpfile );
                }
                my $backup_file = $source_file . ".bak";
                File::Copy::copy( $source_file, $backup_file )
                  or die(
                    "File::Copy failed trying to backup $source_file: $OS_ERROR"
                  );
                rename( $tmpfile, $source_file )
                  or die("problem renaming $tmpfile to $source_file:  $!\n");
                print
"Moved $source_file to $backup_file and updated $source_file\n";
                $rstatus->{'YEAR'} = 'OK';
            }
        }
        else {
            print
"Copyright year and version year are '$reported_year' in $source_file\n";
            $rstatus->{'YEAR'} = 'OK';
        }
    }
    query("hit <cr> to continue\n");
    return;
} ## end sub update_copyright_date

sub slurp_to_string {
    my ($filename) = @_;
    my $buf;
    if ( open( my $fh, '<', $filename ) ) {
        local $INPUT_RECORD_SEPARATOR = undef;
        $buf = <$fh>;
        $fh->close() or Warn("Cannot close $filename\n");
    }
    else {
        Warn("Cannot open $filename: $OS_ERROR\n");
        return;
    }
    return $buf;
} ## end sub slurp_to_string

sub spew_string_to_file {

    my ( $string, $fname ) = @_;

    # write to a temporary
    my $ftmp;
    if ( !open( $ftmp, '>', $fname ) ) {
        query("cannot open $fname: $!\n");
        return;
    }
    if ( !$ftmp ) { query("Could not get a temporary file"); return }
    $ftmp->print($string);
    $ftmp->close();
    return;
} ## end sub spew_string_to_file

sub query {
    my ($msg) = @_;
    print $msg;
    my $ans = <STDIN>;
    chomp $ans;
    return $ans;
} ## end sub query

sub queryu {
    return uc query(@_);
}

sub hitcr {
    my ( ($msg) ) = @_;
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

sub line_diff {

    my ( $s1, $s2 ) = @_;

    # Given two strings, Return
    #  $diff_marker = a string with caret (^) symbols indicating differences
    #  $pos1 = character position of first difference; pos1=-1 if no difference

    # Form exclusive or of the strings, which has null characters where strings
    # have same common characters so non-null characters indicate character
    # differences.
    my $diff_marker = EMPTY_STRING;
    my $pos         = -1;
    my $pos1        = -1;
    if ( defined($s1) && defined($s2) ) {
        my $mask = $s1 ^ $s2;

        while ( $mask =~ /[^\0]/g ) {
            my $pos_last = $pos;
            $pos = $LAST_MATCH_START[0];
            if ( $pos1 < 0 ) { $pos1 = $pos; }
            $diff_marker .= SPACE x ( $pos - $pos_last - 1 ) . '^';

            # we could continue to mark all differences, but there is no point
            last;
        } ## end while ( $mask =~ /[^\0]/g)
    }
    return ( $diff_marker, $pos1 );
} ## end sub line_diff

sub compare_string_buffers {

    my ( $string_i, $string_o, ($max_diff_count) ) = @_;

    # Compare input and output string buffers and return a brief text
    # description of the first difference.

    # Given:
    #   $string_i = input string, or ref to input string
    #   $string_o = output string, or ref to output string
    #   $max_diff_count = optional maximum number of differences to show,
    #       default=1
    # Return:
    #   a string showing differences

    my $rbufi = ref($string_i) ? $string_i : \$string_i;
    my $rbufo = ref($string_o) ? $string_o : \$string_o;

    if ( !defined($max_diff_count) ) { $max_diff_count = 1 }

    my ( @aryi, @aryo );
    my ( $leni, $leno ) = ( 0, 0 );
    if ( defined($rbufi) ) {
        $leni = length( ${$rbufi} );
        @aryi = split /^/, ${$rbufi};
    }
    if ( defined($rbufo) ) {
        $leno = length( ${$rbufo} );
        @aryo = split /^/, ${$rbufo};
    }
    my $nlines_i = @aryi;
    my $nlines_o = @aryo;
    my $msg      = <<EOM;
Input   file length has $leni chars in $nlines_i lines
Output  file length has $leno chars in $nlines_o lines
EOM
    return $msg unless ( $leni && $leno );

    my $truncate = sub {
        my ( $str, $lenmax ) = @_;
        if ( length($str) > $lenmax ) {
            $str = substr( $str, 0, $lenmax ) . "...";
        }
        return $str;
    }; ## end $truncate = sub

    my $last_nonblank_line  = EMPTY_STRING;
    my $last_nonblank_count = 0;

    # loop over lines until we find a difference
    my $count      = 0;
    my $diff_count = 0;
    while ( @aryi && @aryo ) {
        $count++;
        my $linei = shift @aryi;
        my $lineo = shift @aryo;
        chomp $linei;
        chomp $lineo;
        if ( $linei eq $lineo ) {
            if ( length($linei) ) {
                $last_nonblank_line  = $linei;
                $last_nonblank_count = $count;
            }
            next;
        }

        #---------------------------
        # lines differ ... finish up
        #---------------------------
        my ( $line_diff, $pos1 ) = line_diff( $linei, $lineo );
        my $ch1    = $pos1 + 1;
        my $reason = "Files first differ at character $ch1 of line $count";

        my ( $leading_ws_i, $leading_ws_o ) = ( EMPTY_STRING, EMPTY_STRING );
        if ( $linei =~ /^(\s+)/ ) { $leading_ws_i = $1; }
        if ( $lineo =~ /^(\s+)/ ) { $leading_ws_o = $1; }
        if ( $leading_ws_i ne $leading_ws_o ) {
            $reason .= "; leading whitespace differs";
            if ( $leading_ws_i =~ /\t/ ) {
                $reason .= "; input has tab char";
            }
        }
        else {
            my ( $trailing_ws_i, $trailing_ws_o ) =
              ( EMPTY_STRING, EMPTY_STRING );
            if ( $linei =~ /(\s+)$/ ) { $trailing_ws_i = $1; }
            if ( $lineo =~ /(\s+)$/ ) { $trailing_ws_o = $1; }
            if ( $trailing_ws_i ne $trailing_ws_o ) {
                $reason .= "; trailing whitespace differs";
            }
        }
        $msg .= $reason . "\n";

        # limit string display length
        if ( $pos1 > 60 ) {
            my $drop = $pos1 - 40;
            $linei     = "..." . substr( $linei, $drop );
            $lineo     = "..." . substr( $lineo, $drop );
            $line_diff = SPACE x 3 . substr( $line_diff, $drop );
        }
        $linei              = $truncate->( $linei,              72 );
        $lineo              = $truncate->( $lineo,              72 );
        $last_nonblank_line = $truncate->( $last_nonblank_line, 72 );

        if ($last_nonblank_line) {
            $msg .= <<EOM;
 $last_nonblank_count:$last_nonblank_line
EOM
        }
        $line_diff = SPACE x ( 2 + length($count) ) . $line_diff;
        $msg .= <<EOM;
<$count:$linei
>$count:$lineo
$line_diff
EOM
        $diff_count++;
        last if ( $diff_count >= $max_diff_count );
    } ## end while ( @aryi && @aryo )

    if ($diff_count) { return $msg }

    #------------------------------------------------------
    # no differences found, see if one file has fewer lines
    #------------------------------------------------------
    if ( $nlines_i > $nlines_o ) {
        $msg .= <<EOM;
Files initially match file but output file has fewer lines
EOM
    }
    elsif ( $nlines_i < $nlines_o ) {
        $msg .= <<EOM;
Files initially match file but input file has fewer lines
EOM
    }
    else {
        $msg .= <<EOM;
Text in lines of file match but checksums differ. Perhaps line endings differ.
EOM
    }
    return $msg;
} ## end sub compare_string_buffers

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
    } ## end while ( my $source_file =...)

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

    # Keep track of the current source version since it differs from the
    # running version.
    $source_VERSION = $new_VERSION;
    return 1;
} ## end sub update_all_sources

sub scan_for_pod {

    # perltidy .pm files should not contain pod text.  The reason is that pod
    # is frequently used as a debugging aid to temporarily remove blocks of
    # code. Mixing pod for debugging and pod for documentation would be
    # confusing.  Any pod markers left in a .pm file are probably leftovers
    # from debugging and need to be removed.

    # And they should not contain __END__ or __DATA__ sections. The reason
    # is that this would cause the pm2pl script to fail.

    my ($rsources) = @_;
    my (@sources)  = @{$rsources};

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
    my @files_with_END;
    my @files_with_DATA;
    my @files_with_FIXME;
    my @files_with_TODO;
    while ( my $source_file = shift @sources ) {
        next unless ( $source_file =~ /\.pm$/ );
        my $basename = basename($source_file);
        my $result = qx/grep "^=cut" $source_file/;
        if ($result) {
            push @files_with_pod, $basename;
        }
        $result = qx/grep "^__END__" $source_file/;
        if ($result) {
            push @files_with_END, $basename;
        }
        $result = qx/grep "^__DATA__" $source_file/;
        if ($result) {
            push @files_with_DATA, $basename;
        }
        $result = qx/grep "FIXME" $source_file/;
        if ($result) {
            push @files_with_FIXME, $basename;
        }
        $result = qx/grep "TODO" $source_file/;
        if ($result) {
            push @files_with_TODO, $basename;
        }
    } ## end while ( my $source_file =...)

    my $saw_pod   = @files_with_pod;
    my $saw_END   = @files_with_END;
    my $saw_DATA  = @files_with_DATA;
    my $saw_FIXME = @files_with_FIXME;
    my $saw_TODO  = @files_with_TODO;

    my $saw_problem;
    print <<EOM;
-------------------------------------------------------------------
Scanning .pm files for ##FIXME pod __END__  and __DATA__ ...
EOM
    if ($saw_FIXME) {
        $saw_problem = 1;
        local $" = ') (';
        query(<<EOM);
Found $saw_FIXME files with 'FIXME': (@files_with_FIXME);
These should be fixed if possible.
-------------------------------------------------------------------
EOM
    }

    if ($saw_pod) {
        $saw_problem = 1;
        local $" = ') (';
        query(<<EOM);
Found $saw_pod files with pod text: (@files_with_pod);
The convention in perltidy is not to have pod code in '.pm' files.
Please remove the pod text before continuing, hit <cr> to continue.
-------------------------------------------------------------------
EOM
    }
    if ($saw_END) {
        $saw_problem = 1;
        local $" = ') (';
        query(<<EOM);
Found $saw_END files with __END__: (@files_with_END);
The convention in perltidy is not to have an __END__ section in '.pm' files.
Please remove the __END__ text before continuing, hit <cr> to continue.
-------------------------------------------------------------------
EOM
    }
    if ($saw_DATA) {
        $saw_problem = 1;
        local $" = ') (';
        query(<<EOM);
Found $saw_DATA files with __DATA__: (@files_with_DATA);
The convention in perltidy is not to have an __DATA__ section in '.pm' files.
Please remove the __DATA__ text before continuing, hit <cr> to continue.
-------------------------------------------------------------------
EOM
    }
    if ($saw_TODO) {
        local $" = ', ';
        query(<<EOM);
Found $saw_TODO files with 'TODO': @files_with_TODO;
These are ok but should be checked.
-------------------------------------------------------------------
EOM
    }
    if ( !$saw_problem ) {
        print <<EOM;
OK
EOM
    }

    return $saw_problem;
} ## end sub scan_for_pod

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
} ## end sub make_tag_script

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
                else {
                    ## keep looking
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
    } ## end while ( my $line = <$fh> )

    $ftmp->close();

    # Report results
    if ( !$old_VERSION_line ) {
        query("could not find old VERSION in file!");
        unlink($tmpfile);
        return;
    }

    print <<EOM;
OLD line: $old_VERSION_line
NEW line: $new_VERSION_line
EOM
    if ( $old_VERSION_line eq $new_VERSION_line ) {
        query("OK. Lines are the same. Nothing to do here.");
        unlink($tmpfile);
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

    unlink($tmpfile);
    return;
} ## end sub update_VERSION

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

sub system_echo {
    my ( $cmd, ($quiet) ) = @_;
    print "$cmd\n" unless ($quiet);
    system $cmd;
    return;
} ## end sub system_echo

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
} ## end sub sys_command
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

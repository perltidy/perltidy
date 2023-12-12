#!/usr/bin/perl -w
use strict;
use warnings;
use File::Copy;
use Perl::Tidy;

my $tmp_dir;
my %Opts;

main();

sub main { #<<<
my $usage = <<EOM;

This utility runs perltidy on a database of stability test cases. 
It can:

  - run perltidy on the stability test cases
  - do maintenance of the database of test cases

There are four options: run [DEFAULT], pack, merge, unpack ( -r -p -m -u )

usage:

   $0 -h -u -m -p -r -s [ arg1 arg2 ...

where one parameter may be given to specify the operation to be done:

  -h prints this help
  -u unpack all or selected files or cases from the database 
     into the default or selected directory
  -m merge files given in arg list into the default or specified database
  -p packs files given in arg list into a new database
  -e overwrites old expect data with the new results for cases run
  -s write statistics on parameter frequency to stdout and exit
  -r runs the tests [DEFAULT operation]

  arg1 arg2 ... may contain: 

    a data file (.data extension, $0.data is default) for all options
    file names to pack (.in and .par extensions) for the -p and -m options
    case names to run (like b037) for the -r option (default is run all cases)
    a temp dir for output of -r and -u (must exist if given, ./tmp is default)
    
  For all options, if no data file is given, the default $0.data is used
  For the -r and -u options, if no case names are given then all cases are run
  For the -r and -u options, if no temp dir is given, the default ./tmp is used

Run test cases:
 $0 [ -r ] [ datafile ] [ case1 case2 ... ]
 run all cases unless specific case names are given

Unpack a database
 $0 -u [ datafile ] [ tmpdir ]

Pack a database
 $0 -p file1 file2 ... [ datafile ]

Merge into a database
 $0 -m file1 file2 ... [ datafile ]

EXAMPLES

 $0               -run latest version on all cases in the database;
                   this is the most common operation.
 $0  b939 b1145   -run only cases b939 and b1145 in the database
 $0  new.data     -run on all cases in the database 'new.data'
 $0 -u            -unpack the entire default database into ./tmp
 $0 -u b939       -unpack only case b939 from the default database into ./tmp
 $0 -u /tmp/xxx   -unpack the default database into /tmp/xxx, which must exist
 $0 -u new.data   -unpack files in new.data into ./tmp 
 $0 -m ./tmp/*    -merge files ./tmp/* into the default database
 $0 -p /tmp/xxx/* new.data   
             -pack files in /tmp/xxx into new.data [new.data must not exist]
EOM

# The numerous test cases are very small files, so to save space they are kept
# in sorted order in a single text file with extension .data. This reduces file
# size by about a factor of 10.

use Getopt::Long;

# here is a summary of the Getopt codes:
# <none> does not take an argument
# =s takes a mandatory string
# :s takes an optional string
# =i takes a mandatory integer
# :i takes an optional integer
# ! does not take an argument and may be negated
#  i.e., -foo and -nofoo are allowed
# a double dash signals the end of the options list
my @option_string = qw(
  e
  h
  m
  p
  r
  s
  u
);

%Opts = ();
if ( !GetOptions( \%Opts, @option_string ) ) {
    die "Programming Bug: error in setting default options";
}

if ( $Opts{h} ) {
    print $usage;
    exit 1;
}

# set default .data file and .expect file
my $db_fname     = $0 . ".data";
my $expect_fname = $0 . ".expect";

# set tmp dir - you may need to change this depending on setup
my $git_home = qx[git rev-parse --show-toplevel];
chomp $git_home;
$tmp_dir = $git_home . "/dev-bin/tmp";
if ( -e $tmp_dir && !-d $tmp_dir ) {
    print STDERR "'$tmp_dir' exists but is not a dir: please fix\n";
    exit 1;
}

if ( !$Opts{s} ) {
    print <<EOM;
default temporary output directory: 
$tmp_dir
EOM
}

# Sift through the args...
my @files;
my @cases;
my @dirs;
my @data_files;
my @unknown;
foreach (@ARGV) {
    if (/\.data$/) {
        push @data_files, $_;
    }
    elsif (-f) {
        if (/\.(in|par)$/) {
            push @files, $_;
        }
        else {
            push @unknown, $_;
        }
    }
    elsif (-d) {
        push @dirs, $_;
    }
    elsif (/^\w+$/) {
        push @cases, $_;
    }
    else {
        push @unknown, $_;
    }
}

# Quit if anything looks wrong
if (@unknown) {
    local $" = ')(';
    print STDERR "Unknown parameter(s): (@unknown)\n";
    exit 1;
}

if (@dirs) {
    if ( @dirs == 1 ) {

        # OK, one directory for output
        $tmp_dir = $dirs[0];
    }
    else {
        local $" = ')(';
        print STDERR "Multiple directories not allowed: (@dirs)\n";
        exit 1;
    }
}
else {

    # Using default tmp dir; make sure it exists
    make_tmp_dir();
}

if (@data_files) {
    if ( @data_files == 1 ) {

        # OK, one data base name
        $db_fname = $data_files[0];
    }
    else {
        local $" = ')(';
        print STDERR "Multiple data files not allowed: (@data_files)\n";
        exit 1;
    }
}
else {
    if ( !-f $db_fname ) {
        print STDERR "No data file given, and default $db_fname not found\n";
        exit 1;
    }
}

##############
# p: pack data
##############

if ( $Opts{'p'} ) {
    if ( -e $db_fname ) {
        print STDERR <<EOM;
ERROR: the file '$db_fname' exists.
To avoid accidentally overwriting data, the 'p' option must create a new file.
You can rename it if it looks ok.
EOM
        exit 1;
    }
    pack_data( $db_fname, \@files );
    exit 1;
}

# The remaining options -m -u -r require reading an existing database
my $rdata_files;
if ( -e $db_fname ) {
    $rdata_files = read_data_to_hash($db_fname);
}
else {

    print STDERR "Cannot find data file $db_fname\n";
    exit 1;
}

###################
# s: run statistics
###################

if ( $Opts{s} ) {
    dump_parameter_frequency($rdata_files);
    exit 1;
}

my $rexpect_files;
if ( -e $expect_fname ) {
    $rexpect_files = read_data_to_hash($expect_fname);
}
else {
    print STDERR "NOTE: Cannot find expect file $expect_fname\n";
}

###############
# m: merge data
###############

if ( $Opts{'m'} ) {
    merge_data( $db_fname, $rdata_files, \@files );
    exit 1;
}

################
# u: unpack data
################

if ( $Opts{'u'} ) {
    unpack_data( $rdata_files, \@cases, \@files );
    exit 1;
}

##############
# r: run tests
##############

if (@files) {
    print STDERR <<EOM;
Unexpected .in and .par files given in run mode
Any files need to first be pack or merged into a single data file
EOM
    exit 1;
}

my ($rexpect_cases_to_update) =
  run_test_cases( $rdata_files, $rexpect_files, \@cases );

# Option to rewrite expect data
if ( @{$rexpect_cases_to_update} ) {
    write_hash_to_data_file( $expect_fname, $rexpect_files );
}

exit 1;
}

sub run_test_cases {
    my ( $rdata_files, $rexpect_files, $rcases ) = @_;

    print "\nRun log...\n";
    my $rsources = {};
    my $rparams  = {};
    my $routputs = {};
    my @expect_cases_to_update;
    my @expect_differences;

    foreach my $fname ( keys %{$rdata_files} ) {
        if ( $fname =~ /^(.*)\.in$/ ) {
            my $case = $1;
            $rsources->{$case} = $rdata_files->{$fname};
        }
        elsif ( $fname =~ /^(.*)\.par$/ ) {
            my $case = $1;
            $rparams->{$case} = $rdata_files->{$fname};
        }
        else {

            # A foreign file seems to have entered the database
            print "ignoring unknown file '$fname'\n";
        }
    }

    # be sure we have both a .in and .par for each case
    my ( @missing_snames, @missing_pnames );
    foreach my $sname ( keys %{$rsources} ) {
        if ( !$rparams->{$sname} ) {
            push @missing_pnames, $sname;
        }
    }
    foreach my $pname ( keys %{$rparams} ) {
        if ( !$rsources->{$pname} ) {
            push @missing_snames, $pname;
        }
    }

    if (@missing_snames) {
        print STDERR
          "WARNING: missing .in files for these cases: @missing_snames\n";
    }
    if (@missing_pnames) {
        print STDERR
          "WARNING: Missing .par files for these cases: @missing_pnames\n";
    }
    exit 1 if ( @missing_snames || @missing_pnames );

    my $iteration_max = 6;
    my @failed_to_converge;
    my $opath = $tmp_dir . '/';

    my @selected_cases = keys %{$rsources};
    if ( @{$rcases} ) {
        @selected_cases = @{$rcases};
    }

    my @skipped_cases;
    my $run_count              = 0;
    my $iteration_total        = 0;
    my $iteration_maximum      = 0;
    my $iteration_maximum_case = "";
    foreach my $sname ( sort @selected_cases ) {

        # remove any old tmp files for this case
        my $str       = $opath . $sname . ".[0-9]";
        my @tmp_files = glob("$str");
        if (@tmp_files) {
            my $num = unlink @tmp_files;
            print "unlinked $num old files for case $sname\n";
        }

        my $pname = $sname;
        my $output;
        my $source = $rsources->{$sname};
        if ( !defined($source) ) {
            print "Skipping case $sname : not in database\n";
            push @skipped_cases, $sname;
            next;
        }
        my $params = $rparams->{$pname};
        my $stderr_string;
        my $errorfile_string;
        my @output_history;
        for ( my $iteration = 1 ; $iteration <= $iteration_max ; $iteration++ )
        {
            # FIXME: remove the -irc flag below when several test cases
            # are corrected to avoid out-of-bounds integer flags
            my $err = Perl::Tidy::perltidy(
                source      => \$source,
                destination => \$output,
                perltidyrc  => \$params,
                argv        => '-irc=1',  # no warnings messages on bad integers
                stderr    => \$stderr_string,
                errorfile => \$errorfile_string, # not used when -se flag is set
            );
            if ($stderr_string) {
                print STDERR "---------------------\n";
                print STDERR "<<STDERR>>\n$stderr_string\n";
                print STDERR "---------------------\n";
                print STDERR "The above error was received with case $sname\n";
                exit 1;
            }
            if ($errorfile_string) {
                print STDERR "---------------------\n";
                print STDERR "<<.ERR file>>\n$errorfile_string\n";
                print STDERR "---------------------\n";
                print STDERR "The above error was received with case $sname\n";
                exit 1;
            }
            if ($err) {
                print STDERR "error calling Perl::Tidy for case $sname\n";
                exit 1;
            }

            push @output_history, $output;

            if ( $output eq $source ) {
                $routputs->{$sname} = $output;
                my $msg    = "";
                my $expect = $rexpect_files->{$sname};
                if ( defined($expect) ) {
                    if ( $expect ne $output ) {

                        # option -e updates expected output
                        if ( $Opts{e} ) {
                            $msg = "UPDATING EXPECT";
                            push @expect_cases_to_update, $sname;
                            $rexpect_files->{$sname} = $output;
                        }
                        else {
                            push @expect_differences, $sname;
                            $msg = "FORMATTING CHANGED";
                        }
                    }
                }
                else {
                    $msg = "..NEW";
                    push @expect_cases_to_update, $sname;
                    $rexpect_files->{$sname} = $output;
                }
                print "$sname: converged on iteration $iteration $msg\n";

                $run_count++;
                $iteration_total += $iteration;
                if ( $iteration > $iteration_maximum ) {
                    $iteration_maximum      = $iteration;
                    $iteration_maximum_case = $sname;
                }

                last;
            }
            elsif ( $iteration < $iteration_max ) {
                $source = $output;
            }
            else {
                push @failed_to_converge, $sname;
                print "$sname: FAILED TO CONVERGE\n";

                my $it = 0;
                foreach my $ostring (@output_history) {
                    $it++;
                    my $basename = "$sname.$it";
                    my $ofile    = $opath . $basename;
                    write_file( $ofile, $ostring, 1 );
                }
                my $sfile = $opath . $sname . ".in";
                my $pfile = $opath . $sname . ".par";
                write_file( $sfile, $rsources->{$sname}, 1 );
                write_file( $pfile, $rparams->{$sname},  1 );
            }

            #push @olist, $basename;
        }
    }

    print "-" x 31 . "\n";
    if ( $run_count > 0 ) {
        my $iteration_mean = sprintf( "%.2f", $iteration_total / $run_count );
        my $spaces         = " " x length($iteration_maximum_case);
        print <<EOM;
<<Stats for $run_count runs>>
$spaces  converged on iteration $iteration_mean (average)
$iteration_maximum_case: converged on iteration $iteration_maximum    (max)
EOM
    }

    print "-" x 31 . "\n";
    if (@failed_to_converge) {

        print <<EOM;

ERROR SUMMARY: These cases failed to converge (see tmp/ for history):
@failed_to_converge
EOM
    }
    else {
        print <<EOM;

OK: all cases converged
EOM
    }
    if (@skipped_cases) {
        print <<EOM;
SKIPPED: these requested cases were not found in the database:
@skipped_cases
EOM
    }
    if (@expect_cases_to_update) {
        my $new_expect = @expect_cases_to_update;
        print <<EOM;
NEW: added $new_expect cases to .expect: @expect_cases_to_update
EOM
    }
    if (@expect_differences) {

        # Write first few differences
        my $count = 0;
        my $msg = "See the .in .par .new and .exp files in the ./tmp directory";
        foreach my $sname (@expect_differences) {
            $count++;
            if ( $count > 5 ) {
                $msg .= " for the first 5 cases";
                last;
            }
            my $sfile = $opath . $sname . ".in";
            my $pfile = $opath . $sname . ".par";
            my $nfile = $opath . $sname . ".new";
            my $efile = $opath . $sname . ".exp";
            write_file( $sfile, $rsources->{$sname},      1 );
            write_file( $pfile, $rparams->{$sname},       1 );
            write_file( $nfile, $routputs->{$sname},      1 );
            write_file( $efile, $rexpect_files->{$sname}, 1 );
        }

        print <<EOM;
DIFFERENCES: these cases gave unexpected output; use -e to update .expect:
@expect_differences
EOM
    }
    return ( \@expect_cases_to_update );
}

sub read_data_to_hash {
    my ($db_fname) = @_;

    # Read a single file containing multiple files which was created with
    #    tail -n+1 file1 file2 ...
    # and return a hash with the source split into the original files:
    #    $rdata_files->{$fname} = $string
    # where $fname is the file name and $string is its text

    my $rdata_files = {};
    if ( !-e $db_fname ) {
        print STDERR "Database $db_fname does not exist\n";
        exit 1;
    }

    my $dstring = get_string($db_fname);
    my @lines   = split /^/, $dstring;
    my $fname   = "";
    my $lno     = 0;
    my $string;

    foreach my $line (@lines) {
        chomp $line;
        $lno++;
        if ( $line =~ /^==>\s*([\w\.]+)\s*<==/ ) {
            if ($string) {
                chomp $string if ( $string =~ /\n\n/ );
                $rdata_files->{$fname} = $string;
            }
            $string = "";
            $fname  = $1;
        }
        else {
            $string .= $line . "\n";
        }
    }
    if ($string) {
        $rdata_files->{$fname} = $string;
    }
    return $rdata_files;
}

sub unpack_data {

    my ( $rdata_files, $rcases, $rfiles ) = @_;

    # unpack selected cases, files, or all of the database into
    # a temporary directory
    my $opath = $tmp_dir . '/';
    my @keys  = @{$rfiles};
    my $count = 0;
    foreach my $case ( @{$rcases} ) {
        push @keys, $case . "\.in";
        push @keys, $case . "\.par";
    }
    if ( !@keys ) { @keys = keys %{$rdata_files} }
    foreach my $key (@keys) {
        my $fname  = $opath . $key;
        my $string = $rdata_files->{$key};
        if ( !$string ) {
            print STDERR "could not find '$fname' in database\n";
        }
        else {
            $count++;
            write_file( $fname, $string );
        }
    }
    print "Wrote $count files to '$tmp_dir'\n";
    return;
}

sub merge_data {
    my ( $db_fname, $rold_data, $rfiles ) = @_;

    my $rnew_data = read_files_to_hash($rfiles);

    # Merge the two data hashes
    my $update_count = 0;
    my $new_count    = 0;
    foreach my $fname ( keys %{$rnew_data} ) {
        my $string = $rnew_data->{$fname};
        if ( defined( $rold_data->{$fname} ) ) {
            $update_count++;
        }
        else {
            $new_count++;
        }
        $rold_data->{$fname} = $string;
    }

    my $num_old = keys %{$rold_data};
    print <<EOM;
Merge results:
$update_count files updated
$new_count new files
$num_old total files in merged data
EOM

    write_hash_to_data_file( $db_fname, $rold_data );
    return;
}

sub dump_parameter_frequency {
    my ($rdata_files) = @_;

    # Dump a list of parameters and the number of times they appear
    my $rcount = {};

    foreach my $fname ( keys %{$rdata_files} ) {
        if ( $fname =~ /^(.*)\.par$/ ) {
            my $string = $rdata_files->{$fname};
            my @lines  = split /^/, $string;
            foreach my $line (@lines) {
                $line =~ s/^\s+//;
                $line =~ s/\s+$//;
                if ( $line && $line =~ /^-/ ) {
                    if ( $line =~ /^-+(\w[\w-]*)/ ) {
                        $rcount->{$1}++;
                    }
                }
            }
        }
    }

    # For a spreadsheet:
##  my @sorted_keys = sort { $rcount->{$b} <=> $rcount->{$a} } keys %{$rcount};
##  foreach my $key (@sorted_keys) {
##      print "$rcount->{$key}, $key\n";
##  }

    # For a script:
    use Data::Dumper;
    print Dumper($rcount);

    return;
}

sub pack_data {
    my ( $db_fname, $rfiles ) = @_;
    my $rdata = read_files_to_hash($rfiles);
    write_hash_to_data_file( $db_fname, $rdata );
    return;
}

sub read_files_to_hash {
    my ($rfiles) = @_;
    my $rdata = {};
    foreach my $file ( @{$rfiles} ) {
        my $string = get_string($file);
        if ( $file =~ /\/([^\/]+$)/ ) { $file = $1; }
        $rdata->{$file} = $string;
    }
    return $rdata;
}

sub write_hash_to_data_file {
    my ( $db_fname, $rdata ) = @_;
    my $nfiles = keys %{$rdata};
    print "packing $nfiles files into $db_fname\n";

    # Pack into a temporary file first,
    # if all goes well...
    #   backup old data file if it exists,
    #   then rename
    my $ostring;
    my $count = 0;
    foreach my $fname ( sort keys %{$rdata} ) {
        my $string = $rdata->{$fname};
        if ($count)                    { $ostring .= "\n"; }
        if ( $fname =~ /\/([^\/]+$)/ ) { $fname = $1; }
        $count++;
        $ostring .= "==> $fname <==\n";
        $ostring .= $string;
    }

    # Backup an existing database
    if ( -e $db_fname ) {
        my $backup_extension = ".bak";
        my $backup_name      = $db_fname . $backup_extension;
        if ( -f $backup_name ) {
            unlink($backup_name)
              or die(
"unable to remove previous '$backup_name' for -b option; check permissions: $!\n"
              );
        }

        # backup the old data file
        # use copy for symlinks, move for regular files
        if ( -l $db_fname ) {
            File::Copy::copy( $db_fname, $backup_name )
              or die("File::Copy failed trying to backup source: $!");
        }
        else {
            rename( $db_fname, $backup_name )
              or die("problem renaming $db_fname to $backup_name: $!\n");
        }
    }

    write_file( $db_fname, $ostring );
    print "Wrote $count files to $db_fname\n";
    return;
}

sub get_string {
    my ($file) = @_;
    open my $fh, '<', $file or die "cannot open $file: $!\n";
    local $/ = undef;
    my $string = <$fh>;
    close $fh;
    return $string;
}

sub make_tmp_dir {
    if ( !-d $tmp_dir ) {
        unless ( mkdir $tmp_dir ) {
            print STDERR "unable to create $tmp_dir\n";
            exit 1;
        }
        if ( !-d $tmp_dir ) {
            print STDERR "problem creating $tmp_dir\n";
            exit 1;
        }
    }
}

sub write_file {
    my ( $fname, $string, $msg ) = @_;
    open my $fh, '>', $fname or die "cannot open $fname: $!\n";
    $fh->print($string);
    $fh->close();
    print STDERR "Wrote $fname\n" if ($msg);
    return;
}

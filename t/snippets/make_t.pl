#!/usr/bin/perl -w
use strict;
use warnings;
use Perl::Tidy;
my $rtests;

my $ipath = 'expect/';

# The packing list file is used to keep the snippets packed in the
# same order each time, in order to avoid creating file differences
# when files are checked in to git.  Each of the snippet files
# also has a small packing list at the top, and the list can
# be obtained there instead.
my $fpacking_list = "packing_list.txt";

# Limit file size to simplify debugging
my $MAX_TESTS_PER_FILE = 20;

# This will combine all of the test snippets and expected outputs into test
# file(s) to be run upon installation.  Test files are named
# 'snippets1.t', 'snippets2.t', 'snippets3.t', etc in the upper directory

# See the README file for source file naming conventions

# TODO:
# Catch and check error output
# Should backup old snippets?.t and remove if successful
# Note that if batch size is increased we may leave some old snippets*.t
# unless we do this.

my $rsources = {};
my $rparams  = {};

my $defname = 'def';

my $get_string = sub {
    my ($file) = @_;
    open my $fh, '<', $file or die "cannot open $file: $!\n";
    local $/ = undef;
    my $string = <$fh>;
    close $fh;
    return $string;
};

my $get_source = sub {
    my ($sname) = @_;
    if ( !defined( $rsources->{$sname} ) ) {
        $rsources->{$sname} = $get_string->( $sname . ".in" );
    }
    return;
};

my $get_param = sub {
    my ($pname) = @_;
    if ( $pname && !defined( $rparams->{$pname} ) ) {
        my $fname = "$pname.par";
        if ( !-e $fname ) {
            if ( $pname eq $defname ) {
                $pname = $defname;
                $rparams->{$pname} = "";
                return;
            }
            die <<EOM;
Cannot locate parameter file $fname. You should either add it or
remove all expect files which depend on it.
EOM
        }
        my $pstring = $get_string->("$pname.par");

        #chomp $pstring;
        # not needed after change from argv to perltidyrc:
        #$pstring =~ s/\n/ /g;
        #$pstring =~ s/\s+/ /;
        #$pstring =~ s/\s*$//;
        $rparams->{$pname} = $pstring;
    }
    return;
};

# We can either get the packing list from the snippets, or by
# reading the packing list file.  To get it from the file,
# pass 'get_passing_list()' the file name.  Otherwise,
# it will be constructed from the snippets.  Both
# methods work. It can be necessary to switch between these
# methods if something goes wrong during development.
#my $rpacking_list=get_packing_list($fpacking_list);
my $rpacking_list = get_packing_list();

my @exp = glob("$ipath*");

#print "exp=(@exp)\n";
my $ix         = 0;
my $rix_lookup = {};
my %is_basename;
foreach my $file_exp ( sort @exp ) {
    my $estring = $get_string->($file_exp);
    my $ename   = $file_exp;
    if ( $ename =~ /([^\/]+)$/ ) { $ename = $1 }
    my ( $sname, $pname ) = split /\./, $ename;
    my $sroot = ( $sname =~ /^([^\d]+)/ ) ? $1 : $sname;
    if ( $sroot ne $sname && $pname ne 'def' ) { $is_basename{$pname}++ }

    $get_source->($sname);
    $get_param->($pname);
    push @{$rtests}, [ $ename, $pname, $sname, $estring ];
    $rix_lookup->{$ename} = $ix;
    $ix++;
}

# Find the base names.  NOTE: I tried packing by basename, which makes tracking
# down errors a little easier, and makes the files change less frequently, but
# the run times increased too much over the 'snippets*.t' packing method.  For
# example, here are times recorded in April 2020

#    packing in 20 files, snippets1.t ... snippets20.t: 17.7 s
#    packing in 226 files, 105484.t ...  wngnu1.t:      44.7 s

# so there is over a factor of 2 increase in run time for the convenience of
# packing by base name.  The extra time is due to continually reloading
# perltidy for tiny snippets of code. Since I run 'make test' frequently, I
# decided to stay with the snippets*.t packing.  I have left the base name
# coding in just in case I ever need the basenames.

my $rpacking_by_basename = {};
foreach my $item ( @{$rtests} ) {
    my ( $ename, $pname, $sname, $estring ) = @{$item};

    # The base name is either $sname or $sroot
    my $basename;
    my $proot = ( $pname =~ /^([^\d]+)/ ) ? $1 : $pname;
    my $sroot = ( $sname =~ /^([^\d]+)/ ) ? $1 : $sname;

    if    ( $sname eq $pname ) { $basename = $sname }
    elsif ( $sname eq $sroot ) { $basename = $sname }
    elsif ( $sroot eq $proot ) { $basename = $sroot }
    elsif ( $pname eq 'def' ) {
        if   ( $is_basename{$sroot} ) { $basename = $sroot }
        else                          { $basename = $sname }
    }
    else {
        # shouldn't happen
        $basename = "$sname-$pname";
        print STDERR
          "Unexpected filename $sname.$pname, using basename=$basename\n";
    }
    push @{$item},                                $basename;
    push @{ $rpacking_by_basename->{$basename} }, $item;
}

# assign indexes to existing packing locations
my $rassigned;
my $rcount;
my $high_file   = "";
my $high_digits = 0;
foreach my $item ( @{$rpacking_list} ) {
    my ( $ofile, $ename ) = @{$item};
    $rcount->{$ofile}++;
    my $ix = $rix_lookup->{$ename};
    push @{$item}, $ix;
    $rassigned->{$ix} = $ofile;

    # Find the last snippet file in the set
    if ( $ofile =~ /snippets(\d+)\.t/ ) {
        my $digits = $1;
        if ( $digits > $high_digits ) {
            $high_digits = $digits;
            $high_file   = $ofile;
        }
    }
}

# Pack all new items. Continue with last file in the list
my $ofile_last = $high_file;                   ##$rpacking_list->[-1]->[0];
my $case_count = $rcount->{$ofile_last} + 1;

my $file_count = $high_digits;

for ( my $ix = 0 ; $ix < @{$rtests} ; $ix++ ) {
    next if ( $rassigned->{$ix} );
    if ( $case_count >= $MAX_TESTS_PER_FILE ) { $case_count = 1; $file_count++ }
    my $ename = $rtests->[$ix]->[0];
    my $ofile = "../snippets" . $file_count . ".t";
    push @{$rpacking_list}, [ $ofile, $ename, $ix ];
    print "Added case $ename to $ofile\n";
    $case_count++;
}

# make the packing list for each file
my $rpacking_hash;
my @missing_cases;
foreach my $item ( @{$rpacking_list} ) {
    my ( $ofile, $ename, $ix ) = @{$item};
    if ( !defined($ix) ) { push @missing_cases, $ename; next }
    push @{ $rpacking_hash->{$ofile} }, $rtests->[$ix];
}

# Write the snippet files
my @empty_files;
foreach my $ofile ( sort keys %{$rpacking_hash} ) {
    my @tests = @{ $rpacking_hash->{$ofile} };
    my $num   = @tests;
    if ($num) {
        make_snippet_t( $ofile, \@tests, $rparams, $rsources );
        print "writing $num tests to $ofile\n";
    }
    else {

        # a file no longer exists, we should delete or move it
        push @empty_files, $ofile;
        system "mv $ofile $ofile.bak";
    }
}

if (@missing_cases) {
    local $" = '> <';
    print <<EOM;
Note that these old cases are missing:
<@missing_cases>
EOM
}

if (@empty_files) {
    local $" = '> <';
    print <<EOM;
NOTE: These old files did nnot have any cases, so I moved them to .bak
<@empty_files>
EOM
}

write_packing_list( "$fpacking_list", $rpacking_list );
print "Now run a 'make test' from the top directory to check these\n";

# Example showing how to pack the snippet files using base names
# See above note on the problem this causes with run times
if (0) {
    foreach my $basename ( sort keys %{$rpacking_by_basename} ) {
        my @tests = @{ $rpacking_by_basename->{$basename} };
        my $ofile = "/tmp/t/$basename.t";    ## FIXME: Just for testing
        my $num   = @tests;
        if ($num) {
            make_snippet_t( $ofile, \@tests, $rparams, $rsources );
            print "writing $num tests to $ofile\n";
        }
        else {
            print STDERR "Strange; no files for $basename\n";
        }
    }
}

sub write_packing_list {
    my ( $ofile, $rpacking ) = @_;
    if ( -e $ofile ) { system "mv $ofile $ofile.bak" }
    open my $fh, '>', $ofile or die "cannot open $ofile: $!\n";
    $fh->print("# This file is automatically generated by make_t.pl\n");
    foreach my $item ( @{$rpacking} ) {
        my ( $ofile, $ename ) = @{$item};
        $fh->print("$ofile\t$ename\n");
    }
    $fh->close();
    print "wrote new packing list to $fpacking_list\n";
}

sub get_packing_list {
    my ($ifile) = @_;
    my $rlist;
    if ( defined($ifile) && -e $ifile ) {
        $rlist = read_packing_list($ifile);
    }
    else {
        $rlist = construct_packing_list();
    }
    return $rlist;
}

sub read_packing_list {

    my ($ifile) = @_;
    my $rlist;
    open my $fh, '<', $ifile or die "cannot open $ifile: $!\n";
    foreach my $line (<$fh>) {
        $line =~ s/^\s+//;
        $line =~ s/\s+$//;
        next unless ($line);
        next if ( $line =~ /^#/ );
        my ( $ofile, $ename ) = split /\t/, $line;
        push @{$rlist}, [ $ofile, $ename ];
    }
    $fh->close();
    return $rlist;
}

sub construct_packing_list {

    # construct the packing list directly from the snippet files
    # this should be more reliable
    my @files = glob("../snippets*.t");
    my $rlist;
    foreach my $ifile (@files) {
        open my $fh, '<', $ifile or die "cannot open $ifile: $!\n";
        my $saw_contents;
        foreach my $line (<$fh>) {
            if ( !$saw_contents ) {
                if ( $line =~ /# Contents/ ) { $saw_contents = 1; next }
            }
            else {
                if ( $line =~ /#\d+\s+(.*)\s*$/ ) {
                    my $ename = $1;
                    push @{$rlist}, [ $ifile, $ename ];
                }
                else { last }
            }
        }
    }
    return $rlist;
}

sub make_snippet_t {
    my ( $ofile, $rtests, $rparams_all, $rsources_all ) = @_;
    my $ename_string = "# Contents:\n";

    # pull out the parameters and sources we need
    my $rparams  = {};
    my $rsources = {};
    my $nn       = 0;
    foreach my $item ( @{$rtests} ) {
        my ( $ename, $pname, $sname, $estring ) = @{$item};
        $rparams->{$pname}  = $rparams_all->{$pname};
        $rsources->{$sname} = $rsources_all->{$sname};
        $nn++;
        $ename_string .= "#$nn $ename\n";
    }

    my $count        = 0;
    my $audit_string = audit_string('#');

    my $script = <<EOM;
$audit_string

$ename_string

# To locate test #13 you can search for its name or the string '#13'

EOM
    $script .= <<'EOM';
use strict;
use Test::More;
use Carp;
use Perl::Tidy;
my $rparams;
my $rsources;
my $rtests;

BEGIN {

    ###########################################
    # BEGIN SECTION 1: Parameter combinations #
    ###########################################
    $rparams = {
EOM

    foreach my $key ( sort keys %{$rparams} ) {
        my $pstring = $rparams->{$key};
        chomp $pstring;
        if ( $pstring !~ /[\n\"\']/ ) {

            # single line, no quotes can go out as a single line
            $script .= <<XYZ;
'$key' => \"$pstring\",
XYZ
        }
        else {

            # everything else goes out in a here doc
            # note that we add back the chompped \n here
            my $XXX = "----------";
            $script .= "    '$key' => <<\'$XXX\',\n" . "$pstring\n" . "$XXX\n";
        }
    }

    $script .= <<'++++++++++';
};

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {
++++++++++

    foreach my $key ( sort keys %{$rsources} ) {
        my $sstring = $rsources->{$key};

        # Note that $sstring might be an empty string
        my $XXX = "----------";
        $script .= "\n" . "'$key' => <<\'$XXX\',\n" . "$sstring" . "$XXX\n";
    }

=pod
foreach my $key ( sort keys %{$rsources} ) {
    my $sstring = $rsources->{$key};
    chomp $sstring;
    $script .= <<XYZ;

'$key' => <<\'----------\',
$sstring
----------
XYZ

}
=cut

    $script .= <<'TMP';
};

    ####################################
    # BEGIN SECTION 3: Expected output #
    ####################################
    $rtests = {
TMP

    foreach my $item ( @{$rtests} ) {
        my $output;
        my ( $ename, $pname, $sname, $estring ) = @{$item};

        #chomp $estring;
        $count++;
        print "added case $ename\n";
        $script .= <<ENDCASE;

'$ename' => {
    source => \"$sname\",
    params => \"$pname\",
ENDCASE

        # Note that $estring might be an empty string
        my $XXX = "#$count...........";
        $script .= "    expect => <<\'$XXX\',\n" . "$estring" . "$XXX\n},\n";
    }

    $script .= <<'EOM';
};

    my $ntests=0+keys %{$rtests};
    plan tests => $ntests;
}

###############
# EXECUTE TESTS
###############

foreach my $key ( sort keys %{$rtests} ) {
    my $output;
    my $sname = $rtests->{$key}->{source};
    my $expect = $rtests->{$key}->{expect};
    my $pname   = $rtests->{$key}->{params};
    my $source = $rsources->{$sname};
    my $params = defined ($pname) ? $rparams->{$pname}: "";
    my $stderr_string;
    my $errorfile_string;
    my $err = Perl::Tidy::perltidy(
        source      => \$source,
        destination => \$output,
        perltidyrc  => \$params,
        argv        => '',         # for safety; hide any ARGV from perltidy
        stderr      => \$stderr_string,
        errorfile   => \$errorfile_string,    # not used when -se flag is set
    );
    if ( $err || $stderr_string || $errorfile_string ) {
        print STDERR "Error output received for test '$key'\n";
        if ($err) {
            print STDERR "An error flag '$err' was returned\n";
            ok(!$err);
        }
        if ($stderr_string) {
            print STDERR "---------------------\n";
            print STDERR "<<STDERR>>\n$stderr_string\n";
            print STDERR "---------------------\n";
            ok(!$stderr_string);
        }
        if ($errorfile_string) {
            print STDERR "---------------------\n";
            print STDERR "<<.ERR file>>\n$errorfile_string\n";
            print STDERR "---------------------\n";
            ok(!$errorfile_string);
        }
    }
    else {
        if ( !is( $output, $expect, $key ) ) {
            my $leno = length($output);
            my $lene = length($expect);
            if ( $leno == $lene ) {
                print STDERR
"#> Test '$key' gave unexpected output.  Strings differ but both have length $leno\n";
            }
            else {
                print STDERR
"#> Test '$key' gave unexpected output.  String lengths differ: output=$leno, expected=$lene\n";
            }
        }
    }
}
EOM

    # Tidy the script with default parameters
    my $output;
    my $stderr_string;
    my $errorfile_string;
    my $err = Perl::Tidy::perltidy(
        source      => \$script,
        destination => \$output,
        argv        => '',                    # hide any ARGV from perltidy
        stderr      => \$stderr_string,
        errorfile   => \$errorfile_string,    # not used when -se flag is set
    );
    if ($err) {
        die "This error received calling Perl::Tidy with script '$ofile'\n";
    }
    if ($stderr_string) {
        print STDERR "---------------------\n";
        print STDERR "<<STDERR>>\n$stderr_string\n";
        print STDERR "---------------------\n";
        die "This STDERR received calling Perl::Tidy with script '$ofile'\n";
    }
    if ($errorfile_string) {
        print STDERR "---------------------\n";
        print STDERR "<<.ERR file>>\n$errorfile_string\n";
        print STDERR "---------------------\n";
        die "This .ERR received calling Perl::Tidy with script '$ofile'\n";
    }

    # and write it out
    #my $ofile = "../snippets.t";
    open my $fh, '>', $ofile or die "cannot open $ofile: $!\n";
    $fh->print($output);
    $fh->close();
    print "Wrote $count test cases to $ofile\n";

}

sub audit_string {
    my ( $ch, $noargs ) = @_;
    my $audit_string;
    my $raudit_lines = audit_lines( $ch, $noargs );
    $audit_string = join( '', @{$raudit_lines} );
}

sub audit_lines {

    # ch is comment character ('*', '#', ..)
    my ( $ch, $noargs ) = @_;
    $ch = "" unless defined($ch);
    my $date = localtime();

    #my $host = `hostname`;
    #chomp $host;
    my $host = "";
    my $args = "";
    $args = join( " ", @ARGV ) unless ($noargs);
    my @audit_trail;
    my $string = "$ch Created with: $0 $args";

    # Truncate to a reasonbale length because when unix wildcards
    # the number of args can be huge when expanded
    $string = truncate_string( $string, 72 );

    #push @audit_trail, "$ch Created with: $0 $args\n";
    push @audit_trail, "$string\n";
    ##push @audit_trail, "$ch $date   $host\n";
    return \@audit_trail;
}

sub truncate_string {

    # Make a short and long version of a given string
    my ( $string, $short_length ) = @_;

    my $short = $string;
    my $long  = $string;

    if ( length($string) > $short_length ) {
        $long = $string;
        my @words = split( /[\s\-\_\(\)\,\&\+]/, $string );
        my $num   = @words;
        $short = shift(@words);
        for ( my $i = 0 ; $i < $num ; $i++ ) {
            my $word   = shift(@words);
            my $newstr = $short . " " . $word;
            last if ( length($newstr) > $short_length );
            $short = $newstr;
        }

        # use the first part of the actual string because we have
        # turned all commas, etc into spaces for testing lengths
        $short = substr( $long, 0, length($short) ) . "...";
    }
    return ($short);
}

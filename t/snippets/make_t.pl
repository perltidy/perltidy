#!/usr/bin/perl -w
use strict;
use warnings;
use Perl::Tidy;
my $rtests;

my $ipath = 'expect/';

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

my @exp = glob("$ipath*");

#print "exp=(@exp)\n";
foreach my $file_exp (@exp) {
    my $estring = $get_string->($file_exp);
    my $ename   = $file_exp;
    if ( $ename =~ /([^\/]+)$/ ) { $ename = $1 }
    my ( $sname, $pname ) = split /\./, $ename;

   #print "BUBBA: file=$file_exp, ename = $ename, sname=$sname, pname=$pname\n";
    $get_source->($sname);
    $get_param->($pname);
    push @{$rtests}, [ $ename, $pname, $sname, $estring ];
}

my $file_count = 0;
my $nend       = -1;
my $nstop      = @{$rtests} - 1;
while ( $nend < $nstop ) {
    $file_count++;
    my $nbeg = $nend + 1;
    $nend += $MAX_TESTS_PER_FILE;
    if ( $nend > $nstop ) { $nend = $nstop }
    my @tests;
    foreach my $n ( $nbeg .. $nend ) { push @tests, $rtests->[$n]; }
    my $ofile = "../snippets" . $file_count . ".t";
    make_snippet_t( $ofile, \@tests, $rparams, $rsources );
    print "Now run a 'make test' from the top directory to check these\n";
}

sub make_snippet_t {
    my ( $ofile, $rtests, $rparams_all, $rsources_all ) = @_;

    # pull out the parameters and sources we need
    my $rparams  = {};
    my $rsources = {};
    foreach my $item ( @{$rtests} ) {
        my ( $ename, $pname, $sname, $estring ) = @{$item};
        $rparams->{$pname}  = $rparams_all->{$pname};
        $rsources->{$sname} = $rsources_all->{$sname};
    }

    my $count        = 0;
    my $audit_string = audit_string('#');

    my $script = <<EOM;
# **This script was automatically generated**
$audit_string

# To locate test #13 for example, search for the string '#13'

EOM
    $script .= <<'EOM';
use strict;
use Test;
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
        if ($err) {
            print STDERR
              "This error received calling Perl::Tidy with '$sname' + '$pname'\n";
            ok(!$err);
        }
        if ($stderr_string) {
            print STDERR "---------------------\n";
            print STDERR "<<STDERR>>\n$stderr_string\n";
            print STDERR "---------------------\n";
            print STDERR
              "This error received calling Perl::Tidy with '$sname' + '$pname'\n";
            ok(!$stderr_string);
        }
        if ($errorfile_string) {
            print STDERR "---------------------\n";
            print STDERR "<<.ERR file>>\n$errorfile_string\n";
            print STDERR "---------------------\n";
            print STDERR
              "This error received calling Perl::Tidy with '$sname' + '$pname'\n";
            ok(!$errorfile_string);
        }
    }
    else {
        ok( $output, $expect );
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
        argv        => '', # hide any ARGV from perltidy
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
    push @audit_trail, "$ch $date   $host\n";
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
        my $num = @words;
        $short = shift(@words);
        for ( my $i = 0 ; $i < $num ; $i++ ) {
            my $word   = shift(@words);
            my $newstr = $short . " " . $word;
            last if ( length($newstr) > $short_length );
            $short = $newstr;
        }

        # use the first part of the actual string because we have
        # turned all commas, etc into spaces for testing lenghts
        $short = substr( $long, 0, length($short) ) . "...";
    }
    return ($short);
}

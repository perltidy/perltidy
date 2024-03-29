#!/usr/bin/perl -w
use strict;
use warnings;
use Perl::Tidy;

# Run this to make the expected output of the test snippets.
# Output will be written in ./tmp and can be moved to ./expect if ok

# TODO:
# - Add ability to run with previous version of perltidy and show differences
#   (but not install)

# generate ./tmp if does not exist
my $opath = "./tmp/";
if ( !-d $opath ) { mkdir $opath; print "Making $opath\n" }

# usage:
#    make_expect.pl
#
# All of the .in source files will be run against the default parameters
# plus all parameter files with the same root name.  See the README file.

my $rsources = {};
my $rparams  = {};

my $Xget_parameters = sub {
    my ($file) = @_;
    open my $fh, '<', $file or die "cannot open $file: $!\n";

    #local $/ = undef;
    my @lines = <$fh>;

    #my $string = <$fh>;
    close $fh;
    my @non_comments = map { $_ !~ /^\s*#/ } @lines;
    my $string;
    foreach (@non_comments) { chomp $_; $string .= $_ }
    return $string;
};

my $read_parameters = sub {

    # FIXME: generalize to handle side comments and almost everything
    # in a .perltidyrc file
    my ($file) = @_;
    open my $fh, '<', $file or die "cannot open $file: $!\n";
    my @lines = <$fh>;
    close $fh;
    my @non_comments = grep { $_ !~ /^\s*#/ } @lines;
    my $string;
    foreach (@non_comments) { chomp $_; $string .= "$_ " }
    return $string;
};

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
};

my $get_param = sub {
    my ($pname) = @_;
    if ( $pname && !defined( $rparams->{$pname} ) ) {
        my $pstring = $get_string->( $pname . ".par" );
        chomp $pstring;

        #        my $pstring = $read_parameters->( $pname . ".par" );
        #        if ($pstring) {
        #            $pstring =~ s/\n/ /g;
        #            $pstring =~ s/\s+/ /;
        #            $pstring =~ s/\s*$//;
        #        }
        $rparams->{$pname} = $pstring;
    }
};

# Be sure we have a parameter set with a special name
my $defname = 'def';
if ( !defined( $rparams->{$defname} ) ) {
    $rparams->{$defname} = "";
}

# To speed up testing, you may enter specific files
# if none are given all are used
my @files = @ARGV;
if ( !@files ) {
    @files = glob('*.in *.par');
}

foreach my $file (@files) {
    if ( $file =~ /^(.*)\.in$/ ) {
        my $sname = $1;
        $get_source->($sname);
    }
    elsif ( $file =~ /^(.*).par$/ ) {
        my $pname = $1;
        $get_param->($pname);
    }
    else {
        die "File $file must be xxx.in or param.xxx\n";
    }
}

my @olist;
my @obasenames;
foreach my $sname ( keys %{$rsources} ) {
    my $sroot = ( $sname =~ /^([^\d]+)/ ) ? $1 : $sname;
    my @pnames;
    @pnames = keys %{$rparams};
    foreach my $pname (@pnames) {
        my $proot = ( $pname =~ /^([^\d]+)/ ) ? $1 : $pname;
        my $match =

          # exact match of source and parameter file base names
          $pname eq $sname

          # match of source root to parameter file base name
          || $pname eq $sroot

          # match of source base name to parameter root
          || $proot eq $sname

          # defaults apply to all files
          || $pname eq $defname;

        next unless ($match);

        my $output;
        my $source = $rsources->{$sname};
        my $params = $pname ? $rparams->{$pname} : "";
        my $stderr_string;
        my $errorfile_string;
        my $err = Perl::Tidy::perltidy(
            source      => \$source,
            destination => \$output,
            perltidyrc  => \$params,
            argv        => '',             # don't let perltidy look at my @ARGV
            stderr      => \$stderr_string,
            errorfile   => \$errorfile_string,   # not used when -se flag is set
        );
        if ($stderr_string) {
            print STDERR "---------------------\n";
            print STDERR "<<STDERR>>\n$stderr_string\n";
            print STDERR "---------------------\n";
            die "The above error was received with $source + $params\n";
        }
        if ($errorfile_string) {
            print STDERR "---------------------\n";
            print STDERR "<<.ERR file>>\n$errorfile_string\n";
            print STDERR "---------------------\n";
            die "The above .ERR was received with $source + $params\n";
        }
        if ($err) {
            die "error calling Perl::Tidy with $source + $params\n";
        }
        my $basename = "$sname.$pname";
        my $ofile    = $opath . $basename;

        open my $fh, '>', $ofile or die "cannot open $ofile: $!\n";
        $fh->print($output);
        $fh->close();

        #print "Wrote '$ofile'\n";
        push @olist, $basename;
    }
}

my @new;
my @changed;
my @same;
my $epath = "expect/";
my @mv;
use File::Compare;
foreach my $basename (@olist) {
    my $tname = $opath . $basename;
    my $ename = $epath . $basename;
    if ( !-e $ename ) {
        my $new_file = "tmp/$basename";
        push @new, $new_file;
        print "$new_file is a new file\n";
        push @mv, "cp $tname $ename";
    }
    elsif ( compare( $ename, $tname ) ) {
        push @changed, $basename;
        push @mv,      "cp $tname $ename";
    }
    else {
        push @same, $basename;
    }
}

my $diff_file = "diff.txt";
if ( -e "$diff_file" ) { unlink("$diff_file") }
if (@same) {
    my $num = @same;
    print "$num Unchanged files\n";
}
if (@new) {
    my $num = @new;
    print "$num New files:\n";
    foreach my $file (@new) { print " $file\n" }
}
if (@changed) {
    my $num = @changed;
    print "$num Changed files:\n";
    foreach my $basename (@changed) {
        system(
"cd tmp; echo $basename >>../diff.txt; diff $basename ../expect/$basename >>../$diff_file"
        );
    }
    print "---differences---\n";
    system("cat $diff_file");
    print "------\n";
}

my $runme = "RUNME.sh";

if ( !@mv ) {
    print "No differences\n";
    if ( -e $runme ) { unlink $runme }
    exit;
}

if ( open( RUN, ">$runme" ) ) {
    print RUN <<EOM;
#!/bin/sh
EOM
    foreach my $cmd (@mv) {
        print RUN <<EOM;
$cmd
EOM
    }

    print RUN <<EOM;
./make_t.pl
unlink \$0;
EOM

    close RUN;
    system("chmod 0755 $runme");

    if (@new) {
        if (
            ifyes(
"You need to review the new tidied files. Do you want to look at them now? [Y/N]"
            )
          )
        {
            my $str = join " ", @new;
            system("vim -R $str");
        }
    }

    if ( -e $diff_file ) {
        if (
            ifyes(
"There are differences between the old and new tidied results.\nDo you want to look at them now? [Y/N]"
            )
          )
        {
            system("vim -R $diff_file");
        }
    }

    my $diff_msg = "no differences";
    if ( -e $diff_file ) {
        $diff_msg = <<EOM;
Look at each differences in '$diff_file'
The full old output for 'somefile.def' is in expect/somefile.def
The full new output for 'somefile.def' is in tmp/somefile.def
EOM

    }

    print <<EOM;
$diff_msg
If the differences and any new results look okay, then
Enter ./$runme to move results from tmp/ to expect/ and make new .t files
EOM
}

sub query {
    my ($msg) = @_;
    print $msg;
    my $ans = <STDIN>;
    chomp $ans;

    #my $val=$ans;
    return $ans;
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


#!/usr/bin/perl -w
use strict;
use warnings;

# Do not use too source files
my $MAX_SOURCES = 10;

my $usage = <<EOM;
Run perltidy repeatedly on a selected file with randomly generated parameters:

    perltidy_random_parameters file1 [file2 [ ... Num

file1 ... are the names of a text to be formatted
Num is the number of times, default 1000;

You can stop the run any time by creating a file "stop.now"
EOM

# Random testing of Perl::Tidy
# Given:
#  - a list of files
#  - a list of options

# Write out a logfile of results:
# name, exit flag, isize, osize
# gzip name.in, name.in.tdy, name.par, name.ERR

# Also save nohup.out, and echo each file as you go
# Looking for perl complaints

# Structure:
#  logfile.txt
#  files/  - has text input files for random selection
#  tmp/    - has zipped output

# Continually generate files with random parameters and some randomly removed
# lines and run perltidy.

my @source_files = @ARGV;

my $max_cases = pop @source_files;
if ( $max_cases !~ /^\d+$/ ) {
    push @source_files, $max_cases;
    $max_cases = 100;
}

# only work on regular source_files with non-zero length
@source_files=grep {-f $_ && !-z $_} @source_files;

if ( !@source_files ) { die "$usage" }

my $rsource_files = [];
my $i = -1;
foreach my $file (@source_files) {
    $i++;
    last if ( $i >= $MAX_SOURCES );
    open( IN, '<', $file ) || die "cannot open $file: $!\n";
    $rsource_files->[$i] = [];
    foreach my $line (<IN>) {
        push @{ $rsource_files->[$i] }, $line;
    }
    close(IN);
}

my $basename = "ranfile";
my $nsources = @{$rsource_files};

for ( my $nf = 1 ; $nf <= $max_cases ; $nf++ ) {
    my $fname = "$basename.$nf";
    my $frac  = rand(1);
    my $ix    = int( rand($nsources) );
    $ix = random_index( $nsources - 1 );
    my $NMETH = 4;
    my $method = random_index(3);
    my $rfile;
    if ( $method == 3 ) {
        my $nchars=1+random_index(1000);
        $rfile = random_characters($nchars);
print STDERR "Method $method, nchars=$nchars\n";
    }
    elsif ( $method == 2 ) {
        $rfile = skip_random_lines( $rsource_files->[$ix], $frac );
print STDERR "Method $method, frac=$frac, file=$ix\n";
    }
    elsif ( $method == 1 ) {
        $rfile = select_random_lines( $rsource_files->[$ix], $frac );
print STDERR "Method $method, frac=$frac, file=$ix\n";
    }
    elsif ( $method == 0 ) {
        $rfile = reverse_random_lines( $rsource_files->[$ix], $frac );
print STDERR "Method $method, frac=$frac, file=$ix\n";
    }

    # Shouldn't happen
    else {
        my $nchars=1+random_index(1000);
        $rfile = random_characters($nchars);
print STDERR "FIXME: method=$method but NMETH=$NMETH; Method $method, nchars=$nchars\n";
    }
    open( OUT, ">", $fname ) || die "cannot open $fname: $!\n";
    foreach my $line ( @{$rfile} ) {
        print OUT $line;
    }
    close OUT;
}

sub random_index {
    my ($ix_max) = @_;
    $ix_max = 0 if ( $ix_max < 0 );
    my $ix_min = 0;
    my $ix     = int( rand($ix_max) + 0.5 );
    $ix = $ix_max if ( $ix > $ix_max );
    $ix = $ix_min if ( $ix < $ix_min );
    return $ix;
}

sub random_characters {

    my ($nchars) = @_;
    my @qc       = qw# { [ ( } ] ) , ; $x for #;
    my $nqc      = @qc;
    my @lines;
    my $ncpl = 0;
    my $line = "";
    for ( my $ich = 0 ; $ich < $nchars ; $ich++ ) {
        my $ix = random_index( $nqc - 1 );
        my $ch = $qc[$ix];
        $line .= " $ch ";
        $ncpl++;
        if ( $ncpl > 20 ) {
            $line .= "\n";
            push @lines, $line;
            $ncpl = 0;
        }
    }
    $line .= "\n";
    push @lines, $line;
    return \@lines;
}

sub select_random_lines { 

    # select some fraction of the lines in a source file
    # in any order
    my ($rsource, $fkeep) = @_;

    my $nlines = @{$rsource};
    my $num_keep = $nlines*$fkeep;
    my $count=0;
    my @selected;
    while ($count < $num_keep) {
       my $ii = random_index($nlines-1);
       push @selected, $rsource->[$ii];
       $count++;
    }
    return \@selected;
}

sub reverse_random_lines { 

    # skip some fraction of the lines in a source file
    # but keep lines in the original order
    my ($rsource, $frand) = @_;

    my %select;
    my $nlines = @{$rsource};
    my $num_delete = $nlines*$frand;
    my $count=0;
    while ($count < $num_delete) {
       my $ii = rand($nlines);
       $ii = int($ii);
       $select{$ii} = 1;
       $count++;
    }
   
    my @lines;
    my $jj = -1;
    foreach my $line (@{$rsource}) {
        $jj++;
        if ($select{$jj} ) {
             chomp $line;
             $line = reverse($line);
             $line .= "\n";
        };
        push @lines, $line;
    }
    return \@lines;
}

sub skip_random_lines { 

    # skip some fraction of the lines in a source file
    # but keep lines in the original order
    my ($rsource, $fskip) = @_;

    my %skip;
    my $nlines = @{$rsource};
    my $num_delete = $nlines*$fskip;
    my $count=0;
    while ($count < $num_delete) {
       my $ii = rand($nlines);
       $ii = int($ii);
       $skip{$ii} = 1;
       $count++;
    }
   
    my @selected;
    my $jj = -1;
    foreach my $line (@{$rsource}) {
        $jj++;
        next if $skip{$jj};
        push @selected, $line;
    }
    return \@selected;
}

__END__

my $ii = rand(@lines);
$ii = int($ii);
my %skip;
$skip{$ii} = 1;
print STDERR "Skipping line $ii\n";

my @select;
my $jj = -1;
foreach my $line (@lines) {
    $jj++;
    next if $skip{$jj};
    push @selected, $line;
}
foreach my $line (@selected) {
    print STDOUT $line;
}

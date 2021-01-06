#!/usr/bin/perl -w
use strict;
use warnings;

# This is one of a set of programs for doing random testing of perltidy.  The
# goal is to try to crash perltidy.  The programs are:

#   random_file_generator.pl  [this file]
#   perltidy_random_setup.pl  [next step]
#   perltidy_random_run.pl    [final step]

# This program creates some random files for testing perltidy. You must supply
# as input args the names of a number of actual source files for it to read and
# manipulate into random files.

# Do not use too many source files
my $MAX_SOURCES = 10;

my $usage = <<EOM;
Create a set of random files for testing perltidy

    random_file_generator file1 [file2 [ ... Num

    file1 file2 ... are perl scripts (or other text files)
    Num is the number of random files to generate [default 100]

Example: generate 100 random files from the scripts in the upper directory:

    random_file_generator.pl ../*.pl 100

EOM

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
    my @qset1       = qw# { [ ( } ] ) , ; #;
    my @qset2 = (
        qw{a b c f g m q r s t w x y z V W X 0 1 8 9},
        ';',  '[', ']', '{', '}',  '(',  ')', '=', '?', '|', '+', '<',
        '>',  '.', '!', '~', '^',  '*',  '$', '@', '&', ':', '%', ',',
        '\\', '/', '_', ' ', "\n", "\t", '-',
        "'",  '"', '`', '#',
    );
    my @qset3 = (
        '!%:',               '!%:',
        '!%:',               '!%:',
        '!*:',               '!@:',
        '%:',                '%:,',
        '%:;',               '*:',
        '*:,',               '*::',
        '*:;',               '+%:',
        '+*:',               '+@:',
        '-%:',               '-*:',
        '-@:',               ';%:',
        ';*:',               ';@:',
        '@:',                '@:,',
        '@::',               '@:;',
        '\%:',               '\&:',
        '\*:',               '\@:',
        '~%:',               '~*:',
        '~@:',               '(<',
        '(<',                '=<',
        'm(',                'm(',
        'm<',                'm[',
        'm{',                'q(',
        'q<',                'q[',
        'q{',                's(',
        's<',                's[',
        's{',                'y(',
        'y<',                'y[',
        'y{',                '$\'0',
        '009',               '0bB',
        '0xX',               '009;',
        '0bB;',              '0xX;',
        "<<'",               '<<"',
        '<<`',               '&::',
        '<<a',               '<<V',
        '<<s',               '<<y',
        '<<_',               'm;;_',
        'm[]_',              'm]]_',
        'm{}_',              'm}}_',
        'm--_',              's[]a',
        's[]b',              's[]0',
        's[];',              's[]]',
        's[]=',              's[].',
        's[]_',              's{}]',
        's{}?',              's<>s',
        's<>-',              '*::0',
        '*::1',              '*:::',
        '*::\'',             '$::0',
        '$:::',              '$::\'',
        '@::0',              '@::1',
        '@:::',              '&::0',
        '&::\'',             '%:::',
        '%::\'',             '$:::z',
        '*:::z',             "\\\@::'9:!",
        "} mz}~<<ts",        "<\@<<q-r8\n/",
        "W<<s`[\n(",         "X<<f+X;g(<~\" \n1\n*",
        "c<<t* 9\ns\n~^{s ", "<<V=-<<Wt",
        "[<<g/.<<r>\nV",     "( {8",
    );
    my @lines;
    my $ncpl = 0;
    my $line = "";
    for ( my $ich = 0 ; $ich < $nchars ; $ich++ ) {
        my $nset = random_index(2);
        my $ch;
        if ($nset==0) {
            my $ix = random_index( @qset1 - 1 );
            $ch = $qset1[$ix];
        }
        elsif ($nset==1) {
            my $ix = random_index( @qset2 - 1 );
            $ch = $qset2[$ix];
        }
        elsif ($nset==2) {
            my $ix = random_index( @qset3 - 1 );
            $ch = $qset3[$ix];
        }
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
    # and reverse the characters on a line
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
